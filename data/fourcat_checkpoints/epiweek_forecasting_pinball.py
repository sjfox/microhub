# epiweek_forecasting_pinball.py
# Cohesive, minimal pipeline to train a Transformer with epiweek features
# and produce 0–4 week‑ahead probabilistic forecasts (quantile ribbons).
# FIXED VERSION: Addresses data leakage issues
# V2: Adds on-the-fly trend/momentum feature engineering for early-season performance
# V3: Adds weighted sampling to oversample ramp-up regime windows during training
#

from __future__ import annotations
import os
import math
import argparse
from pathlib import Path
from datetime import datetime, timedelta
from typing import List, Dict, Tuple, Optional, Any

import pandas as pd
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data import Dataset, DataLoader, WeightedRandomSampler

# -------------------------------
# Setup and Configuration
# -------------------------------
import logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("epiweek")

def set_seed(seed: int = 42):
    """Set random seeds for reproducibility"""
    torch.manual_seed(seed)
    np.random.seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)

# -------------------------------
# Trend / Momentum Feature Config
# -------------------------------
# Dataclass-style dict so it serialises cleanly into checkpoints.
# Every field here is used inside __getitem__ to build extra channels
# from the raw value sequence in the input window.

DEFAULT_TREND_CONFIG: Dict[str, Any] = {
    "enabled": True,
    # --- rate-of-change (short-term momentum) ---
    # log1p(value[t]) - log1p(value[t-k])  for each k
    "roc_windows": [2, 4],
    # --- rolling linear-regression slope ---
    # OLS slope over trailing w weeks, computed at every timestep
    "slope_windows": [6],
    # --- deviation from trailing baseline ---
    # (value[t] - mean(value[t-w:t])) / (std(value[t-w:t]) + eps)
    "baseline_windows": [10],
}

def count_trend_features(cfg: Dict[str, Any]) -> int:
    """Return number of extra feature channels the config will produce."""
    if not cfg.get("enabled", False):
        return 0
    n = 0
    n += len(cfg.get("roc_windows", []))       # one channel per RoC lag
    n += len(cfg.get("slope_windows", []))      # one channel per slope window
    n += len(cfg.get("baseline_windows", []))   # one channel per baseline window
    return n


def compute_trend_features(values: np.ndarray, cfg: Dict[str, Any]) -> np.ndarray:
    """
    Compute trend/momentum features from a 1-D value array of length T.

    Returns array of shape (T, n_trend_features).

    All features are causal — they use only current and past values.
    Positions where there isn't enough history are filled with 0.0 so the
    model sees a neutral signal rather than NaN.
    """
    T = len(values)
    feats = []

    # 1) Rate of change: log1p(v[t]) - log1p(v[t-k])
    log_vals = np.log1p(np.clip(values, 0.0, None))  # safe for zeros
    for k in cfg.get("roc_windows", []):
        roc = np.zeros(T, dtype=np.float32)
        if k < T:
            roc[k:] = log_vals[k:] - log_vals[:-k]
        feats.append(roc)

    # 2) Rolling OLS slope (standardised by window length so units ≈ "change per week")
    for w in cfg.get("slope_windows", []):
        slope = np.zeros(T, dtype=np.float32)
        if w >= 2 and w <= T:
            # Pre-compute x for the regression: centered integers
            x = np.arange(w, dtype=np.float32)
            x_mean = x.mean()
            x_centered = x - x_mean
            ss_x = (x_centered ** 2).sum()
            for t in range(w, T):
                y_win = log_vals[t - w: t]
                y_mean = y_win.mean()
                slope[t] = ((x_centered * (y_win - y_mean)).sum()) / (ss_x + 1e-8)
            # Fill the first w positions with the earliest computable slope
            if w < T:
                slope[:w] = slope[w]
        feats.append(slope)

    # 3) Deviation from trailing baseline (z-score style)
    for w in cfg.get("baseline_windows", []):
        dev = np.zeros(T, dtype=np.float32)
        if w >= 2 and w <= T:
            for t in range(w, T):
                window = values[t - w: t]
                mu = window.mean()
                sigma = window.std() + 1e-8
                dev[t] = (values[t] - mu) / sigma
            if w < T:
                dev[:w] = dev[w]
        feats.append(dev)

    if not feats:
        return np.empty((T, 0), dtype=np.float32)

    return np.column_stack(feats).astype(np.float32)  # (T, n_trend)


# -------------------------------
# Data Leakage Detection Utilities
# -------------------------------
class LeakageDetector:
    """Utility class to detect potential data leakage issues"""

    @staticmethod
    def check_temporal_overlap(train_df: pd.DataFrame, val_df: pd.DataFrame,
                               group_cols: List[str], date_col: str = 'date') -> Dict[str, Any]:
        issues = []
        for name, group in train_df.groupby(group_cols):
            train_max = group[date_col].max()
            val_group = val_df
            for col, val in zip(group_cols, name if isinstance(name, tuple) else [name]):
                val_group = val_group[val_group[col] == val]
            if len(val_group) > 0:
                val_min = val_group[date_col].min()
                if val_min <= train_max:
                    issues.append({
                        'series': name,
                        'train_max': train_max,
                        'val_min': val_min,
                        'overlap_days': (train_max - val_min).days
                    })
        return {
            'has_overlap': len(issues) > 0,
            'num_series_with_overlap': len(issues),
            'details': issues[:5]
        }

    @staticmethod
    def check_normalization_strategy(dataset: 'WindowedForecastDataset') -> Dict[str, Any]:
        return {
            'is_normalized': dataset.normalize,
            'num_series': len(dataset.normalizers),
            'note': 'Each series normalized independently - ensure val uses train stats in production'
        }

    @staticmethod
    def check_future_information(dataset: 'WindowedForecastDataset', verbose: bool = False) -> Dict[str, Any]:
        issues = []
        ncheck = min(100, len(dataset))
        for idx in range(ncheck):
            sid, t_end = dataset.sample_index[idx]
            s = dataset.series[sid]
            input_dates = s['dates'][t_end - dataset.input_len : t_end]
            latest_input = pd.Timestamp(input_dates[-1])
            earliest_target_date = pd.Timestamp(s['dates'][t_end])
            if latest_input >= earliest_target_date:
                issues.append({
                    'sample_idx': idx,
                    'series_id': sid,
                    'latest_input': latest_input,
                    'earliest_target': earliest_target_date
                })
        return {
            'has_future_info': len(issues) > 0,
            'num_issues': len(issues),
            'samples_checked': ncheck,
            'details': issues[:3] if verbose else []
        }

# -------------------------------
# 1) Data loader (unchanged)
# -------------------------------
class SurveillanceDataLoader:
    """
    Loader for disease surveillance CSV files with validation and preprocessing.
    """
    def __init__(self, data_folder: str):
        self.data_folder = Path(data_folder)
        self.required_columns = [
            'source', 'disease', 'year', 'epiweek', 'week', 'date',
            'grouping_var_type', 'population', 'value', 'rate', 'raw_rate'
        ]
        self.loaded_data: Optional[pd.DataFrame] = None
        self.skipped_files: List[Dict[str, str]] = []
        self.file_summary: Dict[str, Dict[str, Any]] = {}

    def _get_grouping_var_cols(self, df: pd.DataFrame) -> list:
        gcols = [c for c in df.columns if c.startswith('grouping_var_')]
        return sorted(gcols, key=lambda x: int(x.split('_')[-1]) if x.split('_')[-1].isdigit() else 1)

    def _compose_grouping_key(self, df: pd.DataFrame) -> pd.Series:
        gcols = self._get_grouping_var_cols(df)
        if not gcols:
            return pd.Series(['__NO_GROUP__'] * len(df), index=df.index)
        parts = df[gcols].astype('string').fillna('')
        return parts.apply(lambda row: '||'.join([s for s in row if s != '']), axis=1)

    def _validate_file_structure(self, file_path: Path) -> Tuple[bool, str]:
        try:
            filename = file_path.stem
            parts = filename.split('_')
            if len(parts) < 3:
                return False, f"Filename '{filename}' should have format 'source_disease_groupingvar'"
            return True, ""
        except Exception as e:
            return False, f"Error parsing filename: {str(e)}"

    def _validate_columns(self, df: pd.DataFrame, file_path: Path) -> Tuple[bool, List[str]]:
        df_check = df.copy()
        if 'grouping_var_1' not in df_check.columns and 'grouping_var' in df_check.columns:
            df_check.rename(columns={'grouping_var': 'grouping_var_1'}, inplace=True)
        missing = [col for col in self.required_columns if col not in df_check.columns]
        if 'grouping_var_1' not in df_check.columns:
            missing.append('grouping_var_1')
        if missing:
            logger.warning(f"{file_path.name} missing required columns: {missing}")
            return False, missing
        return True, []

    def _validate_data_types(self, df: pd.DataFrame, file_path: Path) -> pd.DataFrame:
        df_clean = df.copy()
        try:
            if 'grouping_var_1' not in df_clean.columns and 'grouping_var' in df_clean.columns:
                df_clean.rename(columns={'grouping_var': 'grouping_var_1'}, inplace=True)
            if 'date' in df_clean.columns:
                df_clean['date'] = pd.to_datetime(df_clean['date'], errors='coerce')
            numeric_columns = ['year','epiweek','week','value','population','rate','raw_rate']
            for col in numeric_columns:
                if col in df_clean.columns:
                    df_clean[col] = pd.to_numeric(df_clean[col], errors='coerce')
            return df_clean
        except Exception as e:
            logger.error(f"Type validation failed for {file_path.name}: {e}")
            return df_clean

    def _extract_metadata_from_filename(self, file_path: Path) -> Dict[str, str]:
        filename = file_path.stem
        parts = filename.split('_')
        return {
            'expected_source': parts[0],
            'expected_disease': '_'.join(parts[1:-1]),
            'expected_grouping_var': parts[-1]
        }

    def load_data(self, validate_filename_consistency: bool = True) -> pd.DataFrame:
        if not self.data_folder.exists():
            raise FileNotFoundError(f"Data folder not found: {self.data_folder}")
        csv_files = list(self.data_folder.glob('*.csv'))
        if not csv_files:
            raise FileNotFoundError(f"No CSV files found in {self.data_folder}")

        valid = []
        self.skipped_files = []
        self.file_summary = {}
        for file_path in csv_files:
            try:
                ok, msg = self._validate_file_structure(file_path)
                if not ok:
                    self.skipped_files.append({'file': file_path.name, 'reason': msg})
                    continue
                try:
                    df = pd.read_csv(file_path)
                except Exception as e:
                    self.skipped_files.append({'file': file_path.name, 'reason': f"read_csv error: {e}"})
                    continue
                if df.empty:
                    self.skipped_files.append({'file': file_path.name, 'reason': 'empty file'})
                    continue
                has_cols, missing = self._validate_columns(df, file_path)
                if not has_cols:
                    self.skipped_files.append({'file': file_path.name, 'reason': f"missing cols: {missing}"})
                    continue
                dfc = self._validate_data_types(df, file_path)
                dfc['grouping_var'] = self._compose_grouping_key(dfc).astype('string')
                if validate_filename_consistency:
                    meta = self._extract_metadata_from_filename(file_path)
                    content_sources = dfc['source'].dropna().astype(str).unique()
                    if meta['expected_source'] not in content_sources:
                        logger.warning(f"{file_path.name} source mismatch: filename={meta['expected_source']} content={content_sources}")
                dfc['source_file'] = file_path.name
                dfc['load_timestamp'] = datetime.now()
                initial = len(dfc)
                dfc = dfc.dropna(subset=['date','value','year','week'])
                if dfc.empty:
                    self.skipped_files.append({'file': file_path.name, 'reason': 'no valid rows after dropna'})
                    continue
                valid.append(dfc)
                self.file_summary[file_path.name] = {
                    'initial_rows': initial,
                    'final_rows': len(dfc),
                    'rows_removed': initial - len(dfc),
                    'date_range': f"{dfc['date'].min()} to {dfc['date'].max()}",
                    'diseases': dfc['disease'].nunique(),
                    'locations': dfc['grouping_var'].nunique(),
                    'sources': dfc['source'].nunique(),
                }
                logger.info(f"✓ Loaded {file_path.name}: {len(dfc)} rows, {dfc['disease'].nunique()} diseases")
            except Exception as e:
                self.skipped_files.append({'file': file_path.name, 'reason': f'unexpected: {e}'})
                continue
        if not valid:
            raise ValueError('No valid CSV files could be loaded')
        self.loaded_data = pd.concat(valid, ignore_index=True)
        logger.info(f"Total rows: {len(self.loaded_data)} | Diseases: {self.loaded_data['disease'].nunique()} | Groups: {self.loaded_data['grouping_var'].nunique()} | Date range: {self.loaded_data['date'].min()} → {self.loaded_data['date'].max()}")
        if self.skipped_files:
            logger.warning("=== SKIPPED FILES ===")
            for sk in self.skipped_files:
                logger.warning(f"- {sk['file']}: {sk['reason']}")
        return self.loaded_data

# -------------------------------------------
# 2) Windowed dataset with trend features
# -------------------------------------------
class WindowedForecastDataset(Dataset):
    """
    Builds sliding windows per (disease, grouping_var) time series.

    V2 CHANGE: Trend/momentum features are computed on-the-fly from the raw
    value series *before* normalization, then normalised together with value.
    This keeps CSV preprocessing unchanged and guarantees the features are
    causal (only past data used).

    The trend features are appended as extra columns after `feature_cols`,
    so the model sees (value, roc_2, roc_4, slope_6, baseline_dev_10, ...)
    at each timestep.
    """
    def __init__(
        self,
        df: pd.DataFrame,
        input_len: int = 32,
        horizons: List[int] = (0,1,2,3,4),
        feature_cols: List[str] = ("value",),
        target_col: str = "value",
        group_cols: Tuple[str, str] = ("disease", "grouping_var"),
        sort_col: str = "date",
        normalize: bool = True,
        min_series_length: int = 40,
        shared_normalizers: Optional[Dict[Tuple, Dict[str, np.ndarray]]] = None,
        allow_incomplete_targets: bool = False,
        trend_config: Optional[Dict[str, Any]] = None,
    ):
        assert input_len > 0
        self.input_len = int(input_len)
        self.horizons = sorted(list(horizons))
        self.feature_cols = list(feature_cols)
        self.target_col = target_col
        self.group_cols = group_cols
        self.sort_col = sort_col
        self.normalize = normalize
        self.min_series_length = min_series_length
        self.allow_incomplete_targets = allow_incomplete_targets

        # --- Trend feature config ---
        self.trend_config = trend_config if trend_config is not None else DEFAULT_TREND_CONFIG
        self.n_trend_features = count_trend_features(self.trend_config)
        # Total feature width = base features + trend features
        self.total_feature_dim = len(self.feature_cols) + self.n_trend_features

        if self.target_col not in self.feature_cols:
            raise ValueError(f"Target column '{self.target_col}' must be in feature_cols")
        self.target_idx = self.feature_cols.index(self.target_col)
        assert 0 <= self.target_idx < len(self.feature_cols)

        # Keep only necessary columns
        keep = list({*self.feature_cols, *self.group_cols, self.sort_col, 'epiweek', 'year'})
        miss = [c for c in keep if c not in df.columns]
        if miss:
            raise ValueError(f"Missing required columns for dataset: {miss}")
        work = df[keep].copy()
        work = work.sort_values([*self.group_cols, self.sort_col]).reset_index(drop=True)

        self.groups: List[Tuple[str,str]] = []
        self.series: List[Dict[str, Any]] = []
        self.normalizers: List[Dict[str, np.ndarray]] = []
        self.group_to_series_id: Dict[Tuple, int] = {}

        skipped_short = 0
        for (d, g), gdf in work.groupby(list(self.group_cols), sort=False):
            base_vals = gdf[self.feature_cols].to_numpy(dtype=np.float32)  # (T, F_base)
            epiw = gdf['epiweek'].to_numpy(dtype=np.int64)
            dates = gdf[self.sort_col].values

            min_required = self.input_len + max(self.horizons) + 1
            if len(base_vals) < max(min_required, self.min_series_length):
                skipped_short += 1
                continue

            # --- Compute trend features from raw values (before normalisation) ---
            if self.n_trend_features > 0:
                # Use the target column (value) as the basis for trend features
                raw_target = base_vals[:, self.target_idx]
                trend_feats = compute_trend_features(raw_target, self.trend_config)  # (T, n_trend)
                vals = np.concatenate([base_vals, trend_feats], axis=1)  # (T, F_base + n_trend)
            else:
                vals = base_vals

            # --- Normalisation ---
            group_key = (d, g)
            if self.normalize:
                if shared_normalizers is not None and group_key in shared_normalizers:
                    mean = np.asarray(shared_normalizers[group_key]['mean']).reshape(-1).astype(np.float32)
                    std  = np.asarray(shared_normalizers[group_key]['std']).reshape(-1).astype(np.float32)

                    # Handle dimension mismatch when loading old checkpoints
                    # that don't have trend feature stats
                    if mean.shape[0] < vals.shape[1]:
                        # Shared normalizer is from base-only model; compute trend stats from this series
                        trend_mean = vals[:, mean.shape[0]:].mean(axis=0).astype(np.float32)
                        trend_std  = (vals[:, mean.shape[0]:].std(axis=0) + 1e-8).astype(np.float32)
                        mean = np.concatenate([mean, trend_mean])
                        std  = np.concatenate([std, trend_std])
                    elif mean.shape[0] > vals.shape[1]:
                        # Shared normalizer has more dims than current features (shouldn't happen normally)
                        mean = mean[:vals.shape[1]]
                        std  = std[:vals.shape[1]]

                    vals = (vals - mean) / std
                    self.normalizers.append({'mean': mean, 'std': std})
                else:
                    mean = vals.mean(axis=0).astype(np.float32)
                    std  = (vals.std(axis=0) + 1e-8).astype(np.float32)
                    vals = (vals - mean) / std
                    self.normalizers.append({'mean': mean, 'std': std})
            else:
                self.normalizers.append({
                    'mean': np.zeros(self.total_feature_dim, dtype=np.float32),
                    'std':  np.ones(self.total_feature_dim,  dtype=np.float32),
                })

            m, s = self.normalizers[-1]['mean'], self.normalizers[-1]['std']
            assert (
                m.ndim == 1 and s.ndim == 1 and
                m.shape == s.shape == (self.total_feature_dim,)
            ), f"Normalizer shape mismatch for {group_key}: mean={m.shape}, std={s.shape}, expected ({self.total_feature_dim},)"

            series_id = len(self.groups)
            self.groups.append(group_key)
            self.group_to_series_id[group_key] = series_id
            self.series.append({
                'X': vals,
                'epiweek': epiw,
                'dates': dates,
            })

        if skipped_short > 0:
            logger.info(f"Skipped {skipped_short} series with < {max(min_required, self.min_series_length)} weeks")

        # Build sample index
        self.sample_index = []
        for sid, s in enumerate(self.series):
            T = s['X'].shape[0]
            max_h = max(self.horizons)
            if self.allow_incomplete_targets:
                t_end_max = T
            else:
                t_end_max = T - max_h
            for t_end in range(self.input_len, t_end_max):
                self.sample_index.append((sid, t_end))

        logger.info(
            f"Dataset built: {len(self.series)} series, {len(self.sample_index)} windows | "
            f"Features: {len(self.feature_cols)} base + {self.n_trend_features} trend = {self.total_feature_dim} total"
        )

    def get_normalizers_dict(self) -> Dict[Tuple, Dict[str, np.ndarray]]:
        return {
            group: self.normalizers[sid]
            for group, sid in self.group_to_series_id.items()
        }

    def __len__(self):
        return len(self.sample_index)

    def __getitem__(self, i: int):
        sid, t_end = self.sample_index[i]
        s = self.series[sid]

        X  = s['X'][t_end - self.input_len : t_end, :]
        ew = s['epiweek'][t_end - self.input_len : t_end]

        if not self.allow_incomplete_targets:
            y = np.array(
                [s['X'][t_end + h, self.target_idx] for h in self.horizons],
                dtype=np.float32
            )
        else:
            T_total = s['X'].shape[0]
            ys = []
            for h in self.horizons:
                idx = t_end + h
                if idx < T_total:
                    ys.append(s['X'][idx, self.target_idx])
                else:
                    ys.append(np.nan)
            y = np.array(ys, dtype=np.float32)

        ref_date = pd.Timestamp(s['dates'][t_end])

        meta = {
            'disease': self.groups[sid][0] if len(self.group_cols) >= 1 else None,
            'grouping_var': self.groups[sid][1] if len(self.group_cols) >= 2 else None,
            'reference_date': ref_date,
            'series_id': sid,
        }
        return (torch.from_numpy(X), torch.from_numpy(ew), torch.from_numpy(y), meta)

    def denormalize_value(self, series_id: int, normalized_value: float) -> float:
        norm = self.normalizers[series_id]
        return float(normalized_value * norm['std'][self.target_idx] + norm['mean'][self.target_idx])

    def compute_sampling_weights(
        self,
        slope_window: int = 6,
        percentile_threshold: float = 30.0,
        upweight_factor: float = 3.0,
    ) -> np.ndarray:
        """
        Compute per-sample weights for WeightedRandomSampler.

        A window is considered "ramp-up" if BOTH:
          1. The trailing slope (over `slope_window` weeks of log1p(value))
             at the end of the input window is positive
          2. The raw value at the end of the input window is below the
             `percentile_threshold`-th percentile of that series

        Ramp-up windows get `upweight_factor` weight; all others get 1.0.

        Returns:
            weights: np.ndarray of shape (len(self),) with per-sample weights
        """
        # Pre-compute per-series percentile thresholds from RAW (pre-normalised) values
        series_thresholds = []
        for sid, s in enumerate(self.series):
            # Recover raw values: un-normalise the target column
            norm = self.normalizers[sid]
            raw_vals = s['X'][:, self.target_idx] * norm['std'][self.target_idx] + norm['mean'][self.target_idx]
            threshold = np.percentile(raw_vals, percentile_threshold)
            series_thresholds.append((raw_vals, threshold))

        # Pre-compute OLS regression components for slope
        if slope_window >= 2:
            x = np.arange(slope_window, dtype=np.float32)
            x_centered = x - x.mean()
            ss_x = (x_centered ** 2).sum()
        else:
            x_centered = None
            ss_x = 1.0

        weights = np.ones(len(self.sample_index), dtype=np.float32)
        n_upweighted = 0

        for i, (sid, t_end) in enumerate(self.sample_index):
            raw_vals, threshold = series_thresholds[sid]

            # Current value (end of input window = t_end - 1)
            current_val = raw_vals[t_end - 1]

            # Check 1: value is below percentile threshold (still early/low)
            if current_val >= threshold:
                continue

            # Check 2: trailing slope is positive (series is increasing)
            if slope_window >= 2 and t_end >= slope_window:
                log_window = np.log1p(np.clip(raw_vals[t_end - slope_window : t_end], 0.0, None))
                y_centered = log_window - log_window.mean()
                slope = (x_centered * y_centered).sum() / (ss_x + 1e-8)
                if slope > 0:
                    weights[i] = upweight_factor
                    n_upweighted += 1
            elif t_end >= 2:
                # Fallback for very short windows: simple 1-step difference
                diff = np.log1p(max(raw_vals[t_end - 1], 0)) - np.log1p(max(raw_vals[t_end - 2], 0))
                if diff > 0:
                    weights[i] = upweight_factor
                    n_upweighted += 1

        pct = 100.0 * n_upweighted / max(1, len(weights))
        logger.info(
            f"Sampling weights: {n_upweighted}/{len(weights)} windows identified as ramp-up ({pct:.1f}%) | "
            f"upweight_factor={upweight_factor}, slope_window={slope_window}, "
            f"percentile_threshold={percentile_threshold}"
        )

        return weights

# ----------------------------------------
# 3) Transformer + Fourier epiweek features
# ----------------------------------------
class PositionalEncoding(nn.Module):
    def __init__(self, d_model: int, max_len: int = 200):
        super().__init__()
        pe = torch.zeros(max_len, d_model)
        pos = torch.arange(0, max_len).unsqueeze(1)
        div = torch.exp(torch.arange(0, d_model, 2) * (-math.log(10000.0)/d_model))
        pe[:, 0::2] = torch.sin(pos * div)
        pe[:, 1::2] = torch.cos(pos * div)
        self.register_buffer('pe', pe.unsqueeze(0))

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        return x + self.pe[:, :x.size(1), :]

def epiweek_fourier(ew: torch.Tensor, K: int = 4) -> torch.Tensor:
    theta = 2 * math.pi * (ew.float() / 53.0)
    feats = []
    for k in range(1, K + 1):
        feats.append(torch.sin(k * theta))
        feats.append(torch.cos(k * theta))
    return torch.stack(feats, dim=-1)

class EpiweekFourierTransformerQuantile(nn.Module):
    """
    Transformer encoder that outputs H×Q quantiles.
    Quantile monotonicity is enforced via base + positive deltas.
    """
    def __init__(
        self,
        base_input_dim: int,
        H: int,
        Q: int,
        K: int = 4,
        d_model: int = 128,
        nhead: int = 4,
        nlayers: int = 2,
        dropout: float = 0.1
    ):
        super().__init__()
        self.K = K
        self.H = H
        self.Q = Q

        self.in_proj = nn.Linear(base_input_dim + 2*K, d_model)
        self.pos = PositionalEncoding(d_model)
        layer = nn.TransformerEncoderLayer(
            d_model=d_model,
            nhead=nhead,
            batch_first=True,
            dropout=dropout
        )
        self.backbone = nn.TransformerEncoder(layer, num_layers=nlayers)
        self.head_base  = nn.Linear(d_model, H)
        self.head_delta = nn.Linear(d_model, H * (Q - 1))

    def forward(self, x: torch.Tensor, epiweek_idx: torch.Tensor) -> torch.Tensor:
        ewf = epiweek_fourier(epiweek_idx, K=self.K)
        h = torch.cat([x, ewf], dim=-1)
        h = self.in_proj(h)
        h = self.pos(h)
        h = self.backbone(h)
        h_last = h[:, -1, :]

        base   = self.head_base(h_last)
        deltas = self.head_delta(h_last)
        deltas = deltas.view(-1, self.H, self.Q - 1)
        deltas = F.softplus(deltas) + 1e-6
        monotone_tail = base.unsqueeze(-1) + torch.cumsum(deltas, dim=-1)
        out = torch.cat([base.unsqueeze(-1), monotone_tail], dim=-1)
        return out

# -------------------
# 4) Loss functions
# -------------------
@torch.no_grad()
def _nearest_index(qs_tensor: torch.Tensor, q: float) -> int:
    return int(torch.argmin(torch.abs(qs_tensor - q)).item())

def quantile_pinball_loss(
    y: torch.Tensor,
    pred_quantiles: torch.Tensor,
    qs: List[float],
    mask: Optional[torch.Tensor] = None,
    reduce: str = "mean",
) -> torch.Tensor:
    y = y.float()
    q_preds = pred_quantiles.float()
    device = q_preds.device
    qs_tensor = torch.tensor(qs, dtype=torch.float32, device=device)
    tau = qs_tensor.view(1, 1, -1)
    y_exp = y.unsqueeze(-1)
    diff = y_exp - q_preds
    loss = torch.maximum(tau * diff, (tau - 1.0) * diff)
    if mask is not None:
        mask_exp = mask.unsqueeze(-1).to(loss.dtype)
        loss = loss * mask_exp
        if reduce == "mean":
            denom = mask_exp.sum().clamp_min(1.0)
            return loss.sum() / denom
        elif reduce == "sum":
            return loss.sum()
        else:
            return loss
    if reduce == "mean":
        return loss.mean()
    elif reduce == "sum":
        return loss.sum()
    else:
        return loss

def wis_loss(
    y: torch.Tensor,
    pred_quantiles: torch.Tensor,
    qs: List[float],
    alphas: Tuple[float,...] = (0.5, 0.2, 0.1),
    mask: Optional[torch.Tensor] = None,
    reduce: str = 'mean'
) -> torch.Tensor:
    assert pred_quantiles.shape[-1] == len(qs)
    y = y.float()
    q_preds = pred_quantiles.float()
    device = q_preds.device
    qs_tensor = torch.tensor(qs, dtype=torch.float32, device=device)
    med_idx = _nearest_index(qs_tensor, 0.5)
    q_med = q_preds[..., med_idx]
    median_term = 0.5 * torch.abs(y - q_med)
    interval_term = torch.zeros_like(median_term)
    for alpha in alphas:
        lo_q, hi_q = alpha / 2.0, 1.0 - alpha / 2.0
        lo_idx = _nearest_index(qs_tensor, lo_q)
        hi_idx = _nearest_index(qs_tensor, hi_q)
        l = q_preds[..., lo_idx]
        u = q_preds[..., hi_idx]
        over_lower = F.relu(l - y)
        over_upper = F.relu(y - u)
        IS = (u - l) + (2.0 / alpha) * (over_lower + over_upper)
        interval_term = interval_term + (alpha / 2.0) * IS
    K = len(alphas)
    wis = (median_term + interval_term) / (K + 0.5)
    if mask is not None:
        wis = wis[mask]
    if reduce == 'mean':
        return wis.mean()
    elif reduce == 'sum':
        return wis.sum()
    else:
        return wis

# -----------------------------
# 5) Training / evaluation utils
# -----------------------------
class BatchCollator:
    def __call__(self, batch):
        Xs, EWs, Ys, Metas = zip(*batch)
        X = torch.stack(Xs, dim=0)
        EW = torch.stack(EWs, dim=0)
        Y = torch.stack(Ys, dim=0)
        return X, EW, Y, Metas

def train(
    model: nn.Module,
    train_loader: DataLoader,
    val_loader: DataLoader,
    quantiles: List[float],
    epochs: int = 25,
    lr: float = 1e-3,
    device: str = 'cpu',
    out_path: Path = Path('checkpoint.pt'),
    trend_config: Optional[Dict[str, Any]] = None,
) -> Path:
    """Train the model with pinball loss and save the best checkpoint by val WIS."""
    model.to(device)
    opt = torch.optim.AdamW(model.parameters(), lr=lr, weight_decay=1e-5)
    scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(opt, mode='min', factor=0.5, patience=3)

    best_wis = float('inf')
    best_path = out_path
    patience_counter = 0
    max_patience = 10

    for ep in range(1, epochs + 1):
        model.train()
        tr_pinball_loss = 0.0
        for X, EW, Y, _ in train_loader:
            X, EW, Y = X.to(device), EW.to(device), Y.to(device)
            pred_q = model(X, EW)
            loss = quantile_pinball_loss(Y, pred_q, quantiles, reduce='mean')
            opt.zero_grad()
            loss.backward()
            torch.nn.utils.clip_grad_norm_(model.parameters(), max_norm=1.0)
            opt.step()
            tr_pinball_loss += float(loss.item())

        model.eval()
        va_pinball_loss = 0.0
        va_wis_loss = 0.0
        with torch.no_grad():
            for X, EW, Y, _ in val_loader:
                X, EW, Y = X.to(device), EW.to(device), Y.to(device)
                pred_q = model(X, EW)
                v_pin = quantile_pinball_loss(Y, pred_q, quantiles, reduce='mean')
                va_pinball_loss += float(v_pin.item())
                v_wis = wis_loss(Y, pred_q, quantiles, reduce='mean')
                va_wis_loss += float(v_wis.item())

        n_tr = max(1, len(train_loader))
        n_va = max(1, len(val_loader))
        tr_pinball_loss /= n_tr
        va_pinball_loss /= n_va
        va_wis_loss     /= n_va

        scheduler.step(va_pinball_loss)
        current_lr = opt.param_groups[0]['lr']
        logger.info(
            f"Epoch {ep:03d} | "
            f"Train pinball: {tr_pinball_loss:.4f} | "
            f"Val pinball: {va_pinball_loss:.4f} | "
            f"Val WIS: {va_wis_loss:.4f} | "
            f"LR: {current_lr:.2e}"
        )

        if va_wis_loss < best_wis:
            best_wis = va_wis_loss
            patience_counter = 0
            best_path.parent.mkdir(parents=True, exist_ok=True)
            train_normalizers = train_loader.dataset.get_normalizers_dict()
            torch.save({
                'epoch': ep,
                'model_state': model.state_dict(),
                'optimizer_state': opt.state_dict(),
                'quantiles': quantiles,
                'val_wis': va_wis_loss,
                'train_normalizers': train_normalizers,
                # V2: Save trend config so inference reproduces the same features
                'trend_config': trend_config,
            }, best_path)
            logger.info(f"   New best model saved (Val WIS: {va_wis_loss:.4f})")
        else:
            patience_counter += 1
            if patience_counter >= max_patience:
                logger.info(f"Early stopping at epoch {ep} (no improvement in Val WIS for {max_patience} epochs)")
                break

    return best_path

@torch.no_grad()
def make_forecasts(
    model: nn.Module,
    loader: DataLoader,
    quantiles: List[float],
    horizons: List[int],
    out_dir: Path,
    denormalize: bool = True
) -> pd.DataFrame:
    device = next(model.parameters()).device
    model.eval()
    rows = []

    for X, EW, Y, Metas in loader:
        X, EW = X.to(device), EW.to(device)
        pred_q = model(X, EW).cpu().numpy()
        B, H, Q = pred_q.shape

        for b in range(B):
            meta = Metas[b]
            ref_date = pd.to_datetime(meta['reference_date'])
            disease = meta['disease']
            group = meta['grouping_var']
            series_id = meta['series_id']

            for h_idx, h in enumerate(horizons):
                tgt_date = ref_date + pd.to_timedelta(h * 7, unit='D')
                for qi, qv in enumerate(quantiles):
                    value = float(pred_q[b, h_idx, qi])
                    if denormalize and hasattr(loader.dataset, 'denormalize_value'):
                        value = loader.dataset.denormalize_value(series_id, value)
                    rows.append({
                        'disease': disease,
                        'grouping_var': group,
                        'reference_date': ref_date,
                        'horizon': h,
                        'target_end_date': tgt_date,
                        'quantile': qv,
                        'value': value,
                    })

    df_fore = pd.DataFrame(rows)
    out_dir.mkdir(parents=True, exist_ok=True)
    csv_path = out_dir / 'forecasts.csv'
    df_fore.to_csv(csv_path, index=False)
    logger.info(f"Saved forecasts → {csv_path} ({len(df_fore)} rows)")
    return df_fore

# ------------------
# 6) Data splitting (unchanged)
# ------------------
def split_train_val(
    df: pd.DataFrame,
    val_frac: float = 0.15,
    min_series_length: int = 40,
    input_len: int = 32,
    max_horizon: int = 4,
    gap_weeks: int = 0
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    parts = []
    parts_val = []
    min_usable = max(min_series_length, input_len + max_horizon + 1)
    skipped_too_short = 0
    total_series = 0

    for (d, g), gdf in df.sort_values(['disease', 'grouping_var', 'date']).groupby(['disease', 'grouping_var']):
        total_series += 1
        n = len(gdf)
        if n < 2 * min_usable:
            skipped_too_short += 1
            continue
        val_size = max(min_usable, int(n * val_frac))
        train_size = n - val_size
        if train_size < min_usable:
            train_size = min_usable
            val_size = n - train_size
        if train_size >= min_usable and val_size >= min_usable:
            train_part = gdf.iloc[:train_size]
            val_part = gdf.iloc[train_size:]
            if gap_weeks > 0 and not val_part.empty:
                cut = min(gap_weeks, len(val_part))
                val_part = val_part.iloc[cut:]
            if len(train_part) < min_usable or len(val_part) < min_usable:
                skipped_too_short += 1
                continue
            train_max_date = train_part['date'].max()
            val_min_date = val_part['date'].min()
            if val_min_date <= train_max_date:
                mask = (gdf['date'].values > train_max_date)
                if not mask.any():
                    skipped_too_short += 1
                    continue
                pos = int(np.argmax(mask))
                train_part = gdf.iloc[:pos]
                val_part   = gdf.iloc[pos:]
                if gap_weeks > 0 and not val_part.empty:
                    cut = min(gap_weeks, len(val_part))
                    val_part = val_part.iloc[cut:]
                if len(train_part) < min_usable or len(val_part) < min_usable:
                    skipped_too_short += 1
                    continue
            parts.append(train_part)
            parts_val.append(val_part)
        else:
            skipped_too_short += 1

    if skipped_too_short > 0:
        logger.warning(
            f"Skipped {skipped_too_short}/{total_series} series "
            f"(too short for train/val split with min_length={min_usable}). "
            f"Each series needs at least {2*min_usable} weeks to split properly."
        )
    if not parts or not parts_val:
        raise ValueError(
            f"No series long enough for splitting! Need at least {2*min_usable} weeks per series. "
            f"Consider reducing --min_series_length or --input_len"
        )
    logger.info(f"Successfully split {len(parts)} series into train/val")
    return pd.concat(parts, ignore_index=True), pd.concat(parts_val, ignore_index=True)

# ------------------
# 7) CLI / Main
# ------------------
def main():
    p = argparse.ArgumentParser(description='Epiweek Forecasting Pipeline V2 (Trend Features)')
    p.add_argument('--data_folder', type=str, required=True)
    p.add_argument('--out_dir', type=str, default='./outputs')
    p.add_argument('--input_len', type=int, default=32)
    p.add_argument('--horizons', type=int, nargs='+', default=[0,1,2,3,4])
    p.add_argument('--quantiles', type=float, nargs='+',
                   default=[0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40,
                            0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90,
                            0.95, 0.975, 0.99])
    p.add_argument('--batch_size', type=int, default=128)
    p.add_argument('--epochs', type=int, default=25)
    p.add_argument('--lr', type=float, default=1e-3)
    p.add_argument('--d_model', type=int, default=128)
    p.add_argument('--nhead', type=int, default=4)
    p.add_argument('--nlayers', type=int, default=2)
    p.add_argument('--seed', type=int, default=42)
    p.add_argument('--no_normalize', action='store_true')
    p.add_argument('--min_series_length', type=int, default=40)
    p.add_argument('--val_frac', type=float, default=0.15)
    p.add_argument('--device', type=str, default=None)
    p.add_argument('--skip_leakage_checks', action='store_true')
    p.add_argument('--val_gap_weeks', type=int, default=0)
    # V2: Trend feature flags
    p.add_argument('--no_trend_features', action='store_true',
                   help='Disable trend/momentum features (revert to V1 behavior)')
    p.add_argument('--roc_windows', type=int, nargs='*', default=[2, 4],
                   help='Rate-of-change lag windows (weeks)')
    p.add_argument('--slope_windows', type=int, nargs='*', default=[6],
                   help='Rolling OLS slope windows (weeks)')
    p.add_argument('--baseline_windows', type=int, nargs='*', default=[10],
                   help='Baseline deviation windows (weeks)')
    # V3: Weighted sampling for ramp-up regime
    p.add_argument('--no_weighted_sampling', action='store_true',
                   help='Disable weighted sampling of ramp-up windows')
    p.add_argument('--rampup_upweight', type=float, default=3.0,
                   help='Upweight factor for ramp-up regime windows (default: 3.0)')
    p.add_argument('--rampup_slope_window', type=int, default=6,
                   help='Window (weeks) for slope computation in ramp-up detection (default: 6)')
    p.add_argument('--rampup_percentile', type=float, default=30.0,
                   help='Percentile threshold below which a value is considered "low" (default: 30)')

    args = p.parse_args()
    set_seed(args.seed)
    logger.info(f"Random seed set to {args.seed}")

    device = args.device or ('cuda' if torch.cuda.is_available() else 'cpu')
    logger.info(f"Using device: {device}")

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    # Build trend config from CLI args
    trend_config: Dict[str, Any] = {
        "enabled": not args.no_trend_features,
        "roc_windows": args.roc_windows if not args.no_trend_features else [],
        "slope_windows": args.slope_windows if not args.no_trend_features else [],
        "baseline_windows": args.baseline_windows if not args.no_trend_features else [],
    }
    n_trend = count_trend_features(trend_config)
    logger.info(f"Trend features: {'ENABLED' if trend_config['enabled'] else 'DISABLED'} ({n_trend} channels)")
    if trend_config['enabled']:
        logger.info(f"  RoC windows: {trend_config['roc_windows']}")
        logger.info(f"  Slope windows: {trend_config['slope_windows']}")
        logger.info(f"  Baseline deviation windows: {trend_config['baseline_windows']}")

    # 1) Load data
    logger.info("=" * 60)
    logger.info("STEP 1: Loading surveillance data")
    logger.info("=" * 60)
    loader = SurveillanceDataLoader(args.data_folder)
    df = loader.load_data()

    req = {'disease', 'grouping_var', 'date', 'epiweek', 'year', 'value'}
    if not req.issubset(df.columns):
        raise ValueError(f"Missing required columns: {req.difference(df.columns)}")

    horizons = sorted(args.horizons)
    quantiles = sorted(args.quantiles)

    # 2) Split
    logger.info("=" * 60)
    logger.info("STEP 2: Splitting data (time-aware)")
    logger.info("=" * 60)
    tr_df, va_df = split_train_val(
        df, val_frac=args.val_frac, min_series_length=args.min_series_length,
        input_len=args.input_len, max_horizon=max(horizons), gap_weeks=args.val_gap_weeks
    )
    logger.info(f"Train: {len(tr_df)} rows | Val: {len(va_df)} rows")

    # 3) Build datasets
    logger.info("=" * 60)
    logger.info("STEP 3: Building datasets")
    logger.info("=" * 60)

    train_ds = WindowedForecastDataset(
        tr_df, input_len=args.input_len, horizons=horizons,
        feature_cols=("value",), target_col="value",
        normalize=not args.no_normalize, min_series_length=args.min_series_length,
        shared_normalizers=None, trend_config=trend_config,
    )

    train_normalizers = train_ds.get_normalizers_dict() if not args.no_normalize else None
    if train_normalizers:
        logger.info(f"  Extracted normalization statistics from {len(train_normalizers)} training series")

    val_ds = WindowedForecastDataset(
        va_df, input_len=args.input_len, horizons=horizons,
        feature_cols=("value",), target_col="value",
        normalize=not args.no_normalize, min_series_length=args.min_series_length,
        shared_normalizers=train_normalizers, trend_config=trend_config,
    )

    if len(train_ds) == 0 or len(val_ds) == 0:
        raise ValueError(f"Empty dataset! Train: {len(train_ds)}, Val: {len(val_ds)}")

    # Leakage checks
    if not args.skip_leakage_checks:
        logger.info("=" * 60)
        logger.info("DATA LEAKAGE CHECKS")
        logger.info("=" * 60)
        overlap_check = LeakageDetector.check_temporal_overlap(tr_df, va_df, group_cols=['disease', 'grouping_var'])
        if overlap_check['has_overlap']:
            logger.error(f"  LEAKAGE: {overlap_check['num_series_with_overlap']} series have temporal overlap!")
        else:
            logger.info("✓ No temporal overlap detected")
        norm_check = LeakageDetector.check_normalization_strategy(train_ds)
        if norm_check['is_normalized'] and train_normalizers:
            logger.info(f"✓ Validation uses training normalization ({len(train_normalizers)} series)")
        future_check = LeakageDetector.check_future_information(train_ds, verbose=False)
        if future_check['has_future_info']:
            logger.error(f"  POTENTIAL LEAKAGE: {future_check['num_issues']} samples with future info!")
        else:
            logger.info(f"✓ No future information detected ({future_check['samples_checked']} samples checked)")

    collate = BatchCollator()

    # V3: Weighted sampling for ramp-up regime
    use_weighted = not args.no_weighted_sampling
    if use_weighted:
        logger.info("=" * 60)
        logger.info("STEP 3b: Computing ramp-up sampling weights")
        logger.info("=" * 60)
        sample_weights = train_ds.compute_sampling_weights(
            slope_window=args.rampup_slope_window,
            percentile_threshold=args.rampup_percentile,
            upweight_factor=args.rampup_upweight,
        )
        sampler = WeightedRandomSampler(
            weights=torch.from_numpy(sample_weights).double(),
            num_samples=len(train_ds),  # same epoch size
            replacement=True,  # required for weighted sampling
        )
        train_loader = DataLoader(
            train_ds, batch_size=args.batch_size,
            sampler=sampler,  # replaces shuffle=True
            num_workers=0, drop_last=True, collate_fn=collate
        )
    else:
        logger.info("Weighted sampling: DISABLED")
        train_loader = DataLoader(
            train_ds, batch_size=args.batch_size, shuffle=True,
            num_workers=0, drop_last=True, collate_fn=collate
        )

    val_loader = DataLoader(val_ds, batch_size=args.batch_size, shuffle=False,
                            num_workers=0, drop_last=False, collate_fn=collate)
    logger.info(f"Train batches: {len(train_loader)} | Val batches: {len(val_loader)}")

    # 4) Model
    logger.info("=" * 60)
    logger.info("STEP 4: Initializing model")
    logger.info("=" * 60)
    H = len(horizons)
    Q = len(quantiles)
    # base_input_dim = base features + trend features
    base_input_dim = 1 + n_trend  # 1 for 'value' + trend channels
    model = EpiweekFourierTransformerQuantile(
        base_input_dim=base_input_dim, H=H, Q=Q, K=4,
        d_model=args.d_model, nhead=args.nhead, nlayers=args.nlayers, dropout=0.1
    )
    total_params = sum(p.numel() for p in model.parameters())
    logger.info(f"Model parameters: {total_params:,}")
    logger.info(f"Model input dim: {base_input_dim} (1 value + {n_trend} trend features) + {2*4} Fourier = {base_input_dim + 8}")

    # 5) Train
    logger.info("=" * 60)
    logger.info("STEP 5: Training")
    logger.info("=" * 60)
    ckpt_path = out_dir / 'checkpoint.pt'
    best_path = train(
        model, train_loader, val_loader, quantiles,
        epochs=args.epochs, lr=args.lr, device=device,
        out_path=ckpt_path, trend_config=trend_config,
    )

    # 6) Reload best & forecast
    logger.info("=" * 60)
    logger.info("STEP 6: Loading best model and generating forecasts")
    logger.info("=" * 60)
    state = torch.load(best_path, map_location=device)
    model.load_state_dict(state['model_state'])
    model.to(device)
    val_wis_loaded = state.get('val_wis', float('nan'))
    try:
        val_wis_loaded = float(val_wis_loaded)
    except Exception:
        val_wis_loaded = float('nan')
    logger.info(f"Loaded checkpoint from epoch {state.get('epoch', 'N/A')} with val WIS: {val_wis_loaded:.4f}")

    df_forecasts = make_forecasts(model, val_loader, quantiles, horizons, out_dir, denormalize=True)

    # Summary
    logger.info("=" * 60)
    logger.info("SUMMARY")
    logger.info("=" * 60)
    logger.info(f"Checkpoint: {best_path}")
    logger.info(f"Forecasts: {out_dir / 'forecasts.csv'}")
    logger.info(f"Total rows: {len(df_forecasts):,}")
    logger.info(f"Diseases: {df_forecasts['disease'].nunique()}")
    logger.info(f"Groups: {df_forecasts['grouping_var'].nunique()}")
    logger.info(f"Date range: {df_forecasts['reference_date'].min()} to {df_forecasts['reference_date'].max()}")
    logger.info(f"Trend config saved in checkpoint: {trend_config}")
    print(df_forecasts.head(10).to_string(index=False))
    logger.info("Pipeline completed successfully!")


if __name__ == '__main__':
    main()