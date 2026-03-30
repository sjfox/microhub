#!/usr/bin/env python3
# infer_pinball_realtime.py
# V2: Loads trend_config from checkpoint so inference computes identical features.
#     Backward-compatible with V1 checkpoints (no trend features).

from __future__ import annotations
import argparse
import numpy as np
import pickle
from pathlib import Path
import re
import json

import pandas as pd
import torch
import warnings

try:
    from torch.serialization import add_safe_globals
except ImportError:
    def add_safe_globals(*args, **kwargs):
        warnings.warn(
            "torch.serialization.add_safe_globals is not available in this "
            "PyTorch version; proceeding without additional safe globals."
        )
        return None

# Import from V2 training script
from epiweek_forecasting_pinball import (
    SurveillanceDataLoader,
    WindowedForecastDataset,
    EpiweekFourierTransformerQuantile,
    make_forecasts,
    set_seed,
    logger,
    # V2 additions
    DEFAULT_TREND_CONFIG,
    count_trend_features,
)


def infer_model_shapes_from_state(state_dict, ckpt_quantiles_len, base_input_dim=1):
    """
    Infer H, Q, d_model, K, nhead, nlayers from checkpoint weights.
    base_input_dim should include trend features if the checkpoint was trained with them.
    """
    if "head_base.weight" in state_dict and "head_delta.weight" in state_dict:
        head_base_w  = state_dict["head_base.weight"]
        head_delta_w = state_dict["head_delta.weight"]
        d_model = head_base_w.shape[1]
        H = head_base_w.shape[0]
        Q_minus_1 = head_delta_w.shape[0] // H
        Q = 1 + Q_minus_1
        if Q != ckpt_quantiles_len:
            warnings.warn(f"Inferred Q={Q} differs from ckpt quantiles len={ckpt_quantiles_len}.")
            Q = ckpt_quantiles_len
    elif "head.weight" in state_dict:
        head_w = state_dict["head.weight"]
        d_model = head_w.shape[1]
        Q = ckpt_quantiles_len
        H = head_w.shape[0] // Q
    else:
        raise KeyError("No compatible quantile head weights found in state_dict.")

    # K from in_proj: maps (base_input_dim + 2K) -> d_model
    in_proj_in = state_dict["in_proj.weight"].shape[1]
    K = (in_proj_in - base_input_dim) // 2
    if base_input_dim + 2*K != in_proj_in:
        warnings.warn(
            f"Unexpected in_proj input size {in_proj_in}; "
            f"computed base_input_dim({base_input_dim})+2K({2*K}) = {base_input_dim + 2*K}."
        )

    layer_idxs = []
    pat = re.compile(r"^backbone\.layers\.(\d+)\.")
    for k in state_dict.keys():
        m = pat.match(k)
        if m:
            layer_idxs.append(int(m.group(1)))
    nlayers = (max(layer_idxs) + 1) if layer_idxs else 2

    nhead = 1
    for cand in (8, 4, 2, 1):
        if d_model % cand == 0:
            nhead = cand
            break

    return H, Q, d_model, K, nhead, nlayers


def _load_checkpoint(ckpt_path, device, unsafe_load=False):
    """Shared checkpoint loading logic with safe/unsafe fallback."""
    ckpt_path = Path(ckpt_path)
    if not ckpt_path.exists():
        raise FileNotFoundError(f"Checkpoint not found: {ckpt_path}")

    try:
        add_safe_globals([np.core.multiarray._reconstruct])
    except Exception:
        pass

    if unsafe_load:
        logger.warning("unsafe_load flag set – loading checkpoint with weights_only=False")
        return torch.load(ckpt_path, map_location=device)

    try:
        state = torch.load(ckpt_path, map_location=device, weights_only=True)
        missing_critical = [k for k in ("quantiles", "model_state") if k not in state]
        if missing_critical:
            warnings.warn(
                f"Checkpoint loaded with weights_only=True but missing keys: {missing_critical}. "
                "Reloading with weights_only=False."
            )
            state = torch.load(ckpt_path, map_location=device)
    except Exception as e:
        warnings.warn(
            f"weights_only=True failed ({e}). Reloading with weights_only=False."
        )
        state = torch.load(ckpt_path, map_location=device)

    return state


def _extract_checkpoint_metadata(state, no_normalize):
    """Extract quantiles, normalizers, and trend config from checkpoint."""
    qs = state.get("quantiles", None)
    if qs is None:
        raise ValueError("Checkpoint missing 'quantiles'.")
    qs = np.array(sorted(qs), dtype=float)

    # Training normalizers
    train_normalizers = state.get("train_normalizers", None)
    if train_normalizers is not None and not no_normalize:
        logger.info(f"✓ Loaded training normalizers for {len(train_normalizers)} series")
    elif not no_normalize:
        logger.warning(
            "  NORMALIZATION LEAKAGE WARNING: Checkpoint has no 'train_normalizers'.\n"
            "   Retrain with fixed script or use --no_normalize."
        )
        train_normalizers = None
    else:
        train_normalizers = None

    # V2: Load trend config (backward-compatible with V1 checkpoints)
    trend_config = state.get("trend_config", None)
    if trend_config is not None:
        n_trend = count_trend_features(trend_config)
        logger.info(f"✓ Loaded trend config from checkpoint ({n_trend} trend features)")
        logger.info(f"  RoC: {trend_config.get('roc_windows', [])}, "
                     f"Slope: {trend_config.get('slope_windows', [])}, "
                     f"Baseline: {trend_config.get('baseline_windows', [])}")
    else:
        logger.info("  No trend_config in checkpoint (V1 model) — using no trend features")
        trend_config = {"enabled": False, "roc_windows": [], "slope_windows": [], "baseline_windows": []}
        n_trend = 0

    if "model_state" not in state:
        raise KeyError("Checkpoint missing 'model_state'.")

    return qs, train_normalizers, trend_config, n_trend


#############
# Callable from R via reticulate
#############
def run_epiweek_forecast_from_folder(
    data_folder: str,
    checkpoint: str,
    out_dir: str = "./outputs_infer",
    input_len: int = 32,
    horizons: list[int] = (0, 1, 2, 3, 4),
    no_normalize: bool = False,
    min_series_length: int = 40,
    device: str | None = None,
    include_group_types: list[str] | None = None,
    include_diseases: list[str] | None = None,
    latest_only: bool = False,
    score: bool = False,
    coverage_alphas: list[float] | tuple[float, ...] = (0.5, 0.2, 0.1),
    year_min: int | None = None,
    year_max: int | None = None,
    unsafe_load: bool = False,
    seed: int = 42,
) -> pd.DataFrame:
    set_seed(seed)
    logger.info(f"Inference seed set to {seed}")

    device = device or ("cuda" if torch.cuda.is_available() else "cpu")
    logger.info(f"Inference device: {device}")

    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    # 1) Load data
    loader = SurveillanceDataLoader(data_folder)
    df = loader.load_data(validate_filename_consistency=False)

    if include_group_types:
        df = df[df["grouping_var_type"].isin(include_group_types)]
    if include_diseases:
        df = df[df["disease"].isin(include_diseases)]
    if year_min is not None:
        df = df[df["year"] >= year_min]
    if year_max is not None:
        df = df[df["year"] <= year_max]

    req = {"disease", "grouping_var", "date", "epiweek", "year", "value"}
    missing = req.difference(df.columns)
    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    # 2) Load checkpoint
    state = _load_checkpoint(checkpoint, device, unsafe_load)
    qs, train_normalizers, trend_config, n_trend = _extract_checkpoint_metadata(state, no_normalize)
    model_state = state["model_state"]

    # base_input_dim = 1 (value) + n_trend
    base_input_dim = 1 + n_trend

    H_ck, Q_ck, d_model, K, nhead, nlayers = infer_model_shapes_from_state(
        model_state, ckpt_quantiles_len=len(qs), base_input_dim=base_input_dim
    )

    if H_ck != len(horizons):
        raise ValueError(
            f"Horizons mismatch: checkpoint expects {H_ck}, got {len(horizons)}."
        )

    logger.info(
        f"Model: d_model={d_model}, nhead={nhead}, nlayers={nlayers}, K={K}, "
        f"H={H_ck}, Q={Q_ck}, base_input_dim={base_input_dim}"
    )

    # 3) Build inference dataset with trend config from checkpoint
    ds = WindowedForecastDataset(
        df,
        input_len=input_len,
        horizons=horizons,
        feature_cols=("value",),
        target_col="value",
        normalize=not no_normalize,
        min_series_length=min_series_length,
        shared_normalizers=train_normalizers,
        allow_incomplete_targets=True,
        trend_config=trend_config,  # V2: from checkpoint
    )
    if len(ds) == 0:
        raise ValueError("No windows found for inference.")

    if train_normalizers is not None:
        matched = sum(1 for g in ds.group_to_series_id if g in train_normalizers)
        total = len(ds.group_to_series_id)
        logger.info(f"Normalization: {matched}/{total} series matched training normalizers")

    infer_loader = torch.utils.data.DataLoader(
        ds, batch_size=512, shuffle=False, num_workers=0, drop_last=False,
        collate_fn=lambda b: (
            torch.stack([x for x, _, _, _ in b]),
            torch.stack([ew for _, ew, _, _ in b]),
            torch.stack([y for _, _, y, _ in b]),
            [m for *_, m in b]
        )
    )

    # 4) Build model & load weights
    model = EpiweekFourierTransformerQuantile(
        base_input_dim=base_input_dim, H=H_ck, Q=Q_ck, K=K,
        d_model=d_model, nhead=nhead, nlayers=nlayers, dropout=0.0
    ).to(device)
    model.load_state_dict(model_state)
    model.eval()

    # 5) Forecast
    df_fore = make_forecasts(
        model, infer_loader, quantiles=list(qs),
        horizons=list(horizons), out_dir=out_dir, denormalize=True,
    )
    df_fore["reference_date"] = pd.to_datetime(df_fore["reference_date"])

    fc_path = out_dir / "forecasts.csv"
    df_fore.to_csv(fc_path, index=False)
    logger.info(f"Saved forecasts → {fc_path} ({len(df_fore)} rows)")

    if latest_only:
        idx = (
            df_fore.groupby(["disease", "grouping_var"])["reference_date"]
            .transform("max").eq(df_fore["reference_date"])
        )
        df_fore = df_fore[idx].reset_index(drop=True)

    return df_fore


#############
# CLI main
#############
def main():
    def _nearest_q_idx(qs: np.ndarray, q: float) -> int:
        return int(np.argmin(np.abs(qs - q)))

    def compute_wis_and_coverage_per_group(group_df, qs, alphas=(0.5, 0.2, 0.1)):
        y_vals = group_df["actual"].dropna().unique()
        if len(y_vals) != 1:
            return pd.Series({"valid": False})
        y = float(y_vals[0])
        qmap = dict(zip(group_df["quantile"].values, group_df["value"].values))
        if any(q not in qmap for q in qs):
            return pd.Series({"valid": False})
        q_preds = np.array([qmap[q] for q in qs], dtype=float)

        med_idx = _nearest_q_idx(qs, 0.5)
        median_term = 0.5 * abs(y - q_preds[med_idx])
        interval_term = 0.0
        for alpha in alphas:
            lo_q, hi_q = alpha / 2.0, 1.0 - alpha / 2.0
            lo_idx = _nearest_q_idx(qs, lo_q)
            hi_idx = _nearest_q_idx(qs, hi_q)
            l, u = q_preds[lo_idx], q_preds[hi_idx]
            IS = (u - l) + (2.0 / alpha) * (max(l - y, 0) + max(y - u, 0))
            interval_term += (alpha / 2.0) * IS
        wis = (median_term + interval_term) / (len(alphas) + 0.5)

        cov = {}
        for alpha in alphas:
            lo_q, hi_q = alpha / 2.0, 1.0 - alpha / 2.0
            l = q_preds[_nearest_q_idx(qs, lo_q)]
            u = q_preds[_nearest_q_idx(qs, hi_q)]
            cov[f"cov_{int((1 - alpha) * 100)}"] = float(l <= y <= u)
        return pd.Series({"valid": True, "wis": wis, **cov})

    # ---------- CLI ----------
    ap = argparse.ArgumentParser("Epiweek Inference V2 (Trend Features)")
    ap.add_argument("--data_folder", required=True)
    ap.add_argument("--checkpoint", required=True)
    ap.add_argument("--out_dir", default="./outputs_infer")
    ap.add_argument("--input_len", type=int, default=32)
    ap.add_argument("--horizons", type=int, nargs="+", default=[0, 1, 2, 3, 4])
    ap.add_argument("--no_normalize", action="store_true")
    ap.add_argument("--min_series_length", type=int, default=40)
    ap.add_argument("--device", default=None)
    ap.add_argument("--include_group_types", nargs="*", default=None)
    ap.add_argument("--include_diseases", nargs="*", default=None)
    ap.add_argument("--latest_only", action="store_true")
    ap.add_argument("--score", action="store_true")
    ap.add_argument("--coverage_alphas", type=float, nargs="*", default=[0.5, 0.2, 0.1])
    ap.add_argument("--year_min", type=int, default=None)
    ap.add_argument("--year_max", type=int, default=None)
    ap.add_argument("--unsafe_load", action="store_true")
    args = ap.parse_args()

    set_seed(42)
    device = args.device or ("cuda" if torch.cuda.is_available() else "cpu")
    logger.info(f"Inference device: {device}")

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    # 1) Load data
    loader = SurveillanceDataLoader(args.data_folder)
    df = loader.load_data(validate_filename_consistency=False)

    if args.include_group_types:
        df = df[df["grouping_var_type"].isin(args.include_group_types)]
    if args.include_diseases:
        df = df[df["disease"].isin(args.include_diseases)]
    if args.year_min is not None:
        df = df[df["year"] >= args.year_min]
    if args.year_max is not None:
        df = df[df["year"] <= args.year_max]

    req = {"disease", "grouping_var", "date", "epiweek", "year", "value"}
    missing = req.difference(df.columns)
    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    # 2) Load checkpoint
    state = _load_checkpoint(args.checkpoint, device, args.unsafe_load)
    qs, train_normalizers, trend_config, n_trend = _extract_checkpoint_metadata(state, args.no_normalize)
    model_state = state["model_state"]

    base_input_dim = 1 + n_trend

    H_ck, Q_ck, d_model, K, nhead, nlayers = infer_model_shapes_from_state(
        model_state, ckpt_quantiles_len=len(qs), base_input_dim=base_input_dim
    )

    if H_ck != len(args.horizons):
        raise ValueError(
            f"Horizons mismatch: checkpoint expects {H_ck}, got {len(args.horizons)}."
        )

    logger.info(
        f"Model: d_model={d_model}, nhead={nhead}, nlayers={nlayers}, K={K}, "
        f"H={H_ck}, Q={Q_ck}, base_input_dim={base_input_dim}"
    )

    # 3) Build inference dataset
    ds = WindowedForecastDataset(
        df,
        input_len=args.input_len,
        horizons=args.horizons,
        feature_cols=("value",),
        target_col="value",
        normalize=not args.no_normalize,
        min_series_length=args.min_series_length,
        shared_normalizers=train_normalizers,
        allow_incomplete_targets=True,
        trend_config=trend_config,  # V2
    )
    if len(ds) == 0:
        raise ValueError("No windows found for inference.")

    if train_normalizers is not None:
        matched = sum(1 for g in ds.group_to_series_id if g in train_normalizers)
        total = len(ds.group_to_series_id)
        logger.info(f"Normalization: {matched}/{total} series matched training normalizers")

    infer_loader = torch.utils.data.DataLoader(
        ds, batch_size=512, shuffle=False, num_workers=0, drop_last=False,
        collate_fn=lambda b: (
            torch.stack([x for x, _, _, _ in b]),
            torch.stack([ew for _, ew, _, _ in b]),
            torch.stack([y for _, _, y, _ in b]),
            [m for *_, m in b]
        )
    )

    # 4) Build model & load weights
    model = EpiweekFourierTransformerQuantile(
        base_input_dim=base_input_dim, H=H_ck, Q=Q_ck, K=K,
        d_model=d_model, nhead=nhead, nlayers=nlayers, dropout=0.0
    ).to(device)
    model.load_state_dict(model_state)
    model.eval()

    # 5) Forecast
    df_fore = make_forecasts(
        model, infer_loader, quantiles=list(qs),
        horizons=args.horizons, out_dir=out_dir, denormalize=True,
    )
    df_fore["reference_date"] = pd.to_datetime(df_fore["reference_date"])

    fc_path = out_dir / "forecasts.csv"

    if args.latest_only:
        idx = (
            df_fore.groupby(["disease", "grouping_var"])["reference_date"]
            .transform("max").eq(df_fore["reference_date"])
        )
        df_fore = df_fore[idx].reset_index(drop=True)

    df_fore.to_csv(fc_path, index=False)
    logger.info(f"Saved forecasts → {fc_path} ({len(df_fore)} rows)")
    logger.info("Inference complete.")

    # === Scoring ===
    if args.score:
        logger.info("Scoring forecasts against actuals...")

        truth = (df[["disease", "grouping_var", "date", "value"]]
                 .rename(columns={"date": "target_end_date", "value": "actual"})
                 .dropna(subset=["target_end_date", "actual"]))
        truth = (truth.sort_values(["disease", "grouping_var", "target_end_date"])
                      .drop_duplicates(subset=["disease", "grouping_var", "target_end_date"], keep="last"))

        fcast = pd.read_csv(fc_path, parse_dates=["reference_date", "target_end_date"])
        fcast["quantile"] = pd.to_numeric(fcast["quantile"], errors="coerce").round(6)
        fcast["value"]    = pd.to_numeric(fcast["value"], errors="coerce")

        merged = (fcast.merge(truth, on=["disease", "grouping_var", "target_end_date"], how="inner")
                       .dropna(subset=["actual", "quantile", "value"]))

        if merged.empty:
            logger.warning("No overlap between forecasts and actuals.")
            return

        qs_np = np.array(sorted(qs))
        alphas = tuple(sorted(args.coverage_alphas, reverse=False))
        key_cols = ["disease", "grouping_var", "reference_date", "horizon"]

        try:
            metrics = (
                merged.groupby(key_cols, group_keys=False)
                .apply(lambda g: compute_wis_and_coverage_per_group(g, qs_np, alphas), include_groups=False)
                .reset_index()
            )
        except TypeError:
            metrics = (
                merged.groupby(key_cols, group_keys=False)
                .apply(lambda g: compute_wis_and_coverage_per_group(g, qs_np, alphas))
                .reset_index()
            )

        if "valid" in metrics.columns:
            metrics = metrics[metrics["valid"] == True].drop(columns=["valid"])
        if metrics.empty:
            logger.warning("No valid metric rows.")
            return

        if "horizon" in metrics.columns:
            metrics["horizon"] = pd.to_numeric(metrics["horizon"], errors="coerce").astype("Int64")

        overall = {
            "n_forecasts": int(len(metrics)),
            "WIS_mean": float(metrics["wis"].mean()),
            "WIS_median": float(metrics["wis"].median()),
        }
        for alpha in alphas:
            cname = f"cov_{int((1 - alpha) * 100)}"
            if cname in metrics.columns:
                overall[cname] = float(metrics[cname].mean())

        cov_aggs = {
            f"cov_{int((1 - a) * 100)}": (f"cov_{int((1 - a) * 100)}", "mean")
            for a in alphas if f"cov_{int((1 - a) * 100)}" in metrics.columns
        }
        per_h = (
            metrics.groupby("horizon", dropna=False)
            .agg(WIS_mean=("wis", "mean"), WIS_median=("wis", "median"),
                 count=("wis", "size"), **cov_aggs)
            .reset_index().sort_values("horizon")
        )

        metrics.to_csv(out_dir / "scores.csv", index=False)
        with open(out_dir / "scores_overall.json", "w") as f:
            json.dump(overall, f, indent=2)
        per_h.to_csv(out_dir / "scores_by_horizon.csv", index=False)

        logger.info("=== OVERALL ===")
        logger.info(overall)
        logger.info("=== PER HORIZON ===")
        logger.info("\n" + per_h.to_string(index=False))


if __name__ == "__main__":
    main()