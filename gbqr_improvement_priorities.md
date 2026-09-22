# GBQR (parGBQR / newGBQR) Improvement Priorities

Working note from a code review of `R/newGBQR_main_fxns.R`, `R/newGBQR_helper_fxns.R`,
and `R/parGBQR.R`. Both models share the same feature pipeline, transform, and
season-block bagging mechanism (parGBQR calls newGBQR's own wrangling/feature/output
functions directly); they differ mainly in how the quantile spread is produced —
newGBQR fits one LightGBM model per quantile per bag, parGBQR fits one median model
per bag and derives the rest of the quantiles from that model's own residuals.

Goal: work through this list with retrospective backtests before changing what
runs in real-time forecasting.

## Tier 1 — fix before trusting any retrospective comparison

1. **parGBQR's residual calibration uses in-sample (in-bag) residuals.**
   `run_pargbqr_multi_group()` / `run_pargbqr_global()` predict each bag's residuals
   on the same rows (`bag_obs_inds`) that bag was just trained on, which will make
   its intervals look narrower/sharper than they really are. Fix: compute residuals
   on the seasons that bag *excluded* (out-of-bag) instead. Cheap, contained change
   (a handful of lines in both functions) — but needs to happen before any
   retrospective WIS/coverage comparison involving parGBQR is trustworthy, since
   right now it's structurally favored on interval width for the wrong reason.

   **Fixed.** Both functions now predict calibration residuals on each bag's
   held-out seasons, falling back to in-bag rows only when a bag's season sample
   happens to cover every season (e.g. `bag_frac_samples = 1`, nothing left out).
   Covered by three new tests in `tests/testthat/test-pargbqr.R` proving, per bag,
   that the calibration rows are disjoint from the training rows and their union
   covers the full dataset (individual and global modes), plus the fallback case.
   Full suite verified green (56/56 assertions, including the two end-to-end
   schema/monotonicity/median-match tests) against the real project source files.
   parGBQR's interval width is no longer structurally favored — it's now safe to
   run a retrospective WIS/coverage comparison against newGBQR.

2. **Neither model has any validation-based tuning of `nrounds`.** Both call
   `lgb.train()` with a flat `nrounds = 100` (hardcoded at the server call site in
   `server/model_runs.R`, for every location/group/quantile) and no `valids` /
   `early_stopping_rounds`. Given `num_leaves = 11` and `min_data_in_leaf = 8` are
   already conservative, this is probably the single biggest source of avoidable
   error in the pipeline today, since nothing currently steers it per group.

## Tier 2 — high expected value, test once comparisons are trustworthy

3. **Season-bagging diversity with few historical seasons.** With
   `bag_frac_samples = 0.7` and, say, 4 seasons on hand, there are only
   `choose(4,3) = 4` distinct subsets — 50 bags mostly redraw the same few season
   combinations with a new LightGBM seed. Likely means both models' intervals are
   narrower than they should be, system-wide (not just parGBQR). Worth testing
   whether fewer bags, a different `bag_frac_samples`, or systematic season-subset
   enumeration changes calibration.

4. **Broader hyperparameter tuning** (`learning_rate`, `num_leaves`,
   `min_data_in_leaf`, `feature_fraction`) beyond just `nrounds`. Real, but likely
   smaller marginal gains than #2 since current defaults are already reasonably
   conservative.

## Tier 3 — plausible, more speculative or bigger lift for less certain payoff

5. **Horizon-specific vs. horizon-pooled models.** Right now one model per
   group/bag/(quantile) is shared across all horizons, differentiated only by
   `horizon`/`horizon_sq`/`horizon_x_level`/`horizon_x_vol_sd4` features. A fully
   separate model per horizon might not buy much beyond what those interaction
   terms already capture, and it's expensive (multiplies newGBQR's fit count by
   the number of horizons).

6. **`peak_week_method` alternatives.** The function signature already supports
   `"empirical"`, `"zone"`, `"fixed"`, but every caller hardcodes `"empirical"`.
   Matters mainly for groups with too little history for a reliable empirical
   peak estimate.

## Tier 4 — lowest priority

7. **Quantile-crossing fix.** newGBQR currently patches crossing quantiles with a
   post-hoc row-wise sort (`t(apply(test_pred_qs, 1, sort))`) rather than a
   monotonicity-constrained or composite quantile method. Correctness nicety more
   than a likely accuracy lever — crossing is probably rare enough between
   adjacent quantiles that it's not moving WIS much.

## What's cheap to turn into a retrospective-testable parameter right now

`bag_frac_samples`, `nrounds`, `learning_rate`, `num_leaves`, `min_data_in_leaf`,
`feature_fraction`, and `peak_week_method` are **already function arguments** in
`fit_process_newgbqr()` / `fit_process_pargbqr()` — none of them are wired into
`retrospective_parameter_specs()` today (only `model_type` and `num_bags` are), so
none of them can currently be swept retrospectively even though the code already
supports different values. Adding all seven to the spec and threading them through
the retrospective runner calls is low-risk, mechanical work (same pattern as
Copycat's `max_matches`) and unlocks:
- grid-searching `nrounds` (50/100/150/200) as a cheap stand-in for real early
  stopping,
- sweeping `bag_frac_samples`/`num_bags` together to see whether the bagging-
  diversity concern (#3) actually shows up in the numbers,
- testing `peak_week_method` alternatives (#6) with zero new modeling code.

**One step up in cost:** parGBQR's residual source (in-bag vs. out-of-bag, #1
above) needs a small code change first (predict on the excluded seasons instead
of the trained-on ones), but once written is trivial to expose as
`residual_source = c("in_bag", "out_of_bag")` and let the retrospective tab settle
which one calibrates better.

**Not parameterizable without new modeling code first:** real early-stopping with
a genuine validation split (vs. the cheap `nrounds` grid-search substitute above),
horizon-specific model fitting (#5), and a monotonicity-constrained quantile
method (#7). Hold off on building these until the cheap sweeps above show whether
they're worth it.

## Suggested next step

Start with the mechanical part: wire `bag_frac_samples`, `nrounds`,
`learning_rate`, `num_leaves`, `min_data_in_leaf`, `feature_fraction`, and
`peak_week_method` into both models' retrospective parameter specs (zero new
modeling code, unlocks most of Tier 1 and 2 for testing). Then tackle the
parGBQR in-bag/out-of-bag residual fix as a second, slightly bigger step.
