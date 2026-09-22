# Supporting Percentage Data Alongside Counts — Implementation Plan

Working note from a review of every model source file (`R/baseline-regular.R`,
`R/baseline-seasonal.R`, `R/copycat.R`, `R/CalCopycat.R`, `R/inla.R`,
`R/newGBQR_main_fxns.R` + `R/newGBQR_helper_fxns.R`, `R/parGBQR.R`,
`R/STArima.R`, `R/FourCAT.R`, `R/ensemble.R`), plus `R/data_utils.R`,
`R/report.R`, `R/plot.R`, and the server/UI wiring in `server/model_runs.R`,
`ui/tab_data.R`, `R/modal_registry.R`.

## Why this matters

Every model today is written assuming `value` is a raw case/hospitalization
**count**: floors at zero, log/sqrt/Box-Cox transforms with no upper bound,
Poisson noise, Poisson likelihoods, rounding to whole numbers. None of that
is valid for **percentage** data (test positivity, ED-visit share, bed
occupancy, etc.), which is bounded above at 100 and shouldn't be rounded to
an integer or modeled with count-specific noise. `validate_data()`'s error
text even says so explicitly today: *"Counts must be zero or positive."*

Two places in the codebase already anticipate this exact need:

- `R/copycat.R` has a literal `## Also need to add functionality for
  selecting count vs percentage forecasts` comment sitting in
  `fit_process_copycat()`.
- `R/inla.R`'s `forecast_samples_inla()` already branches on a `family`
  argument with a `beta` case (`rbeta(n, mu*phi, mu*(1-phi))`), and
  `fit_process_inla()` already threads a `family` parameter through to the
  `inla()` call — this looks like a half-finished start on exactly this
  feature. Nothing currently sets `family` to `"beta"` from the UI, and
  Poisson's `E = offset` (population exposure) term has no meaning for beta.

## Proposed design

### 1. New setting: Data Type

Add a new input on the **Data Upload & Settings** tab (`ui/tab_data.R`),
next to "Data to Drop" / "Local Seasonality":

```r
radioButtons(
  "data_type",
  label = tagList("Data Type", modal_info_link("modal_data_type")),
  choices = c("Counts" = "count", "Percentage" = "percentage"),
  selected = "count"   # backward-compatible default
)
```

- Add a `modal_data_type` row to `R/modal_registry.R` and a
  `www/content/modal-data-type.md` explaining the setting, matching every
  other input on that tab.
- **Fix the percentage scale at 0–100**, not 0–1, so it matches how
  `R/report.R` already formats percentages elsewhere (`sprintf("%+d%%", x)`)
  and how the modal/template will describe it. Document this explicitly —
  ambiguity between "45" and "0.45" is an easy source of silent bugs.
- **Population offset interaction**: INFLAenza's and newGBQR's "population"
  column/offset assumes `value` is a count to be divided or multiplied by a
  population. That's a different transform axis than "already a
  percentage." Recommend disabling/hiding `use_population_column` whenever
  `data_type == "percentage"` rather than trying to support both at once in
  the first pass.
- Store as `rv$data_type` (`server/init.R` / `server/data_upload.R`), and add
  the equivalent for the Retrospective tab's independently-uploaded dataset
  (mirroring how `retrospective_has_population()` already exists in
  `server/retrospective.R`).

### 2. Thread `data_type` through every model function

Every `fit_process_*()` already takes explicit settings like `seasonality`.
Add `data_type = c("count", "percentage")` the same way, defaulting to
`"count"` so existing callers/tests don't need to change. Wire
`server/model_runs.R`, `server/copycat.R`, `server/calcopycat.R`,
`server/newgbqr.R`, `server/pargbqr.R`, `server/starima.R`,
`server/inlaenza.R`, `server/fourcat.R`, and `server/retrospective.R` to pass
`input$data_type` (or `retrospective$data_type`) through, the same way
`input$seasonality` is passed today.

### 3. Centralize the safety net in `format_forecasts()`

`format_forecasts()` in `R/data_utils.R` is the one function every model's
output already passes through before it's stored/plotted/downloaded — the
natural single choke point for two cross-cutting fixes:

- **Range clipping**: keep `pmax(value, 0)` for counts (applied
  inconsistently today — some models clip internally, some don't — see
  per-model notes below), and add `pmin(value, 100)` whenever
  `data_type == "percentage"`. This guarantees no model can emit an
  impossible negative or >100% forecast even before its internal transform
  is fixed.
- **Rounding policy**: right now four different call sites round
  independently and none of them are data-type aware —
  `R/ensemble.R:101`, `R/FourCAT.R:305`, `R/validate_outside_model.R:188`,
  and `server/download.R:102` all call `round(value, 0)`. Replace all four
  with one shared helper (e.g. `finalize_forecast_values(df, data_type)`)
  that rounds to whole numbers for counts and to ~1–2 decimal places for
  percentages.

This phase alone — no model-internals changes — removes the most visibly
broken outputs (negative or >100% percentages, percentages rounded to whole
numbers) for every model at once, and is a good first PR before touching any
individual model's statistics.

### 4. Validation (`R/data_utils.R: validate_data()`)

Check 3b currently hardcodes count semantics:
*"The 'value' column contains negative values. Counts must be zero or
positive."* Branch this on `data_type`: keep the `>= 0` check and message
for counts, and add a `> 100` check with an updated message for percentages
("...Percentages must be between 0 and 100."). Apply the same branching in
`R/validate_outside_model.R` if the outside-model upload template is kept in
sync.

### 5. Reporting, plots, labels

- `R/plot.R`'s axis labels (`"Value"`, three call sites) are already
  data-type-agnostic; consider appending `(%)` when `data_type ==
  "percentage"` for clarity, but it's optional.
- `R/report.R`'s `.fmtL()` formats every number with `round(x)` plus a
  thousands separator — correct for counts, wrong for percentages (rounds
  away the meaningful decimal, and a thousands separator is pointless on a
  0–100 number). Add a percentage-aware formatter and pick between the two
  based on `data_type`. (Don't confuse this with `.pctlabL()`, which already
  exists in that file but formats *relative change* percentages, not a
  %-scale target variable — keep the naming distinct.)

### 6. Per-model statistical adjustments

Each model's core assumption is that `value` is an unbounded, non-negative
count. Below is what's specifically wrong for percentage data and the
proposed fix, ordered roughly by how much rework each needs.

**INFLAenza (`R/inla.R`) — smallest lift, do first.**
Already has a `family` parameter threaded through `fit_process_inla()` into
`inla()`, and `forecast_samples_inla()`'s `rfun` switch already has a `beta`
branch. Gaps to close:
1. Nothing sets `family` from the UI — wire `family = "poisson"` when
   `data_type == "count"`, `family = "beta"` when `"percentage"`.
2. INLA's beta family needs the response strictly inside `(0, 1)` — convert
   percentages to a fraction and nudge off the boundary, e.g.
   `pmin(pmax(value / 100, eps), 1 - eps)`.
3. `E = fit_df$offset` (the Poisson exposure term) has no meaning for beta —
   force `offset <- 1` / skip `use_offset` whenever `family == "beta"`.
4. Convert `rbeta()` draws back to the 0–100 scale before returning.

**Regular & Optimal Baseline (`R/baseline-regular.R`,
`fit_process_baseline_flat()`).** Transform is `sqrt(value + 1)` via
`simplets::fit_simple_ts()`, with `force_nonneg = TRUE` and a manual
`pmax(sim_matrix, 0)` floor — no ceiling anywhere. Stopgap: rely on the
phase-3 `format_forecasts()` clip. Statistically-correct fix: replace the
sqrt transform with a bounded one (logit of `value/100`) when
`data_type == "percentage"`, since sqrt won't reflect shrinking variance near
the 0/100 boundary the way logit does.

**Seasonal Baseline (`R/baseline-seasonal.R`,
`fit_process_baseline_seasonal()`).** Fits `log(count) ~ s(season_week,
bs="cc")` via `mgcv::gam`, simulates Gaussian noise on the log scale,
exponentiates back, floors at 0 (`pmax(x - 1, 0)`) — same unbounded-ceiling
problem. Fix: swap `log(count)` for a logit transform of `value/100` when
`data_type == "percentage"`, and back-transform with the inverse logit
instead of `exp()`.

**newGBQR (`R/newGBQR_main_fxns.R` / `R/newGBQR_helper_fxns.R`).** Already
applies a 4th-root, Freeman-Tukey-style transform (`inc_4rt <- (model_inc +
0.01 + 0.75^4)^0.25`) designed for open-ended non-negative rates, and its
inverse only floors at 0. For percentages, swap `inc_4rt`/its inverse for a
logit transform of `value/100` (keeping the existing
`inc_4rt_scale_factor`/`inc_4rt_center_factor` standardization step), and
clip the back-transform to `[0, 100]` instead of `[0, ∞)`. Also: the
existing `population`/`rate_per` pathway (divide by population, rescale to
per-100k) is a *different* concept (per-capita rate) than "already a
percentage" — force `uses_population <- FALSE` whenever `data_type ==
"percentage"`.

**parGBQR (`R/parGBQR.R`).** Reuses `wrangle_newgbqr_for_app()` and the same
`inc_4rt` feature/target pipeline and `process_and_combine_newgbqr_forecasts()`
output step as newGBQR rather than reimplementing them — so fixing newGBQR's
transform fixes parGBQR for free. Its residual-quantile calibration
(`pargbqr_residual_quantile_offsets()`) works purely in transformed-delta
space and needs no separate change.

**STArima (`R/STArima.R`).** Fits a Box-Cox transform (`guerrero` lambda) +
STL/ARIMA on the raw series, bootstraps residuals, and only floors the
simulated quantiles at 0 (`starima_quantiles_from_sims()`). Box-Cox has no
notion of an upper bound. Fix: when `data_type == "percentage"`, pre-transform
with `qlogis(value / 100)` instead of calling `box_cox()`, run the same
STL+ARIMA+bootstrap machinery on that unbounded logit series, and
`plogis()` the simulated quantiles back to 0–100 — this structurally
guarantees the ceiling instead of relying on a final clip.

**Copycat (`R/copycat.R`) — largest rework.** This is the model with the
TODO comment already in it. It works in log-growth-rate space (`log(lead(value
+ 1) / (value + 1))`) and adds `rpois()` observation noise — both are
count-specific. For percentages: (a) redefine growth as a logit-difference
(`qlogis(value/100)` step-change) so compounding trajectories can't blow
through the ceiling, and (b) replace Poisson noise with something
percentage-appropriate (Gaussian/beta noise on the logit scale, or simply
disable `add_poisson_noise` for percentage mode — CalCopycat already shows
this is a workable pattern, see below). This touches the trajectory database
construction itself (`get_seasonal_spline_vals()` builds the historical
database on the growth scale), not just the final noise step, so budget
real time for it.

**CalCopycat (`R/CalCopycat.R`).** Shares the same growth-trajectory core as
Copycat (including a duplicated `get_seasonal_spline_vals()`), but already
drops Poisson noise in favor of LOO calibration residuals — so the
noise-model concern doesn't apply here. It still has the same
`log(lead/lag)` unbounded-growth definition and needs the matching
logit-based rework once Copycat's lands. Worth doing both together and
de-duplicating the copy-pasted growth logic while you're in there.

**FourCAT (`R/FourCAT.R`) — not fixable with R-side changes.** A pretrained
Transformer checkpoint (`checkpoint_41/42/43.pt`) whose normalization,
seasonal embeddings, and output head are baked into the trained weights at
whatever scale its training data used. Recommend marking it **unsupported**
for `data_type == "percentage"` in the first release — exclude it from the
Development-tab model list / ensemble membership when percentage mode is
selected (it's already excluded from "Run All Default Models" per the
README, so this is a small, low-risk restriction) — and treat a checkpoint
retrain on percentage-scale series as a separate future project if there's
real demand for it.

**Ensemble (`R/ensemble.R`).** `hubEnsembles::simple_ensemble()` /
`linear_pool()` combine quantile levels and are agnostic to scale, so no
change needed there beyond dropping the hardcoded `round(value, 0)` (line
101) in favor of the shared data-type-aware rounding from phase 3. Note: until
every member model is converted, an ensemble mixing an already-bounded
member with a not-yet-converted one could still slip outside `[0, 100]` in
edge cases — the `format_forecasts()` clip is the backstop during that
transition.

### 7. Retrospective tab & tests

- `server/retrospective.R` calls into the same `fit_process_*()` functions
  for historical reference weeks — once each accepts `data_type`, thread
  `retrospective$data_type` through the same call sites that already thread
  `retrospective_has_population()`.
- `run_tests.R` / `tests/testthat`: add a small synthetic percentage fixture
  (e.g. a test-positivity series bounded 0–100) alongside the existing count
  fixtures, and add a regression test asserting no model can emit a quantile
  below 0 or above 100 in percentage mode.
- Update the README's Data Format table and the downloadable CSV template
  (`ui/tab_data.R`'s `template_choice`) — both currently describe `value` as
  "Case count or hospitalizations for that week."

### 8. Suggested rollout order

Given the size of this change, land it in phases and backtest each one
retrospectively before it affects real-time forecasting output — same
convention already used in `gbqr_improvement_priorities.md`.

1. **Phase 0 (plumbing, ships first, low risk):** `data_type` setting + UI +
   modal, threaded as a parameter everywhere (default `"count"`, so nothing
   changes for existing users), centralized clip + rounding in
   `format_forecasts()`, updated `validate_data()`, updated README/template.
2. **Phase 1:** INFLAenza beta-family wiring (closest to already built).
3. **Phase 2:** Baselines (regular / optimal / seasonal) transform swap.
4. **Phase 3:** newGBQR + parGBQR transform swap (shared code — one change
   covers both models).
5. **Phase 4:** STArima logit pre-transform.
6. **Phase 5:** Copycat + CalCopycat growth-model rework (largest effort;
   touches the trajectory database).
7. **Phase 6:** Decide FourCAT's fate (exclude from percentage mode vs.
   scope a retrain).
