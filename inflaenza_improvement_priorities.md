# INFLAenza Improvement Priorities

Working note from a comparison of the microhub INFLAenza implementation
(`R/inla.R`, driven from `server/model_runs.R` and `R/retrospective.R`, exposed in
`ui/tab_inlaenza.R`) against `brendandaisy/inla-forecasting-paper` — both its
library code (`src/model-formulas.R`, `src/fit-inla-model.R`, `src/prep-fit-data.R`,
`src/sample-forecasts.R`) and, importantly, the production scripts that actually
run each week (`scripts/flusight-25-26/inflaenza-forecast.R`,
`scripts/metrocast-25-26/run-metrocast.R`, `scripts/rsvnet-24-25/`).

The production scripts matter as much as `src/` here: several of the features with
the highest apparent forecast value (the Christmas effect, group-specific
seasonality, regime-shift covariates) are written inline in those scripts and never
made it back into `model_formula()`. A straight `src/`-to-microhub diff would miss
them.

Goal: work through this list with retrospective backtests before changing what runs
in real-time forecasting.

## Where the two implementations stand

microhub's `fit_process_inla()` has exactly two formulas, chosen automatically by
whether there is more than one `target_group`:

```r
# single group
value ~ 1 + f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model=TRUE)
          + f(t, model="ar1", hyper=hyper_wk)

# multi group
value ~ 1 + target_group
          + f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model=TRUE)
          + f(t, model="ar1", hyper=hyper_wk, group=group_idx,
              control.group=list(model="exchangeable"))
```

Two knobs reach the user: `forecast_uncertainty` (default / small / tiny → PC prior
`u` of 1 / 0.2 / 0.05 on the short-term effect) and `use_population_column` (yes/no).
Family is Poisson, or Beta when `data_type` is `"proportion"`. Everything else —
seasonal prior, temporal model, group-interaction structure, sample count, covariates
— is fixed in code.

The reference `model_formula()` is a generator over four axes: `covars` (arbitrary),
`seasonal` (none / shared / iid), `temporal` (none / ar1 / rw1 / rw2), and `spatial`,
which is really the group-interaction structure (none / iid / exchangeable /
besagproper). `fit_inla_model()` takes `pc_prior_u` as a length-2 vector — separate
scales for the seasonal and short-term effects — plus a `...` passthrough to `inla()`
that the production scripts use for `control.family`, `dic` and `config`.

So the headline difference is not any single missing effect. It is that microhub
exposes 2 fixed model structures where the reference spans roughly 4 × 4 × 3 plus
covariates, and microhub can express only one point in that space (shared seasonal /
ar1 / exchangeable) — with a prior combination the reference's own RSV production runs
do not use (`pc_prior_u=c(1, 0.2)` is not reachable from the three presets).

## Tier 1 — correctness and robustness, fix before comparing anything

1. **NAs inside the training window become phantom forecast rows.**
   `fit_process_inla()` sets `pred_idx <- which(is.na(fit_df$value))` and carries the
   code comment "TODO: currently assumes value has no NAs in the input df". It does
   not. A user uploading a series with one missing week gets that row selected for
   joint prediction, given a negative `horizon` by `forecast_samples_inla()`, and
   emitted into the quantile table alongside the real horizons. The reference guards
   this two ways: `fit_inla_model()` accepts a `forecast_date` and intersects
   (`is.na(count) & date > forecast_date`), and the flusight script pre-filters with
   `filter(!(is.na(count) & date <= forecast_date))`. Adopt the filter — it is one
   line and it removes a whole class of silent garbage output.

2. **Beta zero-handling is a clamp, not a censor.** microhub nudges proportions with
   `pmin(pmax(value, 1e-4), 1 - 1e-4)`. The reference instead uses INLA's native
   censoring, `control.family = list(beta.censor.value = cens)` with
   `cens = min(positive values)/2`, in both the flusight ED-visit and metrocast
   scripts. Two problems with the clamp: it asserts a specific likelihood
   contribution at exactly the boundary observations rather than treating them as
   "below detection", and `1e-4` is an absolute constant that is simply the wrong
   scale for a metro-level series whose typical values are ~1e-5, or for data
   supplied on a 0–100 percent scale. Switching to a data-derived censor value is a
   small change and strictly better founded.

3. **Aggregation to "Overall" is disabled for proportions.** microhub sets
   `pred_agg_group <- pred_agg_group & identical(family, "poisson")`, so a user
   forecasting proportions gets no aggregate series at all. The comment is right that
   summing proportions is meaningless, but the reference does aggregate them — with
   `fun=mean` (`aggregate_forecast(pred_samp, fun=mean, ...)` in the flusight ED
   script). A population-weighted mean when a population column is present, falling
   back to an unweighted mean, restores the feature correctly rather than dropping it.

4. **`wrangle_inla_population()` / `wrangle_inla_no_population()` are dead and
   broken.** Both call `add_population_column(pop_table, ...)` and
   `prep_fit_data_population()`. The only definitions of those are commented out at
   the bottom of `R/inla.R`; the live `add_population_column()` in
   `R/global-data-processing.R` has a different signature. Nothing calls them today,
   so this is latent rather than active — but they will error the moment someone wires
   them up. Delete them or repair them, and while there, note that they also hardcode
   `filter(year > 2021)`, which is a flu-specific assumption with no business being in
   a general tool.

## Tier 2 — the options actually worth adding, highest value first

5. **Holiday / distance-to-Christmas effect.** `f(ixmas, model="rw1",
   hyper=hyper_epwk, scale.model=TRUE)`, where `ixmas` indexes a six-level factor from
   `dist_xmas()` ("2 weeks before" … "week of" … "2 weeks after", "none"). This is in
   every current flusight production run and is *not* in `model_formula()`. Reporting
   and care-seeking artifacts around Christmas and New Year are the largest recurring
   structural error in respiratory forecasts in that window, and the effect is purely
   date-derived — no new user input required, just `dist_xmas()` ported into
   `prep_data_inla()` and a checkbox. Best value-to-effort ratio on this list.

6. **Temporal model choice: `ar1` / `rw1` / `rw2` / `none`.** microhub hardcodes
   `ar1`. This is the single biggest lever on 3–4 week-ahead behavior: `ar1`
   mean-reverts toward the seasonal baseline, `rw1` persists the current level, `rw2`
   extrapolates the current *trend*. Which one wins is genuinely data-dependent, which
   is exactly why it should be a control rather than a decision baked in. Note the
   reference's `scale.model=TRUE` applies to rw1/rw2 but not ar1 — copy that
   conditional from `model_formula()` rather than reinventing it.

7. **Group-interaction structure: `exchangeable` / `iid` / `none`.** microhub
   hardcodes `exchangeable` whenever there is more than one group. `exchangeable`
   assumes a common correlation between every pair of groups' short-term deviations —
   reasonable for age strata, often wrong for heterogeneous geographies or epizones,
   where `iid` (independent group dynamics, pooled only through the prior) or `none`
   (one shared trend, groups differing by intercept only) fits better. Cheap to add
   once the formula generator exists.

8. **Seasonal structure: `shared` / group-specific (`iid`) / `none`.** Also hardcoded
   to `shared`. Both alternatives matter:
   - `none` is important for short series. With under ~2 seasons of history the
     cyclic rw2 on `epiweek` is barely identified and can inject seasonality that the
     data does not support — the reference runs an explicit "no seasonal" variant in
     `retrospective-new.R` for exactly this comparison.
   - group-specific seasonality (`group=..., control.group=list(model="iid")`) is what
     the flusight script uses via a hand-built `season_group` that separates AK, HI and
     PR from the contiguous states. The generalization for microhub is to let the user
     choose whether seasonality is shared across target groups or estimated per group.

9. **Separate prior knob for the seasonal effect.** microhub exposes only `hyper_wk`;
   `hyper_epwk` is nailed to `pc.prec(1, 0.01)`. The reference's `pc_prior_u=c(u_seas,
   u_short)` tunes both, and its RSV production runs use `c(1, 0.2)`. Worth noting the
   commented-out older microhub code already had a `seasonal_smoothness` switch
   (default / less / more → 0.5 / 0.25 / 1) that was dropped somewhere along the way —
   this is half-written already.

10. **Make the uncertainty scale continuous rather than three presets.** The presets
    span `u` ∈ {1, 0.2, 0.05} and cannot go *above* 1, which is the direction you want
    for volatile or sparsely reported series. A numeric input (log-spaced slider, say
    0.01–5) covers the presets and more. Pair it with #9 so the two effects are set
    independently.

**Suggested first slice:** items 5–10 are essentially one piece of work — port a
`model_formula()`-style generator into `R/inla.R` to replace the two hardcoded
strings, then add four controls to `ui/tab_inlaenza.R` (temporal model, seasonality,
group interaction, holiday effect) and split the uncertainty control into seasonal and
short-term. The retrospective harness already passes a `params` list through
`R/retrospective.R:2193`, so the new options get backtestable for free as soon as
they are arguments to `fit_process_inla()`.

## Tier 3 — larger lift, or payoff contingent on user data

11. **Arbitrary fixed-effect covariates.** The reference passes `covars` freely, and
    production adds `early_covid` (a binary pre-2022-09-01 indicator absorbing the
    COVID-era regime shift) alongside location fixed effects. For microhub the general
    version is: let a user designate an uploaded column as a fixed effect. The
    narrower, more immediately useful version is a built-in "regime change before date
    X" indicator, which is what `early_covid` actually is and which would let users
    keep older history instead of truncating it.

12. **Spatial / adjacency structure (`besagproper`).** The reference author's inline
    note says besagproper was faster than besag *and* gave slightly smaller prediction
    intervals for COVID, and it is what the current flusight model uses. But microhub's
    data contract is `date` / `target_group` / `value` (+ optional population) with no
    geography and no adjacency graph, so this needs an entirely new input path — a
    shapefile or neighbor-matrix upload, plus `sf`/`spdep` dependencies, plus the
    `insert_iso_loc()` handling for locations absent from the base map. Only pays off
    for users whose target groups are genuinely places. Defer until someone asks.

13. **Per-group independent fitting as an option and as a fallback.** The reference's
    `forecast_baseline()` and metrocast's `pred_state_metro()` fit each group
    separately and bind the results. Two uses in microhub: a true "no pooling"
    comparator, and a recovery path when the joint fit fails or diverges.

14. **Exposure beyond raw population.** The flusight pipeline builds
    `pop_weighted = population * pct_reporting` and averages `ex_lam` over recent weeks
    (`pct_reporting_weeks`) rather than taking the single most recent value, which is
    what microhub's `prep_data_inla()` does via `slice_max(date)`. Matters whenever
    reporting coverage moves — which for hospitalization data it does.

15. **Divergence handling.** `retrospective-new.R` is revealing: the authors run the
    flu no-spatial model twice and take the better result per forecast date, then drop
    scores above WIS 1000 as unrecoverable, and separately drop a diverging RSV
    "no seasonal" forecast. INLA does fail on these models often enough that the paper
    authors built manual workarounds. microhub has no retry and no sanity check. A
    cheap guard — if the predicted median exceeds some multiple of the historical
    maximum, refit once with different initial values, then fall back to a simpler
    formula — would matter most in the retrospective sweeps, where one divergent fit
    silently poisons a model's average score.

16. **Threading and parallelism.** `R/inla.R` sets `inla.setOption(num.threads="1:1")`
    globally and nothing in microhub runs fits in parallel; the reference runs
    retrospectives under `future::plan(multicore, workers=4)`. Single-threading is
    plausibly a deliberate Shiny-safety choice for interactive single fits, but the
    retrospective path fits (forecast dates × groups) models serially at one thread
    each, which is the worst case. Worth revisiting for that path specifically.

## Verification plan

Nothing here should be adopted on argument alone. The retrospective module already
supports per-model `params`, so the natural test is a sweep over the Tier 2 axes —
temporal ∈ {ar1, rw1, rw2} × seasonal ∈ {shared, group, none} × interaction ∈
{exchangeable, iid, none}, with and without the holiday term — scored by WIS and
50/95% coverage on the KDCA data and at least one proportion-type dataset, since
Poisson and Beta paths may well prefer different structures. The reference's own
finding is worth keeping in view as a sanity check: in their retrospectives the full
model beat both "no seasonal" and "no spatial" on flu, RSV and COVID, so a sweep here
that shows the current hardcoded structure winning everywhere is a plausible outcome —
in which case the value of this work is the holiday effect, the Tier 1 fixes, and
having the comparison on record rather than assumed.
