# INFLAenza + Cross-Model Implementation Plan

Companion to `inflaenza_improvement_priorities.md`. That document argued *what* to
build; this one is *how*, against the actual code. Four workstreams:

1. Neighbor-graph CSV upload, used by INFLAenza when present, falling back to
   `exchangeable` when absent
2. New INFLAenza model options, all testable in the retrospective tab
3. Max-value censoring, user-selectable at 1.1× – 2.0× of each region's observed
   maximum, off by default
4. A coherent strategy for proportion vs count at the aggregate ("Overall") level
   across all models

Decisions already taken: censoring is a **post-hoc clip** (not a censored likelihood),
computed from a **flat all-time max per target group**. Plan only — no code written yet.

---

## 0. Two findings that shape everything below

**The retrospective tab already supports multiple configurations of the same model in
one run.** `retrospective$configs` is a table of run configs — one row per
(model_id, params) — and a single "Run Retrospective" fits every row
(`R/retrospective.R:2432-2483`). The "Add Combination" button appends a row
(`server/retrospective.R:1088-1138`), and tests already exercise three Copycat configs
in one run (`tests/testthat/test-retrospective.R:669`). Adding runs is incremental:
`add_retrospective_run_configs()` (`R/retrospective.R:3293+`) fits only new run_ids and
reuses prior forecasts.

This means the requirement "any options for individual models must be testable in
retrospective" is nearly free. The per-model settings UI is entirely generated from
`retrospective_parameter_specs()` (`R/retrospective.R:29-188`) — `ui/tab_retrospective.R`
contains **no per-model code at all**, just 9 lines of generic scaffolding
(`:112-141`). Adding an option is three edits, none of them UI:

1. `R/retrospective.R:32-45` — add a spec to the `inla` block
2. `R/retrospective.R:2192-2205` — pass `params$<new>` into the runner's `fit_process_inla()` call
3. `R/inla.R:233` — add the argument to `fit_process_inla()`

**The models return quantiles, not samples.** This is the binding constraint on
workstream 4 and is why only INFLAenza aggregates today. Covered in §4.

---

## 1. Neighbor-graph CSV upload

### 1.1 Fix target-group ordering first — this is a prerequisite, not a detail

There is **no canonical ordering of target groups anywhere in the app**. Three
different orders coexist today:

- `R/inla.R:106` — `group_idx = as.numeric(fct_inorder(target_group))`, i.e. order of
  first appearance in `fcast_data()`'s rows
- `server/init.R:147-151` — `target_groups()` uses `distinct(target_group)` on
  `rv$raw_data`, a *different* data frame, with nothing asserting the two agree
- `R/STArima.R:31` and every `facet_wrap(~target_group)` in `R/plot.R` — alphabetical

Nobody has had to care because `control.group = list(model="exchangeable")` is
permutation-invariant. The moment `group_idx` indexes a besag adjacency matrix it
becomes load-bearing, and a mismatch produces a clean fit with wrong forecasts and no
error. This is the single highest-risk part of the whole plan.

Two options:

- **(a) Reorder at point of use.** Validate the uploaded matrix by name, then permute
  its rows/columns to `levels(fct_inorder(df$target_group))` inside `prep_data_inla()`.
  Contained, changes nothing else, but leaves the three-orders situation in place for
  the next person.
- **(b) Introduce one canonical order.** Add a `target_group_levels()` helper beside
  `target_groups()` in `server/init.R` and convert `target_group` to a factor with
  those levels once, in `read_raw_data()` (`R/data_utils.R:40`). Correct, but it
  changes facet order in every existing plot and report.

**Recommendation: (a) now, (b) as a separate follow-up.** Do the permutation in one
function with a test that deliberately feeds a shuffled matrix and asserts the fit
either reorders correctly or refuses. Do *not* trust `target_groups()` order.

### 1.2 File contract

A square CSV, first column and header row both being target group labels:

```
,Adult,Pediatric,Senior
Adult,0,1,1
Pediatric,1,0,0
Senior,1,0,0
```

Validation rules, all blocking unless noted:

| Check | Rule |
|---|---|
| Square | `nrow == ncol` after dropping the label column |
| Labels match | Row labels == column labels, as sorted sets |
| Groups match data | Set equality against `target_groups()`, both directions |
| Numeric | All cells finite and non-negative |
| Symmetric | `W == t(W)` within tolerance |
| Zero diagonal | `diag(W) == 0` |
| No all-zero row | **Warning, not error** — an isolated region is legitimate (Puerto Rico in the flusight model needs `insert_iso_loc()`), but is usually a typo |
| Connectedness | **Warning** — report the number of disconnected components |

The "groups match data" check is a direct copy of Check 5 in
`R/validate_outside_model.R:81-98`, which already does sorted-set `setdiff` in both
directions and echoes the expected set in the error message. Reuse that wording.

Row/column ordering in the file is **not** a validation concern — the file is keyed by
name and permuted at point of use per §1.1. That is the point of accepting names rather
than a bare matrix.

### 1.3 Upload path

The right precedent is **not** the population column (which was folded into the main
CSV — the abandoned `validate_population()` is still commented out at
`R/data_utils.R:388-406`, with the vestigial `rv$valid_pop` slot at `server/init.R:15`).
It is the **outside-model upload** in the ensemble tab, the only true auxiliary-file
upload in the app.

Files to add or touch:

- **`R/validate_neighbor_graph.R`** (new). Follow the `validate_outside_model()`
  contract — `list(errors=, warnings=, data=)`, where `data` is a cleaned numeric
  matrix with dimnames, or NULL. This is the right idiom here (as opposed to
  `validate_data()`'s errors-only contract) precisely because you need to return a
  coerced object, not just a verdict. `source()` it in `app.R` near `:55`.

- **`ui/tab_data.R`** — a `fileInput("neighbor_graph_file")` plus
  `uiOutput("neighbor_graph_status_ui")` and a template `downloadButton`, in that order,
  mirroring `ui/tab_ensemble.R:46-62`.

- **`server/data_upload.R`** — `observeEvent(input$neighbor_graph_file, ...)`.
  **Use the `renderUI`-from-state feedback pattern** (`server/retrospective.R:991-1013`),
  not the Data tab's `insertUI` into `#error_message`. `insertUI` accumulates across
  uploads and is not idempotent under re-render; the graph will be re-validated whenever
  the main dataset changes, so state-driven rendering is the correct choice. Keep the
  Bootstrap `alert alert-*` classes and FontAwesome icons for visual consistency with
  the rest of that tab.

- **Template generated from live data**, following `server/ensemble.R:16-36`. Emit an
  N×N grid of zeros with the actual uploaded `target_group` names as row and column
  labels, gated on `req(rv$raw_data)`. This removes the entire class of "group names
  don't match" errors before it exists, and is the single highest-value piece of
  polish in this workstream.

- **`server/init.R`** — add `neighbor_graph` and `neighbor_graph_validation` to
  `reactiveValues` (`:4-29`); clear both in `reset_forecast_state()` (`:95-145`).

  **Critical:** a new main-data upload can change the target-group set, so it must
  invalidate the graph. Either clear it outright or re-run validation against the new
  groups. The precedent for re-validation on a settings change is the
  `observeEvent(data_type(), ...)` block at `server/data_upload.R:224-272`.

- **Gating** — extend the `observe()` at `server/data_upload.R:380-423`. The pattern to
  copy is the population gate at `:400`: enable the spatial interaction choice only when
  a valid graph is loaded, and force the selection back to `exchangeable` otherwise.

- **`R/modal_registry.R` + `www/content/modal-neighbor-graph.md`** if the control gets a
  help link. `modal_info_link()` hard-errors at source time on an unregistered id
  (`R/ui_helpers.R:39-53`), so this is not optional if you add the link.

### 1.4 Model wiring and fallback

In `fit_process_inla()` (`R/inla.R:233`), add `neighbor_graph = NULL`. The fallback the
user asked for falls out of the formula generator (§2): resolve the interaction term as

```
if (is.null(neighbor_graph))  interaction <- "exchangeable"   # or the user's choice
else                          interaction <- user's choice, besagproper now available
```

Make the fallback **visible, not silent**. If a user explicitly selects `besagproper`
and no graph is loaded, that should surface as a notification, not a quiet downgrade to
`exchangeable` — a silent fallback in a retrospective run would produce a row labelled
`besagproper` that wasn't one. Given the gating in §1.3 the UI shouldn't allow it, but
the retrospective path takes params programmatically and bypasses UI gating entirely.

### 1.5 The retrospective-scope problem

The retrospective tab has its **own** data upload (`retrospective$raw_data`,
`server/retrospective.R:3+`), separate from `rv$raw_data`. So:

- The graph must be reachable from both scopes, or uploaded separately in each.
- Worse: when the retrospective CSV has a `retrospective_group` column (multiple
  countries, each with their own target groups), **one graph cannot serve all groups**.

Options, in increasing order of effort:

- **Restrict:** allow `besagproper` in retrospective runs only when there is no
  `retrospective_group` column. Refuse with a clear message otherwise. Cheapest, and
  honest.
- **Per-group graphs:** accept a named list of matrices keyed by retrospective group.
  Means a multi-file upload or a long-format edge-list CSV with a `retrospective_group`
  column, which is arguably a nicer input format anyway.
- **Defer:** ship the graph for the live tabs only in v1, and add retrospective support
  once the single-group case is proven.

**Recommendation: restrict in v1.** Note that this partly undercuts "testable in
retrospective" for multi-country users — worth deciding explicitly rather than
discovering later.

Threading: the runner closures capture `settings` and `data_type` when the registry is
built (`R/retrospective.R:2384-2387`). The graph must be captured the same way —
`retrospective_model_runners(settings, data_type, neighbor_graph = NULL)` — not passed
through `params`, since `params` is serialized into run labels and run ids
(`retrospective_make_run_id()`, `:241-264`) and a matrix has no sensible string form.
What *does* belong in params is a boolean or factor recording *that* a graph was used.

### 1.6 Build order

1. Ordering fix + test with a deliberately shuffled matrix (§1.1)
2. `R/validate_neighbor_graph.R` + unit tests, no UI
3. `fit_process_inla(neighbor_graph=)` + formula generator support
4. UI, template generation, gating
5. Retrospective threading with the multi-group restriction

Steps 1–3 are testable without any Shiny involvement and are where the risk is.

---

## 2. New INFLAenza options

### 2.1 Which ones

Six new specs in `retrospective_parameter_specs()$inla` (`R/retrospective.R:32-45`),
chosen for forecast impact per the priorities doc:

| Param | Type | Choices / range | Default | Rationale |
|---|---|---|---|---|
| `temporal` | choice | `ar1`, `rw1`, `rw2`, `none` | `ar1` | Biggest lever on 3–4 wk shape: ar1 mean-reverts, rw1 persists level, rw2 extrapolates trend |
| `interaction` | choice | `exchangeable`, `iid`, `none`, `besagproper` | `exchangeable` | The §1 payoff; `besagproper` gated on a graph |
| `seasonal` | choice | `shared`, `group`, `none` | `shared` | `none` matters for <2 seasons of history; `group` is what flusight does via `season_group` |
| `holiday` | logical | — | `FALSE` | Christmas-distance rw1; date-derived, no new user input |
| `pc_prior_seasonal` | numeric | 0.01–5, step 0.05 | 1 | Currently nailed to `pc.prec(1, 0.01)` with no way to change it |
| `pc_prior_short` | numeric | 0.01–5, step 0.05 | 1 | Replaces the 3 presets; production uses 0.2 (paper RSV) and 0.1 (RSV_2025), neither reachable today |

All six use widget types that `retrospective_parameter_input_widget()`
(`R/retrospective.R:371-416`) already handles. **Do not invent a new spec type** — that
function's `if/else if` chain has no terminal `else`, so an unrecognized type renders
as NULL silently, and `retrospective_validate_model_params()` (`:266-354`) has the same
gap.

### 2.2 Backward compatibility on the uncertainty knob

`forecast_uncertainty` (default/small/tiny → u of 1/0.2/0.05) is the existing control
and appears in saved retrospective run configs. Replacing it outright breaks
`retrospective_run_configs_from_metadata()` (`R/retrospective.R:706-739`) when loading a
previous run. Keep it, and treat the new numeric as an override:

```
u_short <- if (!is.null(params$pc_prior_short)) params$pc_prior_short
           else switch(params$forecast_uncertainty, default=1, small=0.2, tiny=0.05)
```

Same for the seasonal side, which has no existing preset. Worth noting the
commented-out older code already had a `seasonal_smoothness` switch
(default/less/more → 0.5/0.25/1) that was dropped — the shape of this is half-written.

### 2.3 The label problem

Model identity in retrospective results is carried by `run_label`, in a column literally
named `model` (`R/retrospective.R:1037-1043`); forecasts carry no `run_id`. Auto-labels
are built from all params (`retrospective_make_run_label()`, `:230-239`), so with six
new options the default label becomes:

```
INFLAenza (forecast_uncertainty=default; use_offset=false; temporal=ar1;
interaction=exchangeable; seasonal=shared; holiday=false; pc_prior_seasonal=1; ...)
```

Score plots wrap labels at width 14 (`retrospective_wrap_labels()`, `:1376`). With
several variants the plots become unreadable.

Two fixes, do both:

- Add an optional `abbrev` field to each spec and have `retrospective_params_label()`
  use it when present (`temporal` → `t`, `interaction` → `int`), yielding
  `INFLAenza (t=rw2; int=besagproper)`.
- Better: have the label include **only params that differ from the defaults**. A
  default-configuration run then labels as plain `INFLAenza`, and variants name only
  what changed. This is a change to `retrospective_params_label()` (`:215-228`) that
  benefits every model, not just INLA.

Two constraints to respect: `retrospective_validate_run_configs()` (`:459-483`) hard-errors
on duplicate run labels, and `is_retrospective_baseline_model()` (`:1061-1063`) is
`grepl("Baseline", ...)` — so never emit "Baseline" in an INLA label.

### 2.4 Optional: a real sweep

No cross-product helper exists anywhere in the app; "Add Combination" appends one row
per click, so testing 4 temporal × 3 interaction means 12 clicks. If that becomes
annoying, the clean insertion point is a generator producing N rows in the same
5-column shape (`run_id, model_id, model_label, run_label, params`) bind_rows'd at
`server/retrospective.R:1131`. Everything downstream — scoring, labelling, incremental
add, failure isolation — already handles N configs per model. Guard each generated
label through `make_unique_retrospective_label()` (`:565-582`), and check the run-size
estimator at `server/retrospective.R:1224+`, since a sweep multiplies fit counts fast
and INLA is single-threaded (`inla.setOption(num.threads="1:1")`, `R/inla.R:2`).

Treat this as optional polish, not part of the core work.

---

## 3. Max-value censoring

### 3.1 The clip on quantiles is exact, not an approximation

Worth stating up front because it determines where this goes. Since `pmin(x, c)` is
monotone non-decreasing, quantiles commute with it:

```
Q_{min(X,c)}(p) = min(Q_X(p), c)
```

So clipping the **quantile table** gives numerically identical results to clipping
**samples** and recomputing quantiles. There is no reason to push this down into each
model's sampler, and no accuracy cost to applying it centrally. It also means the
feature works uniformly for Poisson and Beta models, for the sample-based INLA path and
the quantile-only paths alike, and for uploaded outside models.

The visible consequence is that upper quantiles pile up at the cap — several quantile
levels reporting the identical value. That is correct behavior for a censored forecast,
but it will look odd in plots and will flatten WIS differences among clipped variants.
Say so in the help modal.

### 3.2 Where it hooks

There are four parallel output funnels, and `format_forecasts()` is **not** the
universal one:

1. `format_forecasts()` — `R/data_utils.R:451-509`, all 10 live models
2. `format_retrospective_forecasts()` — `R/retrospective.R:1037-1059`, a near-duplicate
   with different horizon math; the retrospective pipeline never calls `format_forecasts()`
3. `build_ensemble()` — `R/ensemble.R:102`
4. `validate_outside_model()` — `R/validate_outside_model.R:196`

All four end in `finalize_forecast_value()` (`R/data_utils.R:411-424`), which is the
true choke point — but it takes a bare numeric vector and cannot see `target_group`, and
the cap is per-group.

**Plan:** add a helper that takes a formatted forecast tibble plus a per-group cap table
and returns the clipped tibble, then call it in (1) and (2). Signature roughly:

```r
apply_forecast_cap <- function(forecast_df, cap_table, multiplier = NULL)
```

with `cap_table` a two-column tibble `(target_group, cap)`. Call it immediately before
the `finalize_forecast_value()` step, so rounding still happens last. Add to (3) and (4)
only if you want uploaded outside models capped too — arguably yes for ensemble
coherence, but it is a defensible v2.

A cleaner long-term move is unifying (1) and (2), which are near-duplicates that have
already drifted. Out of scope here, but every cross-cutting change pays this tax twice.

### 3.3 Computing the cap

Flat all-time max per target group, from the training data actually used for that fit:

```r
cap_table <- df |>
  filter(!is.na(value)) |>
  group_by(target_group) |>
  summarise(cap = multiplier * max(value, na.rm = TRUE), .groups = "drop")
```

Details that matter:

- Compute from **training data only**, never the full series. In a retrospective run the
  cap must reflect what was known at that reference date, or the backtest leaks future
  information and the feature scores better than it deserves. This is the single most
  important correctness point in this workstream — compute it inside the per-reference-date
  loop, from the same `train_data` handed to the runner.
- **Groups with no observations** (all NA) get no cap — leave uncapped rather than
  capping at `-Inf`.
- **Proportions** are already bounded above by 1 in `finalize_forecast_value()`. A
  multiplier cap still does useful work below 1, so apply it the same way; just ensure
  `min(cap, 1)` for proportion data.
- **The "Overall" group** gets its cap from its own observed history, which exists in
  the data for every model. For INLA's derived aggregate, the summed samples should be
  capped against Overall's own history too — but see §4, because this is exactly where
  the aggregate's provenance gets confusing.

### 3.4 UI

Live tabs: this is a **cross-model** setting, so it belongs in the "Settings for All
Models" block of `ui/tab_data.R` (alongside `data_type`, `forecast_date`,
`data_to_drop`), not on the INFLAenza tab.

```r
selectInput(
  "forecast_cap_multiplier",
  label = tagList("Cap forecasts at multiple of observed max",
                  modal_info_link("modal_forecast_cap")),
  choices = c("No censoring" = "none", "1.1x" = "1.1", "1.2x" = "1.2",
              "1.3x" = "1.3", "1.4x" = "1.4", "1.5x" = "1.5",
              "1.6x" = "1.6", "1.7x" = "1.7", "1.8x" = "1.8",
              "1.9x" = "1.9", "2x" = "2.0"),
  selected = "none"
)
```

Retrospective: because it is cross-model rather than per-model, it does **not** fit the
`retrospective_parameter_specs()` shape cleanly — those are per-model. Two options:

- **(a)** Add it as a top-level retrospective setting beside `retrospective_data_type`
  (`server/retrospective.R:85-87`), applied uniformly to every config in the run.
  Simpler, but means you cannot compare capped vs uncapped in a single run — you'd run
  the retrospective twice.
- **(b)** Add an identical `cap_multiplier` spec to **every** model's spec block, so it
  rides the existing per-config machinery and capped/uncapped variants coexist in one
  run and score side by side.

**Recommendation: (b).** The whole point of the feature is to find out whether capping
helps, and (b) makes that a single run with two rows in the config table. The cost is
one duplicated spec entry per model, which is mechanical. It also means the cap shows up
in run labels automatically, which is what you want for reading a score plot.

Note the interaction with §2.3: with default-only labelling, an uncapped run stays
`INFLAenza` and the capped one becomes `INFLAenza (cap=1.5)`. That reads well.

### 3.5 Tests

- Clipping a quantile table equals clipping samples then re-quantiling (verifies the
  §3.1 identity numerically on a small case)
- Monotonicity across quantile levels is preserved after clipping
- Cap computed from training data only — construct a series whose post-reference-date
  values exceed the pre-reference max, and assert the cap ignores them
- `"none"` is a strict no-op: output byte-identical to the uncapped path
- A group with all-NA values passes through uncapped
- Proportion data never produces a cap above 1

---

## 4. Proportion vs count at the aggregate level

### 4.1 What's actually true today

- **Only INFLAenza aggregates.** `aggregate_forecast_inla()` (`R/inla.R:200-210`) sums
  *samples* and then quantiles them — which is correct. Every other model treats
  `"Overall"` as just another `target_group` row present in the upload and fits it
  independently.
- **INLA silently changes its output shape by data type.** `pred_agg_group` is ANDed
  with `identical(family, "poisson")` at `R/inla.R:264`, so in proportion mode "Overall"
  stays in the training data and is fit as an independent Beta series. INFLAenza is the
  only model whose target-group set depends on `data_type`.
- **`agg_group = "Overall"` is a default argument never passed by any caller** —
  not by `server/model_runs.R:270`, not by `R/retrospective.R:2196`. The aggregate group
  name is not configurable from anywhere.
- **No model sums quantiles across groups**, so there's no statistical error to fix. The
  risk is the opposite: for 9 of 10 models the "Overall" forecast is an independent fit
  that is *not* coherent with the sum of its parts.
- **There is an orphaned detector for exactly this.** `check_overall_completeness()`
  (`R/data_utils.R:101-125`) checks whether component groups sum to Overall and returns
  `"single_target"` / `"aggregate"`. It is wired to `overall_type` at
  `server/init.R:177-180` and **referenced nowhere else in the repo**. This is the hook a
  unified strategy wants, already written.

### 4.2 The real blocker

Generalizing INLA's approach to the other models is not a small change, because
**`fit_process_*()` returns quantiles, not samples**. Aggregating quantiles is wrong —
the sum of per-group quantiles is not the quantile of the sum, except under perfect rank
correlation. So there is no correct way to derive an aggregate for the other nine models
without changing their return contract.

That is the architectural decision hiding in this question: either models optionally
return samples, or aggregation stays INLA-only.

### 4.3 Options

**Option A — independent fits everywhere.** Drop INLA's aggregation; let Overall be an
independently fit series for all 10 models. Consistent and trivial. Cost: throws away
INLA's one genuinely correct aggregate, and Overall won't equal the sum of parts for
anyone.

**Option B — samples everywhere.** Add an optional `return_samples` to each
`fit_process_*()` and aggregate centrally. Statistically right, and would also give
sample-based ensembling. But it touches all nine models, several of which
(FourCAT shells out to a Python CLI; STArima goes through `fable`) do not naturally
expose a sample matrix. Large.

**Option C — declared capability, honest defaults.** Keep sample-based aggregation for
models that can produce samples (today: INLA only), fit Overall independently for the
rest, and make which one happened **visible** rather than implicit. Concretely:

- Add an `aggregates = TRUE/FALSE` field to the model registry entries in
  `retrospective_model_runners()` (`R/retrospective.R:2155-2304`) and to the live
  wrappers.
- Use the orphaned `overall_type` reactive to decide whether aggregation is even
  meaningful for the uploaded data — if `check_overall_completeness()` says the groups
  don't sum to Overall, aggregation should be off for everyone, including INLA, because
  the data says Overall isn't an aggregate.
- Make `agg_group` an actual parameter rather than an unreachable default, so a dataset
  using "Total" or "National" works.
- For proportions, replace the current blanket disable with a **population-weighted
  mean** when a population column exists, and an unweighted mean otherwise (the
  reference does exactly this: `aggregate_forecast(pred_samp, fun=mean, ...)` in the
  flusight ED script). Document the unweighted case as an approximation.

**Recommendation: Option C.** It fixes the genuine bug (proportions get no aggregate at
all), removes the silent shape change, and makes the capability legible — without
committing to rewriting nine models' return contracts. Option B is the right end state
if sample-based ensembling ever becomes a goal; it should be its own project.

### 4.4 Known adjacent bugs worth folding in

Two items from `count_vs_percentage_data_plan.md` that are still open and will confuse
any aggregate work:

- **FourCAT is broken in proportion mode.** `data_type` is a declared no-op
  (`R/FourCAT.R:234-240`) and it hardcodes `round(value, 0)` at `:305-306`, so every
  proportion forecast is rounded to 0 or 1 before `format_forecasts()` sees it.
  `finalize_forecast_value()` cannot undo this. Phase 6 of that plan (exclude FourCAT
  from proportion mode) was never done. Either exclude it from the model list when
  `data_type == "proportion"`, or fix the rounding.
- **Copycat's growth transform isn't proportion-aware.** It still uses
  `log(lead(v+1)/(v+1))` with a `+1` shift (`R/copycat.R:48-49`), where CalCopycat was
  updated to a `1e-4` shift (`R/CalCopycat.R:286-312`). Phase 5 is half-done.

Neither blocks this plan, but both will produce confusing retrospective scores in
proportion mode and should be fixed before anyone reads a proportion comparison.

### 4.5 A structural limit worth knowing

`data_type` has three scopes — live global (`server/init.R:182-188`), retrospective
(`server/retrospective.R:85-87`), and per-`retrospective_group`
(`server/retrospective.R:276-287`) — but **nothing is per-`target_group`**. You cannot
express "Adults and Pediatrics are counts, Overall is a proportion", or the reverse.
If the aggregate strategy ever needs mixed scales within one series, that's a schema
change, not a settings change.

---

## 5. Sequencing

| Order | Work | Why here |
|---|---|---|
| 1 | Target-group ordering fix (§1.1) | Prerequisite for the graph; silent-wrongness risk |
| 2 | Max-value censoring (§3) | Self-contained, cross-model, no dependencies, immediately useful |
| 3 | Formula generator + the 6 options, minus `besagproper` (§2) | Unblocks retrospective comparison of everything except spatial |
| 4 | Label improvements (§2.3) | Needed before the sweeps in 3 are readable |
| 5 | Neighbor graph: validator → model → UI (§1.2–1.4) | The main event; rests on 1 and 3 |
| 6 | Aggregate strategy, Option C (§4) | Independent; do after the FourCAT/Copycat fixes in §4.4 |
| 7 | Retrospective graph threading (§1.5) | Only after single-group graph fits are proven |

Items 2 and 3 are independent of each other and could go in parallel.

---

## 6. Open questions

1. **Retrospective + multi-country graphs (§1.5).** Restrict `besagproper` to
   single-group retrospective runs in v1, or invest in per-group graphs up front? This
   affects whether multi-country users can backtest the headline feature at all.
2. **Cap as per-model spec vs global setting (§3.4).** Recommendation is per-model
   (duplicated across all 10 spec blocks) so capped/uncapped compare in one run. Confirm
   the duplication is acceptable.
3. **Canonical target-group ordering (§1.1 option b).** Worth doing properly now, given
   it changes facet order in every existing plot and report? Or patch locally and defer?
4. **Aggregate for proportions (§4.3).** Population-weighted mean when population
   exists — confirm that's the intended semantics, and what should happen when only
   some groups have population values.
5. **FourCAT in proportion mode (§4.4).** Exclude it from the model list, or fix the
   rounding? Exclusion is a one-line guard; fixing means touching the Python side.
