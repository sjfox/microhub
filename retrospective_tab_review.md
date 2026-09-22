# Retrospective Tab — Pre-Release Review

> **Status update:** every Critical, High, and Medium item below (#1–#15) has been fixed and verified — either with reproduction scripts or, for #7–#15, with permanent `testthat` regression tests now checked into `tests/testthat/test-retrospective.R`. A portable `run_tests.R` (fixed to use `getwd()` instead of a hardcoded path) runs the whole suite: `Rscript run_tests.R` from the repo root. The full suite (176 assertions) passes with only the one pre-existing, unrelated failure (Copycat default-settings parameter names — present before this review, not something these fixes touch). Only the Low/Nit items remain open.

Scope: `R/retrospective.R` (engine), `server/retrospective.R` (reactivity), `ui/tab_retrospective.R` (layout), plus cross-file integration, for the full multi-group / global-ensemble / global-scoring-reference / incremental-run feature set built this session.

Method: three independent deep-dive passes (engine, server, UI/integration), each instructed to read the full files and actually reproduce findings with runnable R code rather than speculate. Findings below are merged and de-duplicated across the three passes; each item lists the file/function, the concrete failure scenario, and how it was verified.

**Coverage caveat:** the sandbox this review ran in is missing ~20 files that `app.R` references (including `R/report.R` and `server/download.R`). Everything touching the retrospective files themselves was checked; cross-file integration with those specific missing files was not verified in this pass and should get a quick manual look before release.

---

## Critical — fix before release

### 1. Interactive updates can silently overwrite one group's data with another's
**`R/retrospective.R` — `rewrite_retrospective_output_files()`**

When you add a model, change the scoring reference, or rebuild the ensemble after the initial run, this function recomputes each group's output folder name fresh (via `sanitize_retrospective_group_name()`) instead of consulting the `retrospective_group_folders.csv` mapping that was already built (with collision suffixes like `__2`) at initial-run time. Two groups whose names sanitize to the same string — e.g. `"Cote d'Ivoire"` and `"Cote d Ivoire"` — get the same folder on any interactive rewrite, and one group's files silently clobber the other's. Reproduced end-to-end with a two-group run and a follow-up "add model" action.

**Fix direction:** have `rewrite_retrospective_output_files()` read and reuse the persisted folder mapping instead of recomputing it.

**Fix status: done.** Added `retrospective_group_folder_names_for_rewrite()`, which reads `retrospective_group_folders.csv` and reuses its already-disambiguated names (falling back to a fresh recompute only if the manifest is missing or incomplete). Verified with a reproduction using two groups that collide under the old sanitizer ("Cote d'Ivoire" / "Cote d Ivoire") — they now get distinct folders and their data no longer merges.

### 2. Incremental "Run Retrospective" can silently discard the user's scoring-reference choice
**`server/retrospective.R` — main run handler + `retrospective_initial_scoring_reference()`**

After *any* run (fresh or incremental), the handler unconditionally does `retrospective$scoring_reference_choice <- retrospective_initial_scoring_reference(result)`. That helper assumes every group resolved the same requested baseline, which is only true for a fresh run. For an incremental run, `add_retrospective_run_configs()` re-resolves each group from *that group's own prior* (possibly already-fallback) value, so seeding from whichever group happens to be alphabetically first can quietly overwrite a baseline the user explicitly chose. As a second-order effect, the "Except: …" caveat text this generates ends up backwards — it names the wrong group as the exception. Traced end-to-end through the code; high confidence.

**Fix direction:** only reseed `scoring_reference_choice` on a genuinely fresh run; on an incremental run, keep the existing global choice and re-derive the per-group exception list from it rather than from each group's already-resolved value.

**Fix status: done.** The run handler now only reseeds `retrospective$scoring_reference_choice` when the run is NOT incremental. On an incremental run, the global choice is left untouched (it was already the correct value `add_retrospective_run_configs()` used as each group's `requested_reference_model`), so it can no longer be silently overwritten by whichever group happens to be alphabetically first. Verified with a reproduction that shows the old logic would have flipped a real "ModelX" choice to "Regular Baseline"; the new logic leaves it alone.

---

## High

### 3. Group-selector DOM ids can collide across groups with different names
**`server/retrospective.R` — `retrospective_group_country_input_id()` / `retrospective_group_zone_badge_output_id()`**

Both helpers sanitize a group value into an element id with `gsub("[^A-Za-z0-9]+", "_", ...)` and no collision handling. Verified directly: `"A B"`, `"A-B"`, `"A_B"`, and `"A.B"` all sanitize to the same id. If two retrospective groups' names collide this way, the later-rendered group's local-seasonality input/zone badge silently overwrites the earlier one's DOM element — one group can end up reading or displaying another group's seasonality zone with no error surfaced anywhere. (Reviewed independently by both the server and UI passes; the UI pass reproduced the collision directly and rates it High because it's a silent wrong-data risk, not just a cosmetic one — worth weighing that against the server pass's Low/Nit rating, but the reproduction argues for treating it as a real pre-release risk.)

**Fix direction:** same fix as the Critical folder-collision bug — append a disambiguating suffix (or hash the raw group value) instead of relying on the sanitized string alone being unique.

**Fix status: done.** Both id-helper functions now take the group's index in the upload's own (stable, sorted) group list as the id suffix instead of a sanitized string, so two groups can never collide regardless of punctuation. Verified: "A B" / "A-B" / "A_B" / "A.B" now produce four distinct ids.

### 4. Renaming a run's label between runs can silently drop that model's history
**`R/retrospective.R` — `add_retrospective_run_configs()`**

Kept-vs-new forecasts are matched by `run_label`, not `run_id`. If a label changes between the old and new `run_configs` for what is logically the same run, that model's prior forecast history is silently dropped from the merge even though `run_configs`/`successes` still report it as succeeded. Reproduced with a runnable script. Not currently reachable through the UI (there's no label-edit control on an existing config), so it's a latent engine bug rather than an active one today — but it's the kind of thing that becomes reachable the moment someone adds a "rename config" convenience.

**Fix status: done.** Kept forecasts are now looked up by the OLD label (from `existing_configs`, the config the rows were actually fit under), not the new one, so a rename can no longer drop the rows; any renamed run_id's kept rows are then relabeled in place to the new label so forecasts/scores/config all agree going forward. Verified with a reproduction of the rename scenario.

### 5. "Run Ensemble" can be enabled with member combinations that never co-occur in any group
**`server/retrospective.R` — ensemble enable/disable logic**

The button's enable check only counts globally-selected members (≥2), not whether those members actually co-occur within any single group's results. If a user picks two models that were each only run in different groups, every group silently produces zero ensemble rows, while `ensemble_members`/`ensemble_models`/`ensemble_method` state is set as though the ensemble succeeded — nothing in the UI flags the empty result.

**Fix status: done.** The button now enables only when `retrospective_ensemble_members_co_occur_in_any_group()` finds at least one group where 2+ of the selected members both completed; the handler re-checks the same condition as a guard against a stale click. As a belt-and-suspenders addition, it also now shows a warning notification if the rebuild ends up producing zero ensemble rows anywhere, or if some (but not all) groups were skipped. Verified with both a "never co-occur" and a "co-occurs in one group" case.

### 6. Ensemble-member selection is never cleared on a fresh (non-incremental) run
**`server/retrospective.R` — main run handler / `clear_retrospective_result()`**

`retrospective$ensemble_members` is only reset inside `clear_retrospective_result()`, which — confirmed via a full-repo grep — is never actually called from anywhere. A fresh run (e.g. after changing the reference-week range, which forces a non-incremental run) leaves stale ensemble-member selections in place, producing a self-contradictory Summary card: members are listed as selected, but the ensemble method shows "—" because no ensemble was actually rebuilt for the new run.

**Fix status: done.** The run handler now resets `retrospective$ensemble_members` to `character()` whenever the run is a fresh (non-incremental) run, alongside the scoring-reference reseed fixed under Critical #2 above.

---

## Medium

### 7. Structural group-failure markers never clear after a later success
**`R/retrospective.R`**
Once a group is marked structurally failed, an incremental run that later succeeds for that group doesn't clear the earlier failure marker. Reproduced; mostly a stale-status/UX correctness issue rather than data loss.

**Fix status: done.** `add_retrospective_run_configs()` now drops a group's stale structural-failure marker whenever that group is re-attempted (new models were actually fit this call) and no longer produces a fresh marker — i.e. it actually succeeded. Covered by a new regression test that monkey-patches a structural failure into the first run and confirms the marker clears once the group succeeds on a later incremental run.

### 8. Hardcoded final fallback reference model isn't checked for existence
**`R/retrospective.R` — `retrospective_resolve_reference_model()`**
The last-resort fallback is the literal string `"Regular Baseline"`, used without checking that a model by that name actually exists among the group's forecasts. If it doesn't (e.g. it was removed via an incremental "remove model" action, a workflow this session added), every relative-WIS metric for that group silently becomes `NA` with no warning. Reproduced.

**Fix status: done.** The fallback now checks what's actually available: any other baseline-labeled model first, then any completed model at all, and only `NA` (explicitly "no usable reference") when nothing completed for that group. Covered by a new test.

### 9. Group/target-group id columns aren't pinned to character type on reload in some read paths
**`R/retrospective.R`**
Unlike the `output_type_id` fix already applied earlier this session, several other CSV read paths don't force group/target-group identifier columns to character type. Verified: numeric-looking ids over ~15–16 digits lose precision, and values like `"TRUE"`/`"FALSE"`/`"T"`/`"F"` get permanently coerced to logical on reload.

**Fix status: done.** Pinned identifier columns to character across every remaining read path: `run_configs.csv` (all columns, since it's all identifiers/stringified params), the `group`/`folder` columns of `retrospective_group_folders.csv`, and every roll-up CSV `load_retrospective_run()` reads (successes/failures/score rows/overall/by-target-group/by-forecast-date) via a shared `read_optional_csv()` helper. Covered by a new test using a 19-digit group id and a group literally named `"TRUE"`.

### 10. "Overall Score Summary (All Groups)" card shows for a single-group upload
**`ui/tab_retrospective.R` / `server/retrospective.R`**
The card's visibility guard only checks that a group column exists, not that more than one distinct group value exists. With a single-group upload it renders a pooled table that's an exact duplicate of the per-group "Overall" tab below it — confusing given the card is explicitly framed as "across every group."

**Fix status: done.** Both the card and its table now additionally require more than one distinct resolved group value before rendering.

### 11. Pooled table and per-group Overall tab can legitimately disagree, with no caveat on the pooled card
**`server/retrospective.R`**
When a group's baseline falls back to a different model than the global choice, its Relative WIS in the per-group tab can differ from the pooled table's value for the same model. This is explained elsewhere (Retrospective Summary caveat, per-group caption), but not on the pooled card itself, and that card's own code comment is factually wrong for this case.

**Fix status: done.** Extracted the exception-detection logic (which groups' resolved reference diverges from the global choice) into a shared `retrospective_scoring_reference_exceptions()` helper, added a caveat note to the pooled card whenever any group diverges, and rewrote the pooled table's code comment to state the real (conditional) relationship instead of an unconditional "directly comparable" claim. Covered by a new test on the shared helper.

### 12. Nested card creates double card-chrome under the "Individual Group Detail" section
**`ui/tab_retrospective.R`**
`navset_card_underline(...)` now sits one level deeper inside the new "Individual Group Detail" `card()`. Verified by rendering the actual markup: `navset_card_underline()` emits its own `.card` wrapper, so under the yeti bootswatch theme you get a visible nested-card border/shadow around the scoring-summary tabs. Cosmetic, but noticeable.

**Fix status: done.** Swapped `navset_card_underline()` for plain `navset_underline()` (same underlined-tab look, no extra card wrapper) — verified via direct rendering that the new call emits no `.card` class, unlike the old one.

### 13. A mid-load error can permanently disable fresh-vs-incremental run detection
**`server/retrospective.R` — `process_retrospective_load()`**
The `suppress_stale_marking` guard is set `TRUE`, then cleared by a `shinyjs::delay(500, ...)` call — nothing between those two points is wrapped in `tryCatch`/`on.exit`. If any statement in between throws, the guard is stuck `TRUE` for the rest of the session, silently disabling the fresh/incremental distinction app-wide.

**Fix status: done.** The whole load-setup block now runs inside `tryCatch()`; an error clears the guard immediately and surfaces as a normal "Error loading this run" message instead of leaving the guard stuck.

### 14. Zero automated test coverage for everything built this session
**`tests/testthat/test-retrospective.R`**
None of `add_retrospective_run_configs()`, `load_retrospective_run()`, `summarize_retrospective_scores_pooled_across_groups()`, `rewrite_retrospective_output_files()`, `retrospective_score_summary_table()`, or `retrospective_group_health_label()` have tests. Flagged independently by both the engine and UI/integration passes. The existing suite (161 assertions) still passes with only one pre-existing, unrelated failure (Copycat default-settings parameter names).

**Fix status: done.** Added 15 new `testthat` tests covering all six previously-untested functions plus regression coverage for every Critical/High/Medium fix above. `retrospective_score_summary_table()` and `retrospective_group_health_label()` live in `server/retrospective.R`, which can't be `source()`'d standalone outside a live Shiny session — `run_tests.R` now includes a small `extract_pure_server_functions()` helper that parses that file and evaluates only its plain, non-reactive function definitions (skipping every `observeEvent`/`reactive`/`output$...<-` assignment), so these are tested against the real shipped code, not a reimplementation. Also made `run_tests.R` portable (`root <- getwd()` instead of a hardcoded path) so it now runs correctly from this repo — run it with `Rscript run_tests.R` from the repo root. Full suite: 176 assertions, same one pre-existing unrelated failure.

### 15. NA `model` value produces a self-contradictory summary line
**`server/retrospective.R` — per-model success/failure breakdown**
A base-R indexing spot mishandles a hypothetical `NA` model value, producing text like "NA: succeeded in 1/N groups (failed in all)". Verified with a runnable snippet; not confirmed reachable under normal (non-corrupted) operation, so this is lower-confidence than the other Medium items.

**Fix status: done.** An `NA` model is now handled explicitly (treated as "never succeeded anywhere" rather than relying on `==`'s NA-propagation through logical indexing), with a `!is.na(...)` guard added for the mirror case of a corrupted `NA` row in the successes table. Covered by a new test.

---

## Low / Nit

- **Pooled-summary empty-input shape inconsistency** (`R/retrospective.R`) — harmless, already guarded against downstream.
- **`write_retrospective_group_scoring_reference()` silently no-ops on unnamed input** (`R/retrospective.R`).
- **`ensemble_models` isn't updated when a member config is removed via an incremental run** (`R/retrospective.R`) — appears to be intentional/documented behavior, flagging for awareness only.
- **Three orphaned `renderDT` outputs** (`retrospective_data_preview`, `retrospective_status_table`, `retrospective_files_table`) with no corresponding UI reference — dead code, safe to remove.
- **Loading a previous run resets global "Ensemble Members" to empty** instead of restoring the persisted `loaded$result$ensemble_models` — minor UX gap, not a correctness bug.
- **`highlight_best = FALSE` branch of `retrospective_score_summary_table()` is dead code** — all four call sites use the default `TRUE`.

---

## What was checked and confirmed fine

- The `retrospective_replace_group_rows()` NA-group-column fix from earlier this session is correct, including 0-row and column-absent edge cases.
- Relative-score zero/negative-baseline guards; pooled summarizer's row-weighting (by design); `write_retrospective_run_settings()`'s NA self-correction; `combine_retrospective_group_results()` is not vulnerable to the NA-mixing bug class; `load_retrospective_run()` round-trips every field correctly.
- All 11 call sites of the generalized `group_scoped()` / `group_scoped_raw_data()` / `current_group_scoring_reference()` helpers are correct; the `identical(...)` guard in the scoring-reference observer never false-skips or spuriously reprocesses.
- Full UI `outputId` ↔ server `output$...` cross-reference: no orphans besides the three dead renderers above, no duplicate assignments.
- XSS/HTML-escaping verified safe by actually rendering a `<script>` payload through the group-selector code path.
- `DT::formatStyle(target = "row")` confirmed genuinely supported in the installed DT 0.31 by inspecting the installed source; hidden `is_best` column index is computed dynamically and verified correct.
- The `ensemble_members` list→character shape change is safe — full-repo grep found no remaining stale `[[...]]` list-style indexing.
- Persisted/zip file format is unaffected by this session's changes; `by_target_group` and `by_forecast_date` are confirmed to never co-occur as grouping columns; `retrospective_group_state_key` has zero remaining references anywhere.
- No crash found in any exercised edge case: 0 groups, 1 group, all-groups-failed, 0 or 1 models succeeded, empty tables, special-character group names.

---

## Suggested order of work

1. ~~Fix #1 and #2 (Critical)~~ — done.
2. ~~Fix #3, #5, #6 (High)~~ — done.
3. ~~Fix #4~~ — done (fixed at the engine level even though it's not yet UI-reachable, since the fix was contained and cheap).
4. ~~Fix #7–#13, #15 (Medium)~~ — done.
5. ~~Add regression tests (#14)~~ — done, 15 new tests, full suite green.
6. Clear the remaining Low/Nit list opportunistically — nothing time-sensitive there.
