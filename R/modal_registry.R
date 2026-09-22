# Modal registry ===============================================================
# Single source of truth for every help/methodology modal in the app: one row
# per input id, giving its dialog title and its content file under
# www/content/<md>.md. server/modals.R and R/ui_helpers.R (modal_info_link())
# both read this table instead of hand-typing the id/title/file combination,
# so a UI trigger can never point at a server handler for a different id, or
# at a missing/renamed content file, without the stopifnot() below catching it
# at source() time. That class of bug (a modal with no handler, or a
# methodology link pointed at the wrong content file) has happened before in
# this app and was previously only caught by manual audit.
#
# To add a new modal: add one row here. Nothing else needs to change —
# server/modals.R generates its observer from this table automatically.

modal_registry <- tibble::tribble(
  ~id,                                   ~title,                            ~md,
  "modal_template",                      "Target Data",                     "modal-template",
  "modal_retrospective_template",        "Retrospective Target Data",       "modal-retrospective-template",
  "modal_forecast_date",                 "Forecast Date",                   "modal-forecast-date",
  "modal_data_drop",                     "Data to Drop",                    "modal-data-drop",
  "modal_seasonality",                   "Seasonality",                     "modal-seasonality",
  "modal_forecast_horizon",              "Forecast Horizon (Weeks)",        "modal-forecast-horizon",
  "modal_data_type",                      "Data Type",                       "modal-data-type",
  "modal_baseline_regular_methodology",  "Regular Baseline Methodology",    "baseline-regular",
  "modal_baseline_seasonal_methodology", "Seasonal Baseline Methodology",   "baseline-seasonal",
  "modal_baseline_opt_methodology",      "Opt Baseline Methodology",        "baseline-opt",
  "modal_inla_methodology",              "INFLAenza Methodology",           "modal-inflaenza",
  "modal_forecast_uncertainty",          "Forecast Uncertainty",            "modal-forecast-uncertainty",
  "modal_population",                    "Population offset",              "modal-population",
  "modal_inla_interaction",              "Group structure",                "modal-inla-interaction",
  "modal_inla_seasonal",                 "Seasonality",                    "modal-inla-seasonal",
  "modal_ensemble_methodology",          "Ensemble Methodology",            "ensemble",
  "modal_newgbqr_methodology",           "newGBQR Methodology",             "newgbqr",
  "modal_newgbqr_model_type",            "Model Fitting",                   "modal-gbqr-model-type",
  "modal_pargbqr_model_type",            "Model Fitting",                   "modal-gbqr-model-type",
  "modal_pargbqr_methodology",           "parGBQR Methodology",             "pargbqr",
  "modal_starima_methodology",           "STArima Methodology",             "starima",
  "modal_copycat_methodology",           "Copycat Methodology",             "modal-copycat",
  "modal_copycat_weight_exponent",        "Weight Exponent",                "modal-copycat-weight-exponent",
  "modal_copycat_poisson_noise",          "Add Poisson Noise",              "modal-copycat-poisson-noise",
  "modal_copycat_noise_dispersion",       "Noise Dispersion",                "modal-copycat-noise-dispersion",
  "modal_copycat_points_per_knot",        "Data Points per Knot",           "modal-copycat-points-per-knot",
  "modal_copycat_max_matches",            "Max Historical Matches",         "modal-copycat-max-matches",
  "modal_calcopycat_methodology",        "CalCopycat Methodology",          "copycat-cal",
  "modal_recent_weeks",                  "Recent Weeks to Use",             "modal-recent-weeks",
  "modal_resp_week_range",               "Respiratory Week Range",          "modal-resp-week-range",
  "modal_calcopycat_week_range",          "Respiratory Week Range",         "modal-calcopycat-week-range",
  "modal_calcopycat_share_groups",       "Group Trajectories",              "modal-copycat-share-groups",
  "modal_copycat_share_groups",          "Group Trajectories",              "modal-copycat-share-groups"
)

# Each modal's DOM container id is derived from its (guaranteed-unique) input
# id rather than hand-typed. This is what lets modal_calcopycat_share_groups
# and modal_copycat_share_groups safely share one content file: their DOM ids
# are still distinct (derived from distinct input ids), so the old
# "-plain" suffix workaround needed to avoid a DOM id collision is no longer
# needed.
modal_registry$dom_id <- paste0("modal-", gsub("_", "-", modal_registry$id))
modal_registry_find_root <- function(start_dir) {
  current <- normalizePath(start_dir, mustWork = FALSE)
  repeat {
    if (dir.exists(file.path(current, "www", "content"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      return(normalizePath(start_dir, mustWork = FALSE))
    }
    current <- parent
  }
}

modal_registry_root <- modal_registry_find_root(getwd())
modal_registry_content_paths <- file.path(
  modal_registry_root,
  "www",
  "content",
  paste0(modal_registry$md, ".md")
)

stopifnot(
  "modal_registry$id must be unique" =
    !anyDuplicated(modal_registry$id),
  "modal_registry$dom_id must be unique" =
    !anyDuplicated(modal_registry$dom_id),
  "every modal_registry$md file must exist under www/content/" =
    all(file.exists(modal_registry_content_paths))
)
