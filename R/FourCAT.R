# FourCAT.R
# Integration of the FourCAT transformer-based forecasting model into the
# MicroHub Shiny tool.
#
# Follows the same wrangle -> fit_process -> format_forecasts pattern as
# the other models (INFLAenza, Copycat, GBQR). The app.R server block for
# FourCAT should mirror the INLA/Copycat blocks exactly:
#
#   fourcat_results <- fit_process_fourcat(
#     clean_data        = fcast_data(),       # already truncated by data_to_drop
#     fcast_horizon     = fcast_horizon(),    # horizon + weeks_to_drop
#     quantiles_needed  = rv$quantiles_needed,
#     seeds             = c(41L, 42L, 43L)
#   )
#   fourcat_results_formatted <- format_forecasts(
#     forecast_df = fourcat_results,
#     model_name  = "FourCAT",
#     data_df     = fcast_data(),
#     data_to_drop = input$data_to_drop
#   )
#
# Checkpoint files are expected at:
#   data/fourcat_checkpoints/checkpoint_41.pt
#   data/fourcat_checkpoints/checkpoint_42.pt
#   data/fourcat_checkpoints/checkpoint_43.pt

# Python environment setup =====================================================
#
# Requires the FourCAT virtualenv to be created first.
# Run setup_fourcat_env.R once before launching the app.

library(reticulate)

# Name of the virtualenv created by setup_fourcat_env.R
FOURCAT_VENV <- "fourcat_env"

# Activate the virtualenv -- works on any machine regardless of Python
# install location, as reticulate resolves the venv path automatically.
if (!reticulate::virtualenv_exists(FOURCAT_VENV)) {
  stop(
    "FourCAT Python environment not found.\n",
    "Please run setup_fourcat_env.R first, then restart R and relaunch the app."
  )
}

reticulate::use_virtualenv(FOURCAT_VENV, required = TRUE)

# Add the checkpoint directory to Python's path so the inference module
# can be found alongside the checkpoint files
reticulate::py_run_string("import sys; sys.path.insert(0, 'data/fourcat_checkpoints')")

epi_infer <- import("infer_pinball_realtime")

# Hardcoded checkpoint directory (relative to app working directory)
FOURCAT_CKPT_DIR <- "data/fourcat_checkpoints"

# Wrangle data for FourCAT =====================================================

#' Prepare Shiny upload data for FourCAT inference.
#'
#' The FourCAT Python data loader (SurveillanceDataLoader) expects:
#'   - Each filename in the format: source_disease_groupingvar.csv
#'   - Specific columns: source, disease, year, epiweek, week, date,
#'     grouping_var_type, population, value, rate, raw_rate, grouping_var_1
#'
#' The Shiny upload only provides date, target_group, value. This function
#' adds the required stub columns and writes one CSV per target_group to a
#' temporary directory, using a fixed filename format that satisfies the
#' loader. The temp directory is cleaned up after inference.
#'
#' @param clean_data  Data frame from fcast_data() — already truncated to the
#'                    correct number of dropped weeks and filtered to
#'                    date <= forecast_date. Columns: date, target_group, value.
#' @return Path to the temporary data folder (character).

wrangle_fourcat <- function(clean_data, zone) {
  
  # Create a fresh temp directory for this run
  tmp_dir <- file.path(tempdir(), paste0("fourcat_", format(Sys.time(), "%Y%m%d%H%M%S")))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # One CSV per target_group — each becomes one series in the loader
  groups <- unique(clean_data$target_group)
  
  for (grp in groups) {
    
    grp_data <- clean_data |>
      dplyr::filter(target_group == grp) |>
      dplyr::arrange(date) |>
      dplyr::mutate(
        source            = "MicroHub",
        disease           = "respiratory",
        year              = lubridate::year(date),
        week              = lubridate::isoweek(date),
        epiweek           = MMWRweek::MMWRweek(date)$MMWRweek,
        grouping_var_type = "target_group",
        grouping_var_1    = target_group,
        population        = NA_real_,
        rate              = NA_real_,
        raw_rate          = NA_real_,
        zone              = zone
      ) |>
      dplyr::select(
        source, disease, year, epiweek, week, date,
        grouping_var_type, grouping_var_1,
        population, value, rate, raw_rate,
        zone
      )
    
    # Sanitise group name for use in filename:
    # replace spaces and special characters with underscores
    grp_safe <- gsub("[^A-Za-z0-9]", "_", grp)
    
    # Filename must satisfy loader format: source_disease_groupingvar.csv
    fname <- paste0("MicroHub_respiratory_", grp_safe, ".csv")
    
    readr::write_csv(grp_data, file.path(tmp_dir, fname))
  }
  
  return(tmp_dir)
}

# Fit and process FourCAT ======================================================

#' Run FourCAT inference across multiple seeds and return quantile forecasts.
#'
#' Horizon numbering: FourCAT internally uses horizons 0..fcast_horizon-1
#' relative to the last date in clean_data. format_forecasts() in data_utils.R
#' then handles the reference_date assignment and horizon re-labeling to match
#' the other models, so no special treatment is needed here.
#'
#' @param clean_data       Data frame from fcast_data(). Columns: date,
#'                         target_group, value.
#' @param fcast_horizon    Kept for API consistency with other models but not
#'                         used internally — the checkpoint horizon count is fixed
#'                         at training time and is inferred from the checkpoint.
#' @param quantiles_needed Numeric vector of quantile levels (e.g. rv$quantiles_needed).
#' @param seeds            Integer vector of seeds. A checkpoint file named
#'                         checkpoint_{seed}.pt must exist in FOURCAT_CKPT_DIR
#'                         for each seed.
#' @param input_len        Input window length passed to the Python model (default 32).
#' @param min_series_length Minimum series length for the Python dataset builder (default 40).
#' @return Data frame with columns: horizon, target_group, output_type,
#'         output_type_id, value. Ready for format_forecasts().

fit_process_fourcat <- function(
    clean_data,
    fcast_horizon,
    quantiles_needed,
    zone,
    seeds             = c(41L, 42L, 43L),
    input_len         = 32L,
    min_series_length = 40L
) {

  # --- 1. Write temp CSVs in FourCAT loader format ---
  tmp_data_dir <- wrangle_fourcat(clean_data, zone = zone)
  on.exit(unlink(tmp_data_dir, recursive = TRUE), add = TRUE)

  # Always pass the fixed horizon sequence the checkpoint was trained on (0:4).
  # fcast_horizon is intentionally NOT used here — the checkpoint has a fixed
  # output head of 5 horizons and will error if given a different count.
  # format_forecasts() in data_utils.R handles horizon re-labeling and dropping
  # of backfill weeks afterward, exactly as it does for every other model.
  horizons_py <- reticulate::py_eval("[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")

  # --- 2. Run inference for each seed ---
  seed_results <- purrr::map(seeds, function(s) {

    ckpt_path <- file.path(FOURCAT_CKPT_DIR, paste0("checkpoint_", s, ".pt"))

    if (!file.exists(ckpt_path)) {
      stop(sprintf(
        "FourCAT checkpoint not found for seed %d.\nExpected: %s",
        s, normalizePath(ckpt_path, mustWork = FALSE)
      ))
    }

    tmp_out_dir <- file.path(tempdir(), paste0("fourcat_out_seed", s, "_",
                                               format(Sys.time(), "%Y%m%d%H%M%S")))
    on.exit(unlink(tmp_out_dir, recursive = TRUE), add = TRUE)

    py_df <- epi_infer$run_epiweek_forecast_from_folder(
      data_folder       = as.character(tmp_data_dir),
      checkpoint        = as.character(ckpt_path),
      out_dir           = as.character(tmp_out_dir),
      input_len         = as.integer(input_len),
      horizons          = horizons_py,
      no_normalize      = FALSE,
      min_series_length = as.integer(min_series_length),
      device            = NULL,
      latest_only       = TRUE,   # only keep the most recent reference_date
      score             = FALSE,
      unsafe_load       = FALSE,
      seed              = as.integer(s)
    )

    tibble::as_tibble(reticulate::py_to_r(py_df)) |>
      dplyr::mutate(seed = s)
  })

  all_seeds <- dplyr::bind_rows(seed_results)

  # --- 3. Keep only the most recent reference_date per seed ---
  # latest_only=TRUE in the Python call should handle this, but the Python
  # groupby selects the max reference_date per (disease, grouping_var) which
  # can still return multiple dates across groups. Filtering here guarantees
  # exactly one reference_date across all groups and seeds, preventing
  # duplicate (grouping_var, horizon, quantile) rows that break spread() in
  # plot_forecasts().
  latest_ref_date <- max(as.Date(all_seeds$reference_date), na.rm = TRUE)

  all_seeds <- all_seeds |>
    dplyr::filter(as.Date(reference_date) == latest_ref_date)


  # --- 4. Ensemble: mean across seeds at each (grouping_var, horizon, quantile) ---
  ensembled <- all_seeds |>
    dplyr::group_by(grouping_var, horizon, quantile) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

  # --- 5. Reformat to match the output contract of all other models ---
  # Required columns: horizon, target_group, output_type, output_type_id, value
  #
  # grouping_var in FourCAT is stored as "target_group||grouping_var_type"
  # (set by SurveillanceDataLoader._compose_grouping_key). We split on "||"
  # to recover the original target_group name.

  result <- ensembled |>
    dplyr::mutate(
      target_group   = stringr::str_split_fixed(as.character(grouping_var), "\\|\\|", 2)[, 2],
      output_type    = "quantile",
      output_type_id = as.character(round(as.numeric(quantile), 3)),
      value          = pmax(value, 0),          # clip negatives before rounding
      value          = round(value, 0),
      horizon        = as.integer(horizon) + 1L # convert 0-indexed to 1-indexed
      # to match the other models
      # (format_forecasts expects
      # horizon >= 1 before re-labeling)
    ) |>
    dplyr::select(horizon, target_group, output_type, output_type_id, value) |>
    dplyr::arrange(target_group, horizon, output_type_id)

  return(result)
}