# Ensemble combination logic ===================================================
# Shared by the live "Run Ensemble" action (server/ensemble.R) and the
# retrospective backtest pipeline (R/retrospective.R) so both combine member
# model forecasts identically. Built on the hubverse `hubEnsembles` package
# (https://hubverse-org.github.io/hubEnsembles/), which several other parts of
# MicroHub's forecasting stack already build on (scoringutils, hubEvals).

#' Combine forecasts from multiple models into a single ensemble forecast
#'
#' @param forecasts Data frame of quantile forecasts in MicroHub's standard
#'   schema (columns: model, reference_date, horizon, target_end_date,
#'   target_group, output_type, output_type_id, value). May contain models
#'   other than the requested `members`; those rows are dropped before
#'   combining.
#' @param members Character vector of model names to combine. If fewer than
#'   two of `members` are actually present in `forecasts`, returns NULL.
#' @param method One of:
#'   - `"median"` (default): per-quantile median across members, via
#'     `hubEnsembles::simple_ensemble()` with `agg_fun = median`. This
#'     matches MicroHub's original ensembling behavior exactly.
#'   - `"mean"`: per-quantile mean across members, via
#'     `hubEnsembles::simple_ensemble()` with `agg_fun = mean`.
#'   - `"linear_pool"`: treats each member's quantiles as describing a full
#'     predictive distribution and mixes those distributions (a linear
#'     opinion pool, sometimes called "Vincentization"), via
#'     `hubEnsembles::linear_pool()`, before re-extracting the same
#'     quantile levels. This is generally considered more statistically
#'     principled than combining quantile-by-quantile, since the per-quantile
#'     median/mean of several distributions is not itself guaranteed to
#'     behave like a coherent single predictive distribution.
#' @param model_label Value used for the `model` column of the result
#'   (default `"Ensemble"`).
#' @param n_samples Passed through to `hubEnsembles::linear_pool()`; the
#'   number of samples used internally to approximate each member's
#'   predictive distribution before mixing. Ignored for `"median"`/`"mean"`.
#'
#' @return A tibble with columns model, reference_date, horizon,
#'   target_end_date, target_group, output_type, output_type_id, value --
#'   or NULL if fewer than two distinct members are available to combine.
build_ensemble <- function(forecasts,
                           members,
                           method = c("median", "mean", "linear_pool"),
                           model_label = "Ensemble",
                           n_samples = 10000,
                           data_type = "count") {
  method <- match.arg(method)

  if (is.null(forecasts) || nrow(forecasts) == 0 || length(members) < 2) {
    return(NULL)
  }

  member_forecasts <- forecasts |>
    dplyr::filter(.data$model %in% members)

  if (dplyr::n_distinct(member_forecasts$model) < 2) {
    return(NULL)
  }

  task_id_cols <- c("reference_date", "horizon", "target_end_date", "target_group")

  # hubEnsembles expects a `model_id` column (MicroHub calls this `model`
  # everywhere else) and a character `output_type_id`.
  hub_input <- member_forecasts |>
    dplyr::transmute(
      model_id        = as.character(.data$model),
      reference_date  = .data$reference_date,
      horizon         = .data$horizon,
      target_end_date = .data$target_end_date,
      target_group    = .data$target_group,
      output_type     = .data$output_type,
      output_type_id  = as.character(.data$output_type_id),
      value           = as.numeric(.data$value)
    )

  ensembled <- if (method == "linear_pool") {
    hubEnsembles::linear_pool(
      hub_input,
      model_id      = model_label,
      task_id_cols  = task_id_cols,
      n_samples     = n_samples
    )
  } else {
    agg_fun <- if (method == "mean") base::mean else stats::median
    hubEnsembles::simple_ensemble(
      hub_input,
      agg_fun       = agg_fun,
      agg_args      = list(na.rm = TRUE),
      model_id      = model_label,
      task_id_cols  = task_id_cols
    )
  }

  ensembled |>
    dplyr::transmute(
      model           = model_label,
      reference_date  = as.Date(.data$reference_date),
      horizon         = .data$horizon,
      target_end_date = as.Date(.data$target_end_date),
      target_group    = .data$target_group,
      output_type     = .data$output_type,
      output_type_id  = .data$output_type_id,
      value           = finalize_forecast_value(as.numeric(.data$value), data_type)
    )
}

#' Human-readable label for an ensemble method, for UI captions/reports.
ensemble_method_label <- function(method) {
  switch(method,
    median      = "Median",
    mean        = "Mean",
    linear_pool = "Linear pool",
    method
  )
}
