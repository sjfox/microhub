
########
##################################
# GBQR main script for app
##################################

 default_gbqr_week_windows <- function(seasonality) {
  if (seasonality == "A") {
    list(c(42, 52), c(1, 16))
  } else if (seasonality == "B") {
    list(c(39, 52), c(1, 13))
  } else if (seasonality == "C") {
    list(c(36, 52), c(1, 10))
  } else if (seasonality == "D") {
    list(c(19, 45))
  } else if (seasonality == "E") {
    list(c(19, 45))
  } else {
    stop("seasonality must be one of: A, B, C, D, E")
  }
}

default_gbqr_peak_week <- function(seasonality) {
  if (seasonality == "A") {
    5
  } else if (seasonality == "B") {
    6
  } else if (seasonality == "C") {
    51
  } else if (seasonality == "D") {
    28
  } else if (seasonality == "E") {
    27
  } else {
    stop("seasonality must be one of: A, B, C, D, E")
  }
}

wrangle_gbqr_for_app <- function(
    clean_data,
    seasonality,
    country = "Paraguay",
    in_season_weeks = NULL,
    rate_per = 100000
) {
  if (is.null(in_season_weeks)) {
    in_season_weeks <- list()
    in_season_weeks[[country]] <- default_gbqr_week_windows(seasonality)
  }

  data_in <- clean_data |>
    dplyr::mutate(date = as.Date(date))

  if ("population" %in% names(data_in)) {
    data_in <- data_in |>
      dplyr::mutate(population = as.numeric(population))
  } else {
    data_in$population <- NA_real_
  }

  mmwr <- MMWRweek::MMWRweek(data_in$date)

  has_population <- !all(is.na(data_in$population))
  if (has_population && any(!is.finite(data_in$population) | data_in$population <= 0, na.rm = TRUE)) {
    stop("When provided, population values must be finite and strictly positive for GBQR.")
  }

  df <- data_in |>
    dplyr::transmute(
      wk_end_date  = date,
      location     = country,
      target_group = target_group,
      inc          = pmax(value, 0),
      population   = population,
      uses_population = has_population,
      season       = mmwr[["MMWRyear"]],
      season_week  = mmwr[["MMWRweek"]],
      log_pop      = if (has_population) log(pmax(population, 1)) else NA_real_
    )

  df |>
    dplyr::mutate(
      model_inc = dplyr::if_else(
        uses_population,
        inc / population * rate_per,
        inc
      ),
      inc_4rt = (model_inc + 0.01 + 0.75^4)^0.25
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      in_season = {
        loc_weeks <- in_season_weeks[[location]]
        if (is.null(loc_weeks)) {
          FALSE
        } else {
          any(vapply(loc_weeks, function(r) season_week >= r[1] & season_week <= r[2], logical(1)))
        }
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(location, target_group) |>
    dplyr::mutate(
      inc_4rt_scale_factor = {
        x <- inc_4rt[in_season & !is.na(inc_4rt)]
        if (length(x) == 0) {
          stats::quantile(inc_4rt, 0.95, na.rm = TRUE)
        } else {
          stats::quantile(x, 0.95, na.rm = TRUE)
        }
      },
      inc_4rt_cs_raw = inc_4rt / (inc_4rt_scale_factor + 0.01),
      inc_4rt_center_factor = {
        x <- inc_4rt_cs_raw[in_season & !is.na(inc_4rt_cs_raw)]
        if (length(x) == 0) {
          mean(inc_4rt_cs_raw, na.rm = TRUE)
        } else {
          mean(x, na.rm = TRUE)
        }
      },
      inc_4rt_cs = inc_4rt_cs_raw - inc_4rt_center_factor
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      wk_end_date,
      location,
      target_group,
      inc,
      population,
      uses_population,
      season,
      season_week,
      in_season,
      inc_4rt_scale_factor,
      inc_4rt_center_factor,
      inc_4rt_cs,
      log_pop
    )
}

fit_process_gbqr <- function(
    clean_data,
    fcast_horizon = NULL,
    quantiles_needed = NULL,
    seasonality,
    forecast_horizon = NULL,
    q_levels = NULL,
    country = "Paraguay",
    in_season_weeks = NULL,
    season_week_windows = NULL,
    peak_week = NULL,
    num_bags = 50,
    bag_frac_samples = 0.7,
    nrounds = 50,
    model_type = "individual",
    rate_per = 100000
) {
  if (is.null(fcast_horizon)) {
    fcast_horizon <- forecast_horizon
  }
  if (is.null(quantiles_needed)) {
    quantiles_needed <- q_levels
  }

  if (is.null(fcast_horizon) || is.null(quantiles_needed)) {
    stop("Provide forecast horizon and quantiles via fcast_horizon/quantiles_needed (or forecast_horizon/q_levels).")
  }

  if (!all(c("date", "target_group", "value") %in% names(clean_data))) {
    stop("clean_data must include: date, target_group, value")
  }

  forecast_horizon <- as.integer(fcast_horizon)
  if (length(forecast_horizon) != 1 || is.na(forecast_horizon) || forecast_horizon < 1) {
    stop("fcast_horizon must be a single positive integer.")
  }

  if (is.null(in_season_weeks)) {
    in_season_weeks <- list()
    in_season_weeks[[country]] <- default_gbqr_week_windows(seasonality)
  }

  if (is.null(season_week_windows)) {
    season_week_windows <- in_season_weeks[[country]]
  }

  if (is.null(peak_week)) {
    peak_week <- default_gbqr_peak_week(seasonality)
  }

  q_levels <- sort(unique(as.numeric(quantiles_needed)))
  q_levels <- q_levels[!is.na(q_levels)]
  if (length(q_levels) == 0) {
    stop("quantiles_needed must contain at least one numeric quantile.")
  }

  gbqr_df <- wrangle_gbqr_for_app(
    clean_data = clean_data,
    seasonality = seasonality,
    country = country,
    in_season_weeks = in_season_weeks,
    rate_per = rate_per
  )

  forecast_date <- max(as.Date(gbqr_df$wk_end_date)) + lubridate::weeks(1)

  feat_out <- preprocess_and_prepare_features(
    df = gbqr_df,
    forecast_date = forecast_date,
    data_to_drop = NULL,
    forecast_horizons = seq_len(forecast_horizon),
    peak_week = peak_week
  )

  filtered_targets <- filter_targets_for_training(
    target_df = feat_out$target_long,
    ref_date = forecast_date,
    in_season_weeks = in_season_weeks,
    drop_missing_targets = FALSE
  )

  split_data <- split_train_test(
    df_with_pred_targets = feat_out$target_long,
    feat_names = feat_out$feature_names,
    ref_date = forecast_date,
    season_week_windows = season_week_windows,
    filtered_targets = filtered_targets
  )

  if (length(split_data) == 0) {
    return(tibble::tibble(
      horizon = integer(),
      target_group = character(),
      output_type = character(),
      output_type_id = character(),
      value = numeric()
    ))
  }

  lgb_fn <- if (model_type == "global") {
    run_quantile_lgb_bagging_global
  } else {
    run_quantile_lgb_bagging_multi_group
  }

  lgb_results <- lgb_fn(
    split_data_list = split_data,
    feat_names = feat_out$feature_names,
    ref_date = forecast_date,
    num_bags = num_bags,
    q_levels = q_levels,
    bag_frac_samples = bag_frac_samples,
    nrounds = nrounds
  )

  process_and_combine_gbqr_forecasts(
    test_preds_by_group = lgb_results$test_preds_by_group,
    split_data = split_data,
    q_labels = as.character(q_levels),
    rate_per = rate_per
  ) |>
    dplyr::mutate(
      output_type = "quantile",
      output_type_id = as.character(output_type_id)
    ) |>
    dplyr::arrange(target_group, horizon, output_type_id) |>
    dplyr::select(
      horizon,
      target_group,
      output_type,
      output_type_id,
      value
    )
}
