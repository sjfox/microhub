##################################
# newGBQR main script for app
##################################

newgbqr_empty_forecast <- function() {
  tibble::tibble(
    horizon = integer(),
    target_group = character(),
    output_type = character(),
    output_type_id = character(),
    value = numeric()
  )
}

wrangle_newgbqr_for_app <- function(
    clean_data,
    seasonality = NULL,
    country = "Paraguay",
    rate_per = 100000
) {
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
    stop("When provided, population values must be finite and strictly positive for newGBQR.")
  }

  df <- data_in |>
    dplyr::transmute(
      wk_end_date = date,
      location = country,
      target_group = target_group,
      inc = pmax(value, 0),
      population = population,
      uses_population = has_population,
      season = mmwr[["MMWRyear"]],
      season_week = mmwr[["MMWRweek"]],
      log_pop = if (has_population) log(pmax(population, 1)) else NA_real_
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
    dplyr::group_by(location, target_group) |>
    dplyr::mutate(
      inc_4rt_scale_factor = stats::quantile(inc_4rt, 0.95, na.rm = TRUE),
      inc_4rt_cs_raw = inc_4rt / (inc_4rt_scale_factor + 0.01),
      inc_4rt_center_factor = mean(inc_4rt_cs_raw, na.rm = TRUE),
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
      inc_4rt_scale_factor,
      inc_4rt_center_factor,
      inc_4rt_cs,
      log_pop
    )
}

compute_newgbqr_empirical_peak_week <- function(df) {
  peak_by_group <- df |>
    dplyr::filter(!is.na(season_week), !is.na(inc_4rt_cs)) |>
    dplyr::mutate(season_week = pmin(as.integer(season_week), 52L)) |>
    dplyr::group_by(location, target_group, season_week) |>
    dplyr::summarise(mean_inc_4rt_cs = mean(inc_4rt_cs, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(location, target_group) |>
    dplyr::arrange(season_week, .by_group = TRUE) |>
    dplyr::mutate(
      smoothed_inc = slider::slide_dbl(
        mean_inc_4rt_cs,
        mean,
        .before = 1,
        .after = 1,
        .complete = FALSE,
        na.rm = TRUE
      )
    ) |>
    dplyr::arrange(dplyr::desc(smoothed_inc), dplyr::desc(mean_inc_4rt_cs), .by_group = TRUE) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  if (nrow(peak_by_group) == 0) {
    return(26)
  }

  peak_week <- stats::median(peak_by_group$season_week, na.rm = TRUE)
  as.integer(round(peak_week))
}

resolve_newgbqr_peak_week <- function(
    df,
    peak_week_method = c("empirical", "zone", "fixed"),
    peak_week = NULL,
    seasonality = NULL
) {
  peak_week_method <- match.arg(peak_week_method)

  if (identical(peak_week_method, "fixed")) {
    peak_week <- as.numeric(peak_week)[1]
    if (is.na(peak_week)) {
      stop("peak_week must be provided when peak_week_method = 'fixed'.")
    }
    return(peak_week)
  }

  if (identical(peak_week_method, "zone")) {
    if (is.null(seasonality) || is.na(seasonality)) {
      stop("seasonality must be provided when peak_week_method = 'zone'.")
    }
    return(default_gbqr_peak_week(seasonality))
  }

  compute_newgbqr_empirical_peak_week(df)
}

fit_process_newgbqr <- function(
    clean_data,
    fcast_horizon = NULL,
    quantiles_needed = NULL,
    seasonality = NULL,
    forecast_horizon = NULL,
    q_levels = NULL,
    country = "Paraguay",
    peak_week_method = c("empirical", "zone", "fixed"),
    peak_week = NULL,
    num_bags = 50,
    bag_frac_samples = 0.7,
    nrounds = 50,
    model_type = "individual",
    rate_per = 100000,
    min_train_rows = 12,
    learning_rate = 0.05,
    num_leaves = 11,
    min_data_in_leaf = 8,
    feature_fraction = 0.8
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

  q_levels <- sort(unique(as.numeric(quantiles_needed)))
  q_levels <- q_levels[!is.na(q_levels)]
  if (length(q_levels) == 0) {
    stop("quantiles_needed must contain at least one numeric quantile.")
  }

  gbqr_df <- wrangle_newgbqr_for_app(
    clean_data = clean_data,
    seasonality = seasonality,
    country = country,
    rate_per = rate_per
  )

  resolved_peak_week <- resolve_newgbqr_peak_week(
    df = gbqr_df,
    peak_week_method = peak_week_method,
    peak_week = peak_week,
    seasonality = seasonality
  )

  forecast_date <- max(as.Date(gbqr_df$wk_end_date)) + lubridate::weeks(1)

  feat_out <- preprocess_and_prepare_newgbqr_features(
    df = gbqr_df,
    forecast_date = forecast_date,
    data_to_drop = NULL,
    forecast_horizons = seq_len(forecast_horizon),
    peak_week = resolved_peak_week
  )

  filtered_targets <- filter_newgbqr_targets_for_training(
    target_df = feat_out$target_long,
    ref_date = forecast_date,
    drop_missing_targets = FALSE
  )

  split_data <- split_newgbqr_train_test(
    df_with_pred_targets = feat_out$target_long,
    feat_names = feat_out$feature_names,
    ref_date = forecast_date,
    filtered_targets = filtered_targets
  )

  if (length(split_data) == 0) {
    return(newgbqr_empty_forecast())
  }

  train_rows <- vapply(split_data, function(x) nrow(x$df_train), integer(1))
  use_global <- identical(model_type, "global") || any(train_rows < min_train_rows)
  global_train_rows <- sum(train_rows)

  if (use_global && global_train_rows < min_train_rows) {
    return(newgbqr_empty_forecast())
  }

  lgb_fn <- if (use_global) {
    run_quantile_lgb_bagging_newgbqr_global
  } else {
    run_quantile_lgb_bagging_newgbqr_multi_group
  }

  lgb_results <- lgb_fn(
    split_data_list = split_data,
    feat_names = feat_out$feature_names,
    ref_date = forecast_date,
    num_bags = num_bags,
    q_levels = q_levels,
    bag_frac_samples = bag_frac_samples,
    nrounds = nrounds,
    learning_rate = learning_rate,
    num_leaves = num_leaves,
    min_data_in_leaf = min_data_in_leaf,
    feature_fraction = feature_fraction
  )

  process_and_combine_newgbqr_forecasts(
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
