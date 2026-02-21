##################################

default_gbqr_week_windows <- function(seasonality) {
  if (seasonality == "NH") {
    list(c(40, 53), c(1, 39))
  } else {
    list(c(8, 50))
  }
}

wrangle_gbqr_for_app <- function(
  clean_data,
  seasonality,
  country = "Paraguay",
  in_season_weeks = NULL
) {
  if (is.null(in_season_weeks)) {
    in_season_weeks <- list()
    in_season_weeks[[country]] <- default_gbqr_week_windows(seasonality)
  }

  data_in <- clean_data |>
    dplyr::mutate(date = as.Date(date))

  mmwr <- MMWRweek::MMWRweek(data_in$date)

  df <- data_in |>
    dplyr::transmute(
      wk_end_date = date,
      location = country,
      target_group = target_group,
      pop = 1000000, # temporary fallback until real population input is wired in
      value_count = pmax(value, 0),
      season = mmwr[["MMWRyear"]],
      season_week = mmwr[["MMWRweek"]]
    ) |>
    dplyr::mutate(inc = value_count / (pop / 100000)) |>
    dplyr::select(-value_count)

  df |>
    dplyr::mutate(
      log_pop = log(pop),
      inc_4rt = (inc + 0.01)^0.25
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(in_season = {
      loc_weeks <- in_season_weeks[[location]]
      if (is.null(loc_weeks)) FALSE
      else any(vapply(loc_weeks, function(r) season_week >= r[1] & season_week <= r[2], logical(1)))
    }) |>
    dplyr::ungroup() |>
    dplyr::group_by(location, target_group) |>
    dplyr::mutate(
      inc_4rt_scale_factor = {
        x <- inc_4rt[in_season & !is.na(inc_4rt)]
        if (length(x) == 0) stats::quantile(inc_4rt, 0.95, na.rm = TRUE) else stats::quantile(x, 0.95, na.rm = TRUE)
      },
      inc_4rt_cs_raw = inc_4rt / (inc_4rt_scale_factor + 0.01),
      inc_4rt_center_factor = {
        x <- inc_4rt_cs_raw[in_season & !is.na(inc_4rt_cs_raw)]
        if (length(x) == 0) mean(inc_4rt_cs_raw, na.rm = TRUE) else mean(x, na.rm = TRUE)
      },
      inc_4rt_cs = inc_4rt_cs_raw - inc_4rt_center_factor
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      wk_end_date,
      location,
      target_group,
      pop,
      inc,
      season,
      season_week,
      log_pop,
      in_season,
      inc_4rt_scale_factor,
      inc_4rt_center_factor,
      inc_4rt_cs
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
  xmas_week = 25,
  delta_offsets = NULL,
  num_bags = 10,
  bag_frac_samples = 0.7,
  nrounds = 10
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
  if (is.null(delta_offsets)) {
    delta_offsets <- list()
    delta_offsets[[country]] <- 3
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
    in_season_weeks = in_season_weeks
  )

  forecast_date <- max(as.Date(gbqr_df$wk_end_date)) + lubridate::weeks(1)

  feat_out <- preprocess_and_prepare_features(
    df = gbqr_df,
    forecast_date = forecast_date,
    data_to_drop = NULL,
    forecast_horizons = seq_len(forecast_horizon),
    xmas_week = xmas_week,
    delta_offsets = delta_offsets
  )

  filtered_targets <- filter_targets_for_training(
    target_df = feat_out$target_long,
    ref_date = forecast_date,
    in_season_weeks = in_season_weeks,
    drop_missing_targets = FALSE
  )

  split_data <- split_train_test(
    df_with_pred_targets = filtered_targets,
    feat_names = feat_out$feature_names,
    ref_date = forecast_date,
    season_week_windows = season_week_windows
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

  lgb_results <- run_quantile_lgb_bagging_multi_group(
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
    q_labels = as.character(q_levels)
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
