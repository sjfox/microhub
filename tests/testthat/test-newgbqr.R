test_that("newGBQR wrangling scales using all weeks", {
  clean_data <- tibble(
    date = seq.Date(as.Date("2024-01-06"), by = "1 week", length.out = 30),
    target_group = rep("Overall", 30),
    value = c(1000, rep(10, 29))
  )

  newgbqr_df <- wrangle_newgbqr_for_app(
    clean_data = clean_data,
    seasonality = "E"
  )

  expected_inc_4rt <- (clean_data$value + 0.01 + 0.75^4)^0.25
  expected_scale <- as.numeric(stats::quantile(expected_inc_4rt, 0.95, na.rm = TRUE))
  expected_center <- mean(expected_inc_4rt / (expected_scale + 0.01), na.rm = TRUE)

  expect_equal(unique(newgbqr_df$inc_4rt_scale_factor), expected_scale, tolerance = 1e-8)
  expect_equal(unique(newgbqr_df$inc_4rt_center_factor), expected_center, tolerance = 1e-8)
})

test_that("newGBQR target filter retains valid rows from every week", {
  clean_data <- tibble(
    date = seq.Date(as.Date("2024-01-06"), by = "1 week", length.out = 60),
    target_group = rep("Overall", 60),
    value = seq_len(60)
  )

  newgbqr_df <- wrangle_newgbqr_for_app(clean_data, seasonality = "E")
  feat_out <- preprocess_and_prepare_newgbqr_features(
    df = newgbqr_df,
    forecast_horizons = 1,
    peak_week = 26
  )

  filtered <- filter_newgbqr_targets_for_training(
    target_df = feat_out$target_long,
    ref_date = max(newgbqr_df$wk_end_date) + lubridate::weeks(1),
    drop_missing_targets = TRUE
  )

  default_e_weeks <- default_gbqr_week_windows("E")[[1]]
  outside_default_e <- filtered$season_week < default_e_weeks[1] |
    filtered$season_week > default_e_weeks[2]

  expect_true(any(outside_default_e))
  expect_true(all(!is.na(filtered$delta_target)))
})

test_that("newGBQR empirical peak selects the highest smoothed incidence week", {
  df <- tidyr::expand_grid(
    location = "Paraguay",
    target_group = c("Overall", "Adult"),
    season_week = 1:52
  ) |>
    dplyr::mutate(
      inc_4rt_cs = dplyr::if_else(season_week == 12L, 10, 1),
      wk_end_date = as.Date("2024-01-06") + lubridate::weeks(season_week - 1)
    )

  expect_equal(compute_newgbqr_empirical_peak_week(df), 12)
})

test_that("newGBQR adds only the narrow volatility features to feature_names", {
  clean_data <- tibble(
    date = seq.Date(as.Date("2024-01-06"), by = "1 week", length.out = 20),
    target_group = rep("Overall", 20),
    value = c(5, 5, 6, 6, 5, 20, 1, 25, 2, 24, rep(5, 10))
  )

  newgbqr_df <- wrangle_newgbqr_for_app(clean_data, seasonality = "E")
  feat_out <- preprocess_and_prepare_newgbqr_features(
    df = newgbqr_df,
    forecast_horizons = 1:2,
    peak_week = 26
  )

  expect_true("inc_4rt_cs_rollsd_w4" %in% feat_out$feature_names)
  expect_true("inc_4rt_cs_abs_change1" %in% feat_out$feature_names)
  expect_false("inc_4rt_cs_rollsd_w4_lag1" %in% feat_out$feature_names)
  expect_false("inc_4rt_cs_abs_change1_lag1" %in% feat_out$feature_names)
  expect_false("inc_4rt_cs_rollsd_w8" %in% feat_out$feature_names)
  expect_false("inc_4rt_cs_abs_change4" %in% feat_out$feature_names)
})

test_that("newGBQR rolling volatility feature increases during volatile period", {
  clean_data <- tibble(
    date = seq.Date(as.Date("2024-01-06"), by = "1 week", length.out = 16),
    target_group = rep("Overall", 16),
    value = c(rep(10, 8), 5, 18, 4, 20, rep(10, 4))
  )

  newgbqr_df <- wrangle_newgbqr_for_app(clean_data, seasonality = "E")
  feat_out <- preprocess_and_prepare_newgbqr_features(
    df = newgbqr_df,
    forecast_horizons = 1,
    peak_week = 26
  )

  stable_sd <- feat_out$df_with_features$inc_4rt_cs_rollsd_w4[8]
  volatile_sd <- feat_out$df_with_features$inc_4rt_cs_rollsd_w4[12]

  expect_true(is.finite(stable_sd))
  expect_true(is.finite(volatile_sd))
  expect_gt(volatile_sd, stable_sd)
})

test_that("newGBQR horizon interaction columns vary by horizon for same base week", {
  clean_data <- tibble(
    date = seq.Date(as.Date("2024-01-06"), by = "1 week", length.out = 20),
    target_group = rep("Overall", 20),
    value = seq(10, 29)
  )

  newgbqr_df <- wrangle_newgbqr_for_app(clean_data, seasonality = "E")
  feat_out <- preprocess_and_prepare_newgbqr_features(
    df = newgbqr_df,
    forecast_horizons = 1:2,
    peak_week = 26
  )

  expect_true(all(c("horizon_sq", "horizon_x_level", "horizon_x_vol_sd4") %in% feat_out$feature_names))

  same_week <- feat_out$target_long |>
    dplyr::filter(wk_end_date == as.Date("2024-04-13")) |>
    dplyr::arrange(horizon)

  expect_equal(same_week$horizon, c(1L, 2L))
  expect_true(same_week$horizon_sq[2] > same_week$horizon_sq[1])
  expect_true(same_week$horizon_x_level[2] > same_week$horizon_x_level[1])
  expect_true(same_week$horizon_x_vol_sd4[2] >= same_week$horizon_x_vol_sd4[1])
})

test_that("newGBQR circular delta treats year boundary weeks as adjacent", {
  expect_equal(newgbqr_circular_delta_peak(52, peak_week = 1), -1)
  expect_equal(newgbqr_circular_delta_peak(2, peak_week = 52), 2)
  expect_equal(newgbqr_circular_delta_peak(26, peak_week = 1), 25)
})

test_that("newGBQR forecast processing preserves schema and orders quantiles", {
  q_labels <- c("0.1", "0.9")
  split_data <- list(
    "Paraguay___Overall" = list(
      df_test = tibble(
        wk_end_date = as.Date("2024-03-02"),
        location = "Paraguay",
        target_group = "Overall",
        population = NA_real_,
        uses_population = FALSE,
        inc_4rt_cs = 1,
        horizon = 1L,
        inc_4rt_center_factor = 0,
        inc_4rt_scale_factor = 1
      )
    )
  )

  test_preds_by_group <- list(
    "Paraguay___Overall" = array(c(1, -1), dim = c(1, 1, 2))
  )

  result <- process_and_combine_newgbqr_forecasts(
    test_preds_by_group = test_preds_by_group,
    split_data = split_data,
    q_labels = q_labels
  )

  expect_named(result, c("horizon", "target_group", "output_type", "output_type_id", "value"))
  expect_equal(result$output_type_id, q_labels)
  expect_true(result$value[1] <= result$value[2])
})

test_that("newGBQR addition leaves old GBQR entry points available", {
  expect_true(is.function(fit_process_gbqr))
  expect_true(is.function(wrangle_gbqr_for_app))
  expect_true(is.function(filter_targets_for_training))
})
