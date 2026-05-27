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

test_that("newGBQR keeps shared season helpers without old GBQR model entry points", {
  expect_true(is.function(default_gbqr_week_windows))
  expect_true(is.function(default_gbqr_peak_week))
  expect_false(exists("fit_process_gbqr", mode = "function"))
  expect_false(exists("wrangle_gbqr_for_app", mode = "function"))
  expect_false(exists("filter_targets_for_training", mode = "function"))
})

test_that("newGBQR global bagging reports one progress event per bag", {
  split_data <- list(
    "Paraguay___Overall" = list(
      target_group = "Overall",
      df_train = tibble(season = c(2022, 2023), feature = c(1, 2)),
      x_train = tibble(feature = c(1, 2)),
      y_train = c(0.1, 0.2),
      x_test = tibble(feature = 3)
    )
  )

  events <- list()
  run_quantile_lgb_bagging_newgbqr_global(
    split_data_list = split_data,
    feat_names = "feature",
    ref_date = as.Date("2024-01-06"),
    num_bags = 3,
    q_levels = 0.5,
    bag_frac_samples = 1,
    nrounds = 1,
    learning_rate = 0.05,
    num_leaves = 2,
    min_data_in_leaf = 1,
    feature_fraction = 1,
    progress_callback = function(completed, total, detail) {
      events[[length(events) + 1]] <<- list(completed = completed, total = total, detail = detail)
    },
    lgb_dataset_fn = function(data, label) list(data = data, label = label),
    lgb_train_fn = function(params, data, nrounds) list(params = params),
    predict_fn = function(object, newdata, ...) rep(0, nrow(newdata))
  )

  expect_equal(vapply(events, `[[`, integer(1), "completed"), 1:3)
  expect_equal(unique(vapply(events, `[[`, numeric(1), "total")), 3)
  expect_true(all(grepl("^Testing bag", vapply(events, `[[`, character(1), "detail"))))
})

test_that("newGBQR multi-group bagging reports progress across groups and bags", {
  split_data <- list(
    "Paraguay___Adults" = list(
      location = "Paraguay",
      target_group = "Adults",
      df_train = tibble(season = c(2022, 2023), feature = c(1, 2)),
      x_train = tibble(feature = c(1, 2)),
      y_train = c(0.1, 0.2),
      x_test = tibble(feature = 3)
    ),
    "Paraguay___Pediatrics" = list(
      location = "Paraguay",
      target_group = "Pediatrics",
      df_train = tibble(season = c(2022, 2023), feature = c(1, 2)),
      x_train = tibble(feature = c(1, 2)),
      y_train = c(0.1, 0.2),
      x_test = tibble(feature = 3)
    )
  )

  events <- list()
  run_quantile_lgb_bagging_newgbqr_multi_group(
    split_data_list = split_data,
    feat_names = "feature",
    ref_date = as.Date("2024-01-06"),
    num_bags = 2,
    q_levels = 0.5,
    bag_frac_samples = 1,
    nrounds = 1,
    learning_rate = 0.05,
    num_leaves = 2,
    min_data_in_leaf = 1,
    feature_fraction = 1,
    progress_callback = function(completed, total, detail) {
      events[[length(events) + 1]] <<- list(completed = completed, total = total, detail = detail)
    },
    lgb_dataset_fn = function(data, label) list(data = data, label = label),
    lgb_train_fn = function(params, data, nrounds) list(params = params),
    predict_fn = function(object, newdata, ...) rep(0, nrow(newdata)),
    lgb_importance_fn = function(model) data.frame(Feature = "feature", Gain = 1)
  )

  expect_equal(vapply(events, `[[`, integer(1), "completed"), 1:4)
  expect_equal(unique(vapply(events, `[[`, numeric(1), "total")), 4)
  expect_true(any(grepl("Adults bag", vapply(events, `[[`, character(1), "detail"))))
  expect_true(any(grepl("Pediatrics bag", vapply(events, `[[`, character(1), "detail"))))
})
