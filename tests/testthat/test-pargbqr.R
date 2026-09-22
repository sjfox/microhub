test_that("parGBQR target weeks wrap around the respiratory year", {
  expect_equal(pargbqr_target_week(c(51, 52, 53, 1), c(1, 1, 1, 4)), c(52L, 53L, 1L, 5L))
  expect_equal(pargbqr_circular_week_distance(c(52, 53, 1, 2), 1), c(2, 1, 0, 1))
})

test_that("parGBQR empirical residual offsets use horizon and nearby target week", {
  df_train <- tibble(
    horizon = c(rep(2L, 9), rep(2L, 9), rep(1L, 9)),
    season_week = c(rep(7L, 9), rep(25L, 9), rep(7L, 9))
  )
  residuals <- c(
    -4:4,
    96:104,
    46:54
  )
  df_test <- tibble(
    horizon = 2L,
    season_week = 7L
  )

  offsets <- pargbqr_residual_quantile_offsets(
    df_train = df_train,
    residuals = residuals,
    df_test = df_test,
    q_levels = c(0.25, 0.5, 0.75),
    season_week_window = 4L,
    min_residuals = 8L
  )

  local_pool <- -4:4
  expected <- stats::quantile(local_pool, c(0.25, 0.5, 0.75), names = FALSE, type = 8) -
    stats::quantile(local_pool, 0.5, names = FALSE, type = 8)

  expect_equal(drop(offsets), expected, tolerance = 1e-8)
})

test_that("parGBQR empirical residual offsets fall back to horizon before all residuals", {
  df_train <- tibble(
    horizon = c(rep(2L, 6), rep(1L, 20)),
    season_week = c(rep(25L, 6), rep(7L, 20))
  )
  residuals <- c(95:100, seq(-10, 9))
  df_test <- tibble(
    horizon = 2L,
    season_week = 7L
  )

  offsets <- pargbqr_residual_quantile_offsets(
    df_train = df_train,
    residuals = residuals,
    df_test = df_test,
    q_levels = c(0.25, 0.5, 0.75),
    season_week_window = 4L,
    min_residuals = 8L
  )

  horizon_pool <- 95:100
  expected <- stats::quantile(horizon_pool, c(0.25, 0.5, 0.75), names = FALSE, type = 8) -
    stats::quantile(horizon_pool, 0.5, names = FALSE, type = 8)

  expect_equal(drop(offsets), expected, tolerance = 1e-8)
})

test_that("parGBQR quantile deltas add empirical residual offsets to delta predictions", {
  delta_hat <- c(0, 1)
  residual_offsets <- matrix(c(-2, 0, 3, -4, 0, 6), nrow = 2, byrow = TRUE)

  quantiles <- pargbqr_delta_quantiles(delta_hat, residual_offsets)

  expect_equal(dim(quantiles), c(2L, 3L))
  expect_equal(quantiles[, 2], delta_hat, tolerance = 1e-8)
  expect_equal(quantiles[1, ], c(-2, 0, 3), tolerance = 1e-8)
  expect_equal(quantiles[2, ], c(-3, 1, 7), tolerance = 1e-8)
})

test_that("parGBQR fits one median quantile LightGBM model per bag", {
  params <- pargbqr_lgb_params(
    seed = 1,
    learning_rate = 0.05,
    num_leaves = 11,
    min_data_in_leaf = 8,
    feature_fraction = 0.8
  )

  expect_equal(params$objective, "quantile")
  expect_equal(params$alpha, 0.5)
})

test_that("parGBQR global runner trains on transformed delta targets", {
  split_data <- list(
    "Paraguay___Overall" = list(
      target_group = "Overall",
      df_train = tibble(season = c(2022, 2023), horizon = c(1L, 1L), season_week = c(10L, 10L)),
      x_train = tibble(feature = c(1, 2)),
      y_train = c(0.1, 0.2),
      df_test = tibble(horizon = 1L, season_week = 10L),
      x_test = tibble(feature = 3)
    )
  )

  seen_labels <- list()
  run_pargbqr_global(
    split_data_list = split_data,
    feat_names = "feature",
    ref_date = as.Date("2024-01-06"),
    num_bags = 10,
    q_levels = c(0.5),
    bag_frac_samples = 1,
    nrounds = 1,
    learning_rate = 0.05,
    num_leaves = 2,
    min_data_in_leaf = 1,
    feature_fraction = 1,
    lgb_dataset_fn = function(data, label) {
      seen_labels[[length(seen_labels) + 1]] <<- label
      list(data = data, label = label)
    },
    lgb_train_fn = function(params, data, nrounds) list(params = params),
    predict_fn = function(object, newdata, ...) rep(0, nrow(newdata))
  )

  expect_length(seen_labels, 10)
  expect_true(all(vapply(seen_labels, function(label) {
    identical(unname(label), unname(split_data[[1]]$y_train))
  }, logical(1))))
})

test_that("parGBQR individual runner calibrates residuals on out-of-bag rows, not in-bag rows", {
  df_train <- tibble(
    season = rep(c(2020, 2021, 2022, 2023), each = 3),
    horizon = 1L,
    season_week = 10L
  )
  x_train <- tibble(row_id = 1:12)
  y_train <- rep(0, 12)

  split_data <- list(
    "Testland___Overall" = list(
      location = "Testland",
      target_group = "Overall",
      df_train = df_train,
      x_train = x_train,
      y_train = y_train,
      df_test = tibble(horizon = 1L, season_week = 10L),
      x_test = tibble(row_id = 999)
    )
  )

  train_calls <- list()
  predict_calls <- list()

  run_pargbqr_multi_group(
    split_data_list = split_data,
    feat_names = "row_id",
    ref_date = as.Date("2024-01-06"),
    num_bags = 6,
    q_levels = c(0.5),
    bag_frac_samples = 0.5,
    nrounds = 1,
    learning_rate = 0.05,
    num_leaves = 2,
    min_data_in_leaf = 1,
    feature_fraction = 1,
    lgb_dataset_fn = function(data, label) {
      train_calls[[length(train_calls) + 1]] <<- data[, "row_id"]
      list(data = data, label = label)
    },
    lgb_train_fn = function(params, data, nrounds) list(params = params),
    predict_fn = function(object, newdata, ...) {
      predict_calls[[length(predict_calls) + 1]] <<- newdata[, "row_id"]
      rep(0, nrow(newdata))
    }
  )

  expect_length(train_calls, 6)
  # Two predict_fn calls per bag: the calibration call (on train-shaped
  # data) and the forecast call (on the single test row tagged 999).
  expect_length(predict_calls, 12)

  calibration_calls <- Filter(function(x) !(length(x) == 1 && x == 999), predict_calls)
  expect_length(calibration_calls, 6)

  for (b in seq_len(6)) {
    trained_ids <- sort(train_calls[[b]])
    calibrated_ids <- sort(calibration_calls[[b]])

    # Out-of-bag calibration: no row used to train bag b's model should
    # also be used to compute that bag's calibration residuals.
    expect_length(intersect(trained_ids, calibrated_ids), 0)
    # With bag_frac_samples = 0.5 across 4 seasons, every bag holds exactly
    # half the seasons out, so together train + calibration cover all rows.
    expect_equal(sort(c(trained_ids, calibrated_ids)), 1:12)
  }
})

test_that("parGBQR global runner calibrates residuals on out-of-bag rows, not in-bag rows", {
  df_train <- tibble(
    season = rep(c(2020, 2021, 2022, 2023), each = 3),
    horizon = 1L,
    season_week = 10L
  )
  x_train <- tibble(row_id = 1:12)
  y_train <- rep(0, 12)

  split_data <- list(
    "Testland___Overall" = list(
      target_group = "Overall",
      df_train = df_train,
      x_train = x_train,
      y_train = y_train,
      df_test = tibble(horizon = 1L, season_week = 10L),
      x_test = tibble(row_id = 999)
    )
  )

  train_calls <- list()
  predict_calls <- list()

  run_pargbqr_global(
    split_data_list = split_data,
    feat_names = "row_id",
    ref_date = as.Date("2024-01-06"),
    num_bags = 6,
    q_levels = c(0.5),
    bag_frac_samples = 0.5,
    nrounds = 1,
    learning_rate = 0.05,
    num_leaves = 2,
    min_data_in_leaf = 1,
    feature_fraction = 1,
    lgb_dataset_fn = function(data, label) {
      train_calls[[length(train_calls) + 1]] <<- data[, "row_id"]
      list(data = data, label = label)
    },
    lgb_train_fn = function(params, data, nrounds) list(params = params),
    predict_fn = function(object, newdata, ...) {
      predict_calls[[length(predict_calls) + 1]] <<- newdata[, "row_id"]
      rep(0, nrow(newdata))
    }
  )

  expect_length(train_calls, 6)
  calibration_calls <- Filter(function(x) !(length(x) == 1 && x == 999), predict_calls)
  expect_length(calibration_calls, 6)

  for (b in seq_len(6)) {
    trained_ids <- sort(train_calls[[b]])
    calibrated_ids <- sort(calibration_calls[[b]])

    expect_length(intersect(trained_ids, calibrated_ids), 0)
    expect_equal(sort(c(trained_ids, calibrated_ids)), 1:12)
  }
})

test_that("parGBQR global runner falls back to in-bag residuals when a bag holds no season out", {
  df_train <- tibble(
    season = c(2022, 2023),
    horizon = c(1L, 1L),
    season_week = c(10L, 10L)
  )
  x_train <- tibble(row_id = c(1, 2))
  y_train <- c(0, 0)

  split_data <- list(
    "Testland___Overall" = list(
      target_group = "Overall",
      df_train = df_train,
      x_train = x_train,
      y_train = y_train,
      df_test = tibble(horizon = 1L, season_week = 10L),
      x_test = tibble(row_id = 999)
    )
  )

  predict_calls <- list()

  run_pargbqr_global(
    split_data_list = split_data,
    feat_names = "row_id",
    ref_date = as.Date("2024-01-06"),
    num_bags = 3,
    q_levels = c(0.5),
    bag_frac_samples = 1,
    nrounds = 1,
    learning_rate = 0.05,
    num_leaves = 2,
    min_data_in_leaf = 1,
    feature_fraction = 1,
    lgb_dataset_fn = function(data, label) list(data = data, label = label),
    lgb_train_fn = function(params, data, nrounds) list(params = params),
    predict_fn = function(object, newdata, ...) {
      predict_calls[[length(predict_calls) + 1]] <<- newdata[, "row_id"]
      rep(0, nrow(newdata))
    }
  )

  calibration_calls <- Filter(function(x) !(length(x) == 1 && x == 999), predict_calls)
  expect_length(calibration_calls, 3)

  # bag_frac_samples = 1 with only 2 seasons leaves nothing out; the
  # calibration call should still run (on the fallback in-bag rows) rather
  # than erroring on a zero-row matrix.
  for (calls in calibration_calls) {
    expect_equal(sort(calls), c(1, 2))
  }
})

test_that("parGBQR forecast preserves newGBQR schema and quantile order in global and individual modes", {
  clean_data <- tibble(
    date = rep(seq.Date(as.Date("2022-01-01"), by = "1 week", length.out = 120), 2),
    target_group = rep(c("Adults", "Pediatrics"), each = 120),
    value = as.numeric(rep(seq_len(120), 2))
  )

  for (model_type in c("global", "individual")) {
    result <- fit_process_pargbqr(
      clean_data = clean_data,
      fcast_horizon = 2,
      quantiles_needed = c(0.75, 0.25, 0.5),
      seasonality = "E",
      model_type = model_type,
      num_bags = 10,
      nrounds = 1
    )

    expect_named(result, c("horizon", "target_group", "output_type", "output_type_id", "value"))
    expect_equal(unique(result$output_type), "quantile")
    expect_equal(sort(unique(result$horizon)), 1:2)
    expect_true(all(c("Adults", "Pediatrics") %in% unique(result$target_group)))

    ordered <- result |>
      dplyr::arrange(target_group, horizon, as.numeric(output_type_id)) |>
      dplyr::group_by(target_group, horizon) |>
      dplyr::summarise(is_ordered = all(diff(value) >= 0), .groups = "drop")

    expect_true(all(ordered$is_ordered))
  }
})

test_that("parGBQR median forecast matches newGBQR when only the median is requested", {
  clean_data <- tibble(
    date = rep(seq.Date(as.Date("2022-01-01"), by = "1 week", length.out = 120), 2),
    target_group = rep(c("Adults", "Pediatrics"), each = 120),
    value = as.numeric(rep(seq_len(120), 2))
  )

  common_args <- list(
    clean_data = clean_data,
    fcast_horizon = 2,
    quantiles_needed = 0.5,
    seasonality = "E",
    model_type = "global",
    num_bags = 10,
    nrounds = 1
  )

  newgbqr_result <- do.call(fit_process_newgbqr, common_args)
  pargbqr_result <- do.call(fit_process_pargbqr, common_args)

  expect_equal(pargbqr_result, newgbqr_result, tolerance = 1e-8)
})
