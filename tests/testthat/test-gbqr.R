test_that("GBQR wrangling uses rates when population is available", {
  clean_data <- tibble(
    date = seq.Date(as.Date("2024-01-06"), by = "1 week", length.out = 8),
    target_group = rep("Overall", 8),
    value = c(10, 12, 15, 18, 20, 17, 14, 13),
    population = rep(100000, 8)
  )

  gbqr_df <- wrangle_gbqr_for_app(
    clean_data = clean_data,
    seasonality = "E"
  )

  feat_out <- preprocess_and_prepare_features(
    df = gbqr_df,
    forecast_horizons = 1:2,
    peak_week = default_gbqr_peak_week("E")
  )

  expected_rate <- clean_data$value[1] / clean_data$population[1] * 100000
  expected_inc_4rt <- (expected_rate + 0.01 + 0.75^4)^0.25

  expect_true(all(gbqr_df$uses_population))
  expect_equal(gbqr_df$inc[1], clean_data$value[1])
  expect_equal(gbqr_df$population[1], clean_data$population[1])
  expect_equal(gbqr_df$log_pop[1], log(clean_data$population[1]))
  expect_equal(
    unname(
      (gbqr_df$inc_4rt_cs[1] + gbqr_df$inc_4rt_center_factor[1]) *
        (gbqr_df$inc_4rt_scale_factor[1] + 0.01)
    ),
    expected_inc_4rt,
    tolerance = 1e-8
  )
  expect_true("log_pop" %in% feat_out$feature_names)
  expect_false(any(grepl("^log_pop_lag", feat_out$feature_names)))
  expect_true(all(is.finite(gbqr_df$log_pop)))
})

test_that("GBQR wrangling stays population-optional", {
  clean_data <- tibble(
    date = seq.Date(as.Date("2024-01-06"), by = "1 week", length.out = 6),
    target_group = rep("Overall", 6),
    value = c(4, 5, 6, 7, 8, 9)
  )

  gbqr_df <- wrangle_gbqr_for_app(
    clean_data = clean_data,
    seasonality = "E"
  )

  feat_out <- preprocess_and_prepare_features(
    df = gbqr_df,
    forecast_horizons = 1,
    peak_week = default_gbqr_peak_week("E")
  )

  expect_true(all(!gbqr_df$uses_population))
  expect_true("log_pop" %in% names(gbqr_df))
  expect_true(all(is.na(gbqr_df$log_pop)))
  expect_false("log_pop" %in% feat_out$feature_names)
})

test_that("GBQR back-transforms modeled rates to counts when population is available", {
  q_labels <- c("0.5")
  split_data <- list(
    "Paraguay___Overall" = list(
      df_test = tibble(
        wk_end_date = as.Date("2024-03-02"),
        location = "Paraguay",
      target_group = "Overall",
      population = 200000,
      uses_population = TRUE,
      inc_4rt_cs = 1,
      horizon = 1L,
      inc_4rt_center_factor = 0,
      inc_4rt_scale_factor = 1
      )
    )
  )

  test_preds_by_group <- list(
    "Paraguay___Overall" = array(0, dim = c(1, 1, 1))
  )

  result <- process_and_combine_gbqr_forecasts(
    test_preds_by_group = test_preds_by_group,
    split_data = split_data,
    q_labels = q_labels
  )

  expected_rate <- (1 * 1.01)^4 - 0.01 - 0.75^4
  expected_count <- expected_rate * 200000 / 100000

  expect_equal(result$value, expected_count, tolerance = 1e-8)
})
