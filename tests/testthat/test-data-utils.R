test_that("read_raw_data preserves optional population data", {
  csv_path <- withr::local_tempfile(fileext = ".csv")

  writeLines(
    c(
      "date,target_group,value,population",
      "2024-01-03,Overall,10,1000",
      "2024-01-10,Overall,12,1000"
    ),
    csv_path
  )

  result <- read_raw_data(csv_path)

  expect_true("population" %in% names(result))
  expect_equal(result$population, c(1000, 1000))
  expect_s3_class(result$date, "Date")
})

test_that("format_forecasts can retain only non-negative horizons", {
  forecast_df <- tibble(
    horizon = c(1, 2, 3),
    target_group = rep("Overall", 3),
    output_type = rep("quantile", 3),
    output_type_id = c("0.025", "0.5", "0.975"),
    value = c(10, 12, 14)
  )

  data_df <- tibble(
    date = as.Date(c("2024-01-03", "2024-01-10", "2024-01-17")),
    target_group = "Overall",
    value = c(7, 8, 9)
  )

  result <- format_forecasts(
    forecast_df = forecast_df,
    model_name = "Example Model",
    data_df = data_df,
    data_to_drop = "0 weeks",
    forecast_date = as.Date("2024-01-10"),
    forecast_output = "horizon_gte_0"
  )

  expect_true(all(result$horizon >= 0))
  expect_equal(unique(result$model), "Example Model")
})
