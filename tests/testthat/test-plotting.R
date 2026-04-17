test_that("plot_forecasts returns a ggplot for current app-shaped data", {
  weekly_dates <- seq.Date(as.Date("2024-01-03"), by = "1 week", length.out = 8)

  data_df <- tibble(
    date = c(weekly_dates - weeks(52), weekly_dates),
    target_group = "Overall",
    value = c(8, 9, 10, 11, 9, 8, 7, 6, 10, 11, 12, 13, 14, 15, 16, 17),
    dropped_week = FALSE
  )

  forecast_dates <- seq.Date(max(weekly_dates) + weeks(1), by = "1 week", length.out = 2)
  quantiles <- c("0.025", "0.25", "0.5", "0.75", "0.975")
  forecast_values <- c(14, 15, 16, 17, 18, 15, 16, 17, 18, 19)

  forecast_df <- tibble(
    target_group = "Overall",
    target_end_date = rep(forecast_dates, each = length(quantiles)),
    horizon = rep(c(0, 1), each = length(quantiles)),
    output_type = "quantile",
    output_type_id = rep(quantiles, times = 2),
    value = forecast_values
  )

  plot_obj <- plot_forecasts(
    target_name = "Overall",
    forecast_df = forecast_df,
    data_df = data_df,
    seasonality = "E"
  )

  expect_s3_class(plot_obj, "ggplot")
})
