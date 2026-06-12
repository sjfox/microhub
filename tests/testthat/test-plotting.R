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

test_that("write_model_plots_pdf writes available plots to a PDF", {
  pdf_path <- withr::local_tempfile(fileext = ".pdf")
  plot_specs <- list(
    list(
      name = "Plot 1",
      plot = ggplot(tibble(x = 1:3, y = 1:3), aes(x, y)) + geom_line()
    ),
    list(
      name = "Plot 2",
      plot = ggplot(tibble(x = 1:3, y = c(3, 2, 1)), aes(x, y)) + geom_point()
    )
  )

  write_model_plots_pdf(plot_specs, pdf_path)

  expect_true(file.exists(pdf_path))
  expect_gt(file.info(pdf_path)$size, 0)
})

test_that("write_model_plots_pdf requires at least one plot", {
  expect_error(
    write_model_plots_pdf(list(), withr::local_tempfile(fileext = ".pdf")),
    "No model plots are available"
  )
})
