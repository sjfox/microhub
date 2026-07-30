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

test_that("plot_forecasts facets target groups and limits recent comparison data", {
  groups <- paste("Group", 1:5)
  data_dates <- seq.Date(as.Date("2024-01-06"), by = "1 week", length.out = 130)
  data_df <- tidyr::expand_grid(
    date = data_dates,
    target_group = groups
  ) |>
    mutate(
      value = dplyr::row_number(),
      dropped_week = FALSE
    )

  reference_date <- max(data_dates)
  forecast_dates <- reference_date + lubridate::weeks(1:4)
  quantiles <- c("0.025", "0.25", "0.5", "0.75", "0.975")
  forecast_df <- tidyr::expand_grid(
    target_group = groups,
    horizon = 0:3,
    output_type_id = quantiles
  ) |>
    mutate(
      target_end_date = forecast_dates[horizon + 1L],
      output_type = "quantile",
      value = 100 + horizon + match(output_type_id, quantiles)
    )

  plot_obj <- plot_forecasts(
    forecast_df = forecast_df,
    data_df = data_df,
    seasonality = "E"
  )

  current_points <- plot_obj$layers[[5]]$data
  prior_year_points <- plot_obj$layers[[4]]$data

  expect_s3_class(plot_obj, "ggplot")
  expect_equal(plot_obj$facet$params$ncol, 3)
  expect_equal(
    current_points |>
      count(target_group) |>
      pull(n),
    rep(8L, length(groups))
  )
  expect_true(all(prior_year_points$date >= min(current_points$date)))
  expect_true(all(prior_year_points$date <= max(forecast_dates)))
})

test_that("plot_forecasts keeps December observations and prior-year comparison in early January", {
  data_dates <- seq.Date(as.Date("2024-11-09"), as.Date("2026-01-03"), by = "1 week")
  data_df <- tibble(
    date = data_dates,
    target_group = "Overall",
    value = seq_along(data_dates),
    dropped_week = FALSE
  )

  forecast_dates <- as.Date("2026-01-10") + lubridate::weeks(0:3)
  quantiles <- c("0.025", "0.25", "0.5", "0.75", "0.975")
  forecast_df <- tidyr::expand_grid(
    target_group = "Overall",
    horizon = 0:3,
    output_type_id = quantiles
  ) |>
    mutate(
      target_end_date = forecast_dates[horizon + 1L],
      output_type = "quantile",
      value = 100 + horizon + match(output_type_id, quantiles)
    )

  plot_obj <- plot_forecasts(
    forecast_df = forecast_df,
    data_df = data_df,
    seasonality = "E"
  )

  current_points <- plot_obj$layers[[5]]$data
  prior_year_points <- plot_obj$layers[[4]]$data

  expect_equal(nrow(current_points), 8L)
  expect_true(any(lubridate::month(current_points$date) == 12L))
  expect_true(any(lubridate::year(prior_year_points$date - lubridate::weeks(52)) == 2024L))
  expect_true(any(lubridate::year(prior_year_points$date - lubridate::weeks(52)) == 2025L))
  expect_true(all(prior_year_points$date >= min(current_points$date)))
  expect_true(all(prior_year_points$date <= max(forecast_dates)))
})

test_that("uploaded respiratory season plot mutes history and highlights current season", {
  raw_data <- tibble(
    date = seq.Date(as.Date("2024-01-06"), by = "1 week", length.out = 120),
    target_group = "Overall",
    value = seq_len(120)
  )

  plot_obj <- plot_uploaded_resp_season_series(
    raw_data = raw_data,
    forecast_date = max(raw_data$date),
    seasonality = "E",
    data_to_drop = "0 weeks"
  )

  built <- ggplot_build(plot_obj)

  expect_s3_class(plot_obj, "ggplot")
  expect_equal(unique(built$data[[1]]$colour), "#B8C0CC")
  expect_equal(unique(built$data[[3]]$colour), "#002454")
})

test_that("uploaded respiratory season plot highlights dropped weeks", {
  raw_data <- tibble(
    date = seq.Date(as.Date("2024-01-06"), by = "1 week", length.out = 120),
    target_group = "Overall",
    value = seq_len(120)
  )

  plot_obj <- plot_uploaded_resp_season_series(
    raw_data = raw_data,
    forecast_date = max(raw_data$date),
    seasonality = "E",
    data_to_drop = "2 week"
  )

  built <- ggplot_build(plot_obj)

  expect_equal(unique(built$data[[5]]$colour), "#D94841")
  expect_equal(nrow(built$data[[5]]), 2)
})

test_that("uploaded data plots can show one selected target group", {
  raw_data <- tidyr::expand_grid(
    date = seq.Date(as.Date("2024-01-06"), by = "1 week", length.out = 120),
    target_group = c("Overall", "Adult")
  ) |>
    mutate(value = dplyr::row_number())

  time_plot <- plot_uploaded_time_series(
    raw_data = raw_data,
    forecast_date = max(raw_data$date),
    data_to_drop = "0 weeks",
    target_group = "Adult"
  )
  season_plot <- plot_uploaded_resp_season_series(
    raw_data = raw_data,
    forecast_date = max(raw_data$date),
    seasonality = "E",
    data_to_drop = "0 weeks",
    target_group = "Adult"
  )

  expect_equal(unique(time_plot$data$target_group), "Adult")
  expect_equal(unique(season_plot$data$target_group), "Adult")
  expect_s3_class(time_plot$facet, "FacetNull")
  expect_s3_class(season_plot$facet, "FacetNull")
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
