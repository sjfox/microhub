test_that("retrospective reference range uses observed weeks and excludes earliest", {
  df <- tibble(
    date = as.Date("2026-01-03") + lubridate::weeks(0:4),
    target_group = "Overall",
    value = 1:5
  )

  expect_equal(
    available_retrospective_reference_dates(df),
    as.Date("2026-01-03") + lubridate::weeks(1:4)
  )

  expect_equal(
    retrospective_reference_range(
      df,
      as.Date("2026-01-17"),
      as.Date("2026-01-31")
    ),
    as.Date(c("2026-01-17", "2026-01-24", "2026-01-31"))
  )
})

test_that("retrospective model choices exclude removed GBQR", {
  expect_false("gbqr" %in% unname(retrospective_model_choices))
  expect_false("GBQR" %in% names(retrospective_model_choices))
})

test_that("retrospective formatter converts model horizons to hub horizons", {
  raw <- tibble(
    horizon = c(1L, 2L),
    target_group = "Overall",
    output_type = "quantile",
    output_type_id = c("0.5", "0.5"),
    value = c(10, 11)
  )

  formatted <- format_retrospective_forecasts(
    raw,
    model_name = "Example Model",
    reference_date = as.Date("2026-02-07")
  )

  expect_equal(formatted$horizon, c(0L, 1L))
  expect_equal(
    formatted$target_end_date,
    as.Date(c("2026-02-07", "2026-02-14"))
  )
  expect_equal(names(formatted), c(
    "model",
    "reference_date",
    "horizon",
    "target_end_date",
    "target_group",
    "output_type",
    "output_type_id",
    "value"
  ))
})

test_that("retrospective scoring summarizes WIS, relative WIS, log WIS, and coverage", {
  quantiles <- c("0.025", "0.25", "0.5", "0.75", "0.975")
  forecast_skeleton <- tidyr::expand_grid(
    reference_date = as.Date(c("2026-01-10", "2026-01-17")),
    target_group = "Overall",
    output_type_id = quantiles
  ) |>
    mutate(
      horizon = 0L,
      target_end_date = reference_date,
      output_type = "quantile"
    )

  model_forecast <- function(model, offsets) {
    forecast_skeleton |>
      mutate(
        model = model,
        value = rep(c(10, 11), each = length(quantiles)) +
          rep(offsets, times = 2)
      ) |>
      select(
        model,
        reference_date,
        horizon,
        target_end_date,
        target_group,
        output_type,
        output_type_id,
        value
      )
  }

  forecasts <- bind_rows(
    model_forecast("Regular Baseline", c(-2, -1, 0, 1, 2)),
    model_forecast("Sharper Model", c(-1, 0, 0, 0, 1)),
    model_forecast("Wide Model", c(-4, -3, 0, 3, 4))
  )
  actual_data <- tibble(
    date = as.Date(c("2026-01-10", "2026-01-17")),
    target_group = "Overall",
    value = c(10, 11)
  )

  scores <- score_retrospective_forecasts(forecasts, actual_data)

  expect_named(scores, c("rows", "overall", "by_target_group", "by_forecast_date"))
  expect_equal(nrow(scores$rows), 6)
  expect_true(all(c(
    "mean_wis",
    "mean_relative_wis",
    "mean_log_wis",
    "mean_relative_log_wis",
    "coverage_50",
    "coverage_95"
  ) %in% names(scores$overall)))

  baseline <- scores$overall |> filter(model == "Regular Baseline")
  sharper <- scores$overall |> filter(model == "Sharper Model")
  wide <- scores$overall |> filter(model == "Wide Model")

  expect_equal(baseline$mean_relative_wis, 1)
  expect_equal(baseline$mean_relative_log_wis, 1)
  expect_lt(sharper$mean_relative_wis, 1)
  expect_lt(sharper$mean_relative_log_wis, 1)
  expect_gt(wide$mean_relative_wis, 1)
  expect_gt(wide$mean_relative_log_wis, 1)
  expect_equal(scores$overall$coverage_50, c(1, 1, 1))
  expect_equal(scores$overall$coverage_95, c(1, 1, 1))
  expect_equal(scores$by_target_group$target_group, rep("Overall", 3))
  expect_equal(unique(scores$by_forecast_date$reference_date), as.Date(c("2026-01-10", "2026-01-17")))
})

test_that("retrospective scoring falls back to direct relative WIS with one non-baseline model", {
  quantiles <- c("0.025", "0.25", "0.5", "0.75", "0.975")
  forecast_skeleton <- tibble(
    reference_date = as.Date("2026-01-10"),
    horizon = 0L,
    target_end_date = as.Date("2026-01-10"),
    target_group = "Overall",
    output_type = "quantile",
    output_type_id = quantiles
  )

  forecasts <- bind_rows(
    forecast_skeleton |>
      mutate(model = "Regular Baseline", value = c(8, 9, 10, 11, 12)),
    forecast_skeleton |>
      mutate(model = "Sharper Model", value = c(9, 10, 10, 10, 11))
  )
  actual_data <- tibble(
    date = as.Date("2026-01-10"),
    target_group = "Overall",
    value = 10
  )

  scores <- score_retrospective_forecasts(forecasts, actual_data)
  baseline <- scores$overall |> filter(model == "Regular Baseline")
  sharper <- scores$overall |> filter(model == "Sharper Model")

  expect_equal(baseline$mean_relative_wis, 1)
  expect_equal(baseline$mean_relative_log_wis, 1)
  expect_equal(sharper$mean_relative_wis, sharper$mean_wis / baseline$mean_wis)
  expect_equal(sharper$mean_relative_log_wis, sharper$mean_log_wis / baseline$mean_log_wis)
  expect_lt(sharper$mean_relative_wis, 1)
})

test_that("retrospective runner automatically ensembles successful non-baseline models", {
  df <- tibble(
    date = as.Date("2026-01-03") + lubridate::weeks(0:2),
    target_group = "Overall",
    value = c(10, 20, 30)
  )

  runner_data <- function(value) {
    tibble(
      horizon = 1L,
      target_group = "Overall",
      output_type = "quantile",
      output_type_id = "0.5",
      value = value
    )
  }

  runners <- list(
    baseline_regular = list(
      label = "Regular Baseline",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        runner_data(20)
      }
    ),
    model_a = list(
      label = "Model A",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        runner_data(10)
      }
    ),
    model_b = list(
      label = "Model B",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        runner_data(30)
      }
    )
  )

  output_dir <- file.path(tempdir(), paste0("retro-ensemble-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  result <- run_retrospective_forecasts(
    data = df,
    reference_dates = as.Date("2026-01-17"),
    models = c("baseline_regular", "model_a", "model_b"),
    horizon = 1,
    seasonality = "E",
    quantiles_needed = c(0.5),
    output_dir = output_dir,
    runners = runners
  )

  expect_true("Ensemble" %in% result$successes$model)
  expect_true("Ensemble" %in% result$forecasts$model)
  expect_true("Ensemble" %in% result$scores$overall$model)

  ensemble_forecast <- result$forecasts |>
    filter(model == "Ensemble")

  expect_equal(nrow(ensemble_forecast), 1)
  expect_equal(ensemble_forecast$value, 20)

  weekly_csv <- readr::read_csv(
    file.path(output_dir, "retrospective_2026-01-17.csv"),
    show_col_types = FALSE
  )
  expect_true("Ensemble" %in% weekly_csv$model)
})

test_that("retrospective ensemble plot data thins forecast origins and keeps intervals", {
  quantiles <- c("0.025", "0.25", "0.5", "0.75", "0.975")
  reference_dates <- as.Date("2026-01-03") + lubridate::weeks(0:6)

  forecasts <- tidyr::expand_grid(
    reference_date = reference_dates,
    horizon = 0:1,
    target_group = "Overall",
    output_type_id = quantiles
  ) |>
    mutate(
      model = "Ensemble",
      target_end_date = reference_date + lubridate::weeks(horizon),
      output_type = "quantile",
      value = 100 + as.integer(reference_date - min(reference_date)) +
        horizon + match(output_type_id, quantiles)
    ) |>
    select(
      model,
      reference_date,
      horizon,
      target_end_date,
      target_group,
      output_type,
      output_type_id,
      value
    )

  actual_data <- tibble(
    date = as.Date("2026-01-03") + lubridate::weeks(0:8),
    target_group = "Overall",
    value = 100 + seq_along(date)
  )

  plot_data <- retrospective_ensemble_plot_data(
    forecasts = forecasts,
    actual_data = actual_data,
    forecast_stride = 3L
  )

  expect_equal(
    unique(plot_data$forecast$reference_date),
    reference_dates[c(1, 4, 7)]
  )
  expect_true(all(c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975") %in% names(plot_data$forecast)))
  expect_equal(nrow(plot_data$forecast), 6)
  expect_equal(
    range(plot_data$actual$date),
    range(plot_data$forecast$target_end_date)
  )
})

test_that("retrospective forecast plot data falls back to a single model without ensemble", {
  quantiles <- c("0.025", "0.25", "0.5", "0.75", "0.975")
  reference_dates <- as.Date("2026-01-03") + lubridate::weeks(0:3)

  forecasts <- tidyr::expand_grid(
    reference_date = reference_dates,
    horizon = 0:1,
    target_group = "Overall",
    output_type_id = quantiles
  ) |>
    mutate(
      model = "Copycat",
      target_end_date = reference_date + lubridate::weeks(horizon),
      output_type = "quantile",
      value = 100 + as.integer(reference_date - min(reference_date)) +
        horizon + match(output_type_id, quantiles)
    ) |>
    select(
      model,
      reference_date,
      horizon,
      target_end_date,
      target_group,
      output_type,
      output_type_id,
      value
    )

  actual_data <- tibble(
    date = as.Date("2026-01-03") + lubridate::weeks(0:5),
    target_group = "Overall",
    value = 100 + seq_along(date)
  )

  plot_data <- retrospective_ensemble_plot_data(
    forecasts = forecasts,
    actual_data = actual_data,
    forecast_stride = 3L
  )

  expect_equal(plot_data$model, "Copycat")
  expect_false(plot_data$is_ensemble)
  expect_equal(
    unique(plot_data$forecast$reference_date),
    reference_dates[c(1, 4)]
  )
  expect_equal(nrow(plot_data$forecast), 4)
})

test_that("target group score plot orders models and removes regular baseline", {
  score_tbl <- tibble(
    target_group = rep(c("Overall", "Adult"), each = 4),
    model = rep(c("Regular Baseline", "Model A", "Seasonal Baseline", "Model B"), times = 2),
    mean_wis = c(10, 12, 11, 8, 10, 9, 12, 7),
    mean_relative_wis = c(1, 1.2, 1.1, 0.8, 1, 0.9, 1.2, 0.7),
    mean_log_wis = 1,
    mean_relative_log_wis = 1,
    coverage_50 = 1,
    coverage_95 = 1,
    n_forecast_targets = 2
  )

  plot <- plot_retrospective_target_group_scores(score_tbl)

  expect_false("Regular Baseline" %in% as.character(plot$data$model))
  expect_equal(
    levels(plot$data$model),
    rev(c("Model B", "Seasonal Baseline", "Model A"))
  )
  expect_equal(plot$theme$axis.text.x$angle, 0)
})

test_that("forecast date score plot removes regular baseline and greys baseline models", {
  score_tbl <- tibble(
    reference_date = rep(as.Date("2026-01-03") + lubridate::weeks(0:1), each = 4),
    model = rep(c("Regular Baseline", "Seasonal Baseline", "Opt Baseline", "Model A"), times = 2),
    mean_wis = c(10, 11, 12, 9, 10, 12, 11, 8),
    mean_relative_wis = c(1, 1.1, 1.2, 0.9, 1, 1.2, 1.1, 0.8),
    mean_log_wis = 1,
    mean_relative_log_wis = 1,
    coverage_50 = 1,
    coverage_95 = 1,
    n_forecast_targets = 2
  )

  plot <- plot_retrospective_forecast_date_scores(score_tbl)
  built_plot <- ggplot2::ggplot_build(plot)$plot
  color_scale <- built_plot$scales$get_scales("colour")

  expect_false("Regular Baseline" %in% plot$data$model)
  expect_true(all(c("Seasonal Baseline", "Opt Baseline", "Model A") %in% plot$data$model))
  expect_match(color_scale$palette.cache[["Seasonal Baseline"]], "^#([0-9A-F]{2})\\1\\1$")
  expect_match(color_scale$palette.cache[["Opt Baseline"]], "^#([0-9A-F]{2})\\1\\1$")
})

test_that("retrospective runner writes weekly CSVs and continues after failures", {
  df <- tidyr::expand_grid(
    date = as.Date("2026-01-03") + lubridate::weeks(0:3),
    target_group = c("Overall", "Adult")
  ) |>
    mutate(value = dplyr::row_number())

  runner_data <- function(label) {
    tibble(
      horizon = c(1L, 2L),
      target_group = "Overall",
      output_type = "quantile",
      output_type_id = c("0.5", "0.5"),
      value = c(100, 101)
    )
  }

  seen_training_dates <- list()
  runners <- list(
    model_a = list(
      label = "Model A",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        seen_training_dates[[length(seen_training_dates) + 1]] <<- max(train_data$date)
        runner_data("Model A")
      }
    ),
    model_b = list(
      label = "Model B",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        stop("intentional failure")
      }
    )
  )

  output_dir <- file.path(tempdir(), paste0("retro-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  result <- run_retrospective_forecasts(
    data = df,
    reference_dates = as.Date(c("2026-01-17", "2026-01-24")),
    models = c("model_a", "model_b"),
    horizon = 2,
    seasonality = "E",
    quantiles_needed = c(0.5),
    output_dir = output_dir,
    runners = runners
  )

  expect_equal(nrow(result$files), 2)
  expect_equal(nrow(result$successes), 2)
  expect_equal(nrow(result$failures), 2)
  expect_true(file.exists(file.path(output_dir, "retrospective_2026-01-17.csv")))
  expect_true(file.exists(file.path(output_dir, "retrospective_2026-01-24.csv")))
  expect_true(file.exists(file.path(output_dir, "retrospective_failures.csv")))
  expect_true(file.exists(result$zip_path))
  expect_equal(
    seen_training_dates,
    list(as.Date("2026-01-10"), as.Date("2026-01-17"))
  )

  weekly_csv <- readr::read_csv(
    file.path(output_dir, "retrospective_2026-01-17.csv"),
    show_col_types = FALSE
  )
  expect_equal(unique(weekly_csv$model), "Model A")
  expect_equal(weekly_csv$horizon, c(0L, 1L))
})

test_that("retrospective runner returns stable empty tables when all models fail", {
  df <- tibble(
    date = as.Date("2026-01-03") + lubridate::weeks(0:1),
    target_group = "Overall",
    value = c(1, 2)
  )
  runners <- list(
    model_a = list(
      label = "Model A",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        stop("all failed")
      }
    )
  )

  output_dir <- file.path(tempdir(), paste0("retro-fail-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  result <- run_retrospective_forecasts(
    data = df,
    reference_dates = as.Date("2026-01-10"),
    models = "model_a",
    horizon = 1,
    seasonality = "E",
    quantiles_needed = c(0.5),
    output_dir = output_dir,
    runners = runners
  )

  expect_named(result$files, c("reference_date", "file", "rows"))
  expect_named(result$successes, c("reference_date", "model", "rows"))
  expect_named(result$failures, c("reference_date", "model", "message"))
  expect_equal(nrow(result$files), 0)
  expect_equal(nrow(result$successes), 0)
  expect_equal(nrow(result$failures), 1)
  expect_true(file.exists(file.path(output_dir, "retrospective_failures.csv")))
  expect_true(file.exists(result$zip_path))
})
