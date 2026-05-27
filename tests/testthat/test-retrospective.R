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
