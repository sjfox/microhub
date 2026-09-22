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

test_that("retrospective model choices include parGBQR without defaulting it on", {
  expect_equal(unname(retrospective_model_choices["parGBQR"]), "pargbqr")
  expect_false("pargbqr" %in% unname(retrospective_default_model_choices))
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

test_that("retrospective summary relative WIS follows summarized WIS", {
  score_rows <- tibble(
    model = rep(c("Regular Baseline", "Individual Model"), each = 2),
    reference_date = rep(as.Date(c("2026-01-10", "2026-01-17")), times = 2),
    horizon = 0L,
    target_end_date = rep(as.Date(c("2026-01-10", "2026-01-17")), times = 2),
    target_group = "Overall",
    actual = 1,
    wis = c(1, 100, 2, 98),
    relative_wis = NA_real_,
    log_wis = c(1, 100, 2, 98),
    relative_log_wis = NA_real_,
    weighted_interval_score_50 = NA_real_,
    covered_50 = TRUE,
    weighted_interval_score_95 = NA_real_,
    covered_95 = TRUE
  )

  summary <- summarize_retrospective_score_rows(
    score_rows,
    reference_model = "Regular Baseline"
  )

  baseline <- summary |> filter(model == "Regular Baseline")
  individual <- summary |> filter(model == "Individual Model")

  expect_lt(individual$mean_wis, baseline$mean_wis)
  expect_equal(individual$mean_relative_wis, individual$mean_wis / baseline$mean_wis)
  expect_lt(individual$mean_relative_wis, 1)
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

test_that("retrospective runner supports multiple parameter configurations for one model", {
  df <- tibble(
    date = as.Date("2026-01-03") + lubridate::weeks(0:2),
    target_group = "Overall",
    value = c(10, 20, 30)
  )

  seen_params <- list()
  runners <- list(
    copycat = list(
      label = "Copycat",
      run = function(train_data, horizon, quantiles_needed, seasonality, params) {
        seen_params[[length(seen_params) + 1]] <<- params
        tibble(
          horizon = 1L,
          target_group = "Overall",
          output_type = "quantile",
          output_type_id = "0.5",
          value = params$recent_weeks_touse
        )
      }
    )
  )

  run_configs <- tibble(
    run_id = c("copycat_100", "copycat_10"),
    model_id = c("copycat", "copycat"),
    model_label = c("Copycat", "Copycat"),
    run_label = c("Copycat weeks 100", "Copycat weeks 10"),
    params = list(
      list(recent_weeks_touse = 100L, resp_week_range = 2L, share_groups = TRUE),
      list(recent_weeks_touse = 10L, resp_week_range = 2L, share_groups = TRUE)
    )
  )

  output_dir <- file.path(tempdir(), paste0("retro-config-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  result <- run_retrospective_forecasts(
    data = df,
    reference_dates = as.Date("2026-01-17"),
    horizon = 1,
    seasonality = "E",
    quantiles_needed = c(0.5),
    output_dir = output_dir,
    run_configs = run_configs,
    auto_ensemble = FALSE,
    runners = runners
  )

  expect_equal(result$forecasts$model, c("Copycat weeks 100", "Copycat weeks 10"))
  expect_equal(result$forecasts$value, c(100, 10))
  expect_equal(vapply(seen_params, `[[`, integer(1), "recent_weeks_touse"), c(100L, 10L))
  expect_false("Ensemble" %in% result$forecasts$model)
  expect_true(file.exists(file.path(output_dir, "retrospective_run_configs.csv")))
})

test_that("retrospective default settings exactly match model-specific tab parameters", {
  settings <- retrospective_default_settings(has_population = TRUE)

  expect_length(settings$baseline_regular, 0)
  expect_length(settings$baseline_seasonal, 0)
  expect_length(settings$baseline_opt, 0)
  expect_equal(names(settings$inla), c("forecast_uncertainty", "use_offset"))
  expect_equal(names(settings$copycat), c(
    "recent_weeks_touse",
    "resp_week_range",
    "share_groups",
    "weight_exponent",
    "add_poisson_noise",
    "points_per_knot",
    "max_matches"
  ))
  expect_equal(names(settings$calcopycat), c(
    "recent_weeks_touse",
    "resp_week_range",
    "share_groups"
  ))
  expect_equal(names(settings$newgbqr), c("model_type", "num_bags", "nrounds", "num_leaves"))
  expect_equal(names(settings$pargbqr), c("model_type", "num_bags", "nrounds", "num_leaves"))
  expect_length(settings$starima, 0)
  expect_length(settings$fourcat, 0)
})

test_that("retrospective validation rejects non-tab parameters", {
  expect_error(
    retrospective_validate_model_params(
      "newgbqr",
      list(peak_week_method = "fixed")
    ),
    "Unknown parameter"
  )
  expect_error(
    retrospective_validate_model_params(
      "pargbqr",
      list(peak_week_method = "fixed")
    ),
    "Unknown parameter"
  )
  expect_error(
    retrospective_validate_model_params(
      "copycat",
      list(nsamps = 100L)
    ),
    "Unknown parameter"
  )
})

test_that("retrospective parGBQR runner forwards only tab-configured parameters", {
  settings <- retrospective_default_settings()
  settings$pargbqr$num_bags <- 12L
  settings$pargbqr$model_type <- "individual"
  settings$pargbqr$nrounds <- 150L
  settings$pargbqr$num_leaves <- 21L

  captured <- NULL
  original_fit_process_pargbqr <- fit_process_pargbqr
  fit_process_pargbqr <<- function(clean_data,
                                   fcast_horizon = NULL,
                                   quantiles_needed = NULL,
                                   seasonality = NULL,
                                   country = "Paraguay",
                                   peak_week_method = c("empirical", "zone", "fixed"),
                                   peak_week = NULL,
                                   num_bags = 50,
                                   bag_frac_samples = 0.7,
                                   nrounds = 100,
                                   model_type = "individual",
                                   rate_per = 100000,
                                   min_train_rows = 12,
                                   learning_rate = 0.05,
                                   num_leaves = 11,
                                   min_data_in_leaf = 8,
                                   feature_fraction = 0.8,
                                   ...) {
    captured <<- list(
      fcast_horizon = fcast_horizon,
      quantiles_needed = quantiles_needed,
      seasonality = seasonality,
      country = country,
      peak_week_method = peak_week_method,
      peak_week = peak_week,
      num_bags = num_bags,
      bag_frac_samples = bag_frac_samples,
      nrounds = nrounds,
      model_type = model_type,
      rate_per = rate_per,
      min_train_rows = min_train_rows,
      learning_rate = learning_rate,
      num_leaves = num_leaves,
      min_data_in_leaf = min_data_in_leaf,
      feature_fraction = feature_fraction
    )
    tibble(
      horizon = 1L,
      target_group = "Overall",
      output_type = "quantile",
      output_type_id = "0.5",
      value = 1
    )
  }
  on.exit(fit_process_pargbqr <<- original_fit_process_pargbqr, add = TRUE)

  runner <- retrospective_model_runners(settings)$pargbqr$run
  runner(
    train_data = tibble(
      date = as.Date("2026-01-03"),
      target_group = "Overall",
      value = 1
    ),
    horizon = 2L,
    quantiles_needed = c(0.25, 0.5, 0.75),
    seasonality = "E"
  )

  expect_equal(captured$fcast_horizon, 2L)
  expect_equal(captured$quantiles_needed, c(0.25, 0.5, 0.75))
  expect_equal(captured$seasonality, "E")
  expect_equal(captured$peak_week_method, c("empirical", "zone", "fixed"))
  expect_null(captured$peak_week)
  expect_equal(captured$num_bags, 12L)
  expect_equal(captured$model_type, "individual")
  expect_equal(captured$bag_frac_samples, 0.7)
  expect_equal(captured$nrounds, 150L)
  expect_equal(captured$learning_rate, 0.05)
  expect_equal(captured$num_leaves, 21L)
  expect_equal(captured$min_data_in_leaf, 8)
  expect_equal(captured$feature_fraction, 0.8)
})

test_that("retrospective newGBQR runner forwards only tab-configured parameters", {
  settings <- retrospective_default_settings()
  settings$newgbqr$num_bags <- 12L
  settings$newgbqr$model_type <- "individual"
  settings$newgbqr$nrounds <- 150L
  settings$newgbqr$num_leaves <- 21L

  captured <- NULL
  original_fit_process_newgbqr <- fit_process_newgbqr
  fit_process_newgbqr <<- function(clean_data,
                                   fcast_horizon = NULL,
                                   quantiles_needed = NULL,
                                   seasonality = NULL,
                                   country = "Paraguay",
                                   peak_week_method = c("empirical", "zone", "fixed"),
                                   peak_week = NULL,
                                   num_bags = 50,
                                   bag_frac_samples = 0.7,
                                   nrounds = 50,
                                   model_type = "individual",
                                   rate_per = 100000,
                                   min_train_rows = 12,
                                   learning_rate = 0.05,
                                   num_leaves = 11,
                                   min_data_in_leaf = 8,
                                   feature_fraction = 0.8,
                                   ...) {
    captured <<- list(
      fcast_horizon = fcast_horizon,
      quantiles_needed = quantiles_needed,
      seasonality = seasonality,
      country = country,
      peak_week_method = peak_week_method,
      peak_week = peak_week,
      num_bags = num_bags,
      bag_frac_samples = bag_frac_samples,
      nrounds = nrounds,
      model_type = model_type,
      rate_per = rate_per,
      min_train_rows = min_train_rows,
      learning_rate = learning_rate,
      num_leaves = num_leaves,
      min_data_in_leaf = min_data_in_leaf,
      feature_fraction = feature_fraction
    )
    tibble(
      horizon = 1L,
      target_group = "Overall",
      output_type = "quantile",
      output_type_id = "0.5",
      value = 1
    )
  }
  on.exit(fit_process_newgbqr <<- original_fit_process_newgbqr, add = TRUE)

  runner <- retrospective_model_runners(settings)$newgbqr$run
  runner(
    train_data = tibble(
      date = as.Date("2026-01-03"),
      target_group = "Overall",
      value = 1
    ),
    horizon = 2L,
    quantiles_needed = c(0.25, 0.5, 0.75),
    seasonality = "E"
  )

  expect_equal(captured$fcast_horizon, 2L)
  expect_equal(captured$quantiles_needed, c(0.25, 0.5, 0.75))
  expect_equal(captured$seasonality, "E")
  expect_equal(captured$peak_week_method, c("empirical", "zone", "fixed"))
  expect_null(captured$peak_week)
  expect_equal(captured$num_bags, 12L)
  expect_equal(captured$model_type, "individual")
  expect_equal(captured$bag_frac_samples, 0.7)
  expect_equal(captured$nrounds, 150L)
  expect_equal(captured$learning_rate, 0.05)
  expect_equal(captured$num_leaves, 21L)
  expect_equal(captured$min_data_in_leaf, 8)
  expect_equal(captured$feature_fraction, 0.8)
})

test_that("parameterized regular baseline is used as default scoring reference", {
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
      run = function(train_data, horizon, quantiles_needed, seasonality, params) {
        runner_data(20)
      }
    ),
    copycat = list(
      label = "Copycat",
      run = function(train_data, horizon, quantiles_needed, seasonality, params) {
        runner_data(30)
      }
    )
  )

  run_configs <- tibble(
    run_id = c("baseline_regular_default", "copycat_default"),
    model_id = c("baseline_regular", "copycat"),
    model_label = c("Regular Baseline", "Copycat"),
    run_label = c("Regular Baseline", "Copycat default"),
    params = list(
      list(),
      list(recent_weeks_touse = 100L, resp_week_range = 2L, share_groups = TRUE)
    )
  )

  output_dir <- file.path(tempdir(), paste0("retro-reference-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  result <- run_retrospective_forecasts(
    data = df,
    reference_dates = as.Date("2026-01-17"),
    horizon = 1,
    seasonality = "E",
    quantiles_needed = c(0.5),
    output_dir = output_dir,
    run_configs = run_configs,
    auto_ensemble = FALSE,
    runners = runners
  )

  expect_equal(result$scoring_reference, "Regular Baseline")
  baseline_score <- result$scores$overall |>
    filter(model == "Regular Baseline")
  expect_equal(baseline_score$mean_relative_wis, 1)
  metadata <- readr::read_csv(
    file.path(output_dir, "retrospective_analysis_metadata.csv"),
    show_col_types = FALSE
  )
  expect_equal(
    metadata$value[metadata$field == "scoring_reference"],
    "Regular Baseline"
  )
})

test_that("retrospective reference models exclude ensemble forecasts", {
  forecasts <- tibble(
    model = c("Regular Baseline", "Copycat", "Ensemble"),
    reference_date = as.Date("2026-01-10"),
    horizon = 0L,
    target_end_date = as.Date("2026-01-10"),
    target_group = "Overall",
    output_type = "quantile",
    output_type_id = "0.5",
    value = c(10, 11, 10.5)
  )

  expect_equal(
    retrospective_available_reference_models(forecasts),
    c("Copycat", "Regular Baseline")
  )
  expect_equal(
    retrospective_resolve_reference_model(
      forecasts,
      requested_reference_model = "Ensemble"
    ),
    "Regular Baseline"
  )
})

test_that("retrospective runner builds ensemble only from selected configuration labels", {
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
    copycat = list(
      label = "Copycat",
      run = function(train_data, horizon, quantiles_needed, seasonality, params) {
        runner_data(params$recent_weeks_touse)
      }
    )
  )

  run_configs <- tibble(
    run_id = c("copycat_100", "copycat_10", "copycat_50"),
    model_id = rep("copycat", 3),
    model_label = rep("Copycat", 3),
    run_label = c("Copycat weeks 100", "Copycat weeks 10", "Copycat weeks 50"),
    params = list(
      list(recent_weeks_touse = 100L, resp_week_range = 2L, share_groups = TRUE),
      list(recent_weeks_touse = 10L, resp_week_range = 2L, share_groups = TRUE),
      list(recent_weeks_touse = 50L, resp_week_range = 2L, share_groups = TRUE)
    )
  )

  output_dir <- file.path(tempdir(), paste0("retro-selected-ensemble-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  result <- run_retrospective_forecasts(
    data = df,
    reference_dates = as.Date("2026-01-17"),
    horizon = 1,
    seasonality = "E",
    quantiles_needed = c(0.5),
    output_dir = output_dir,
    run_configs = run_configs,
    ensemble_models = c("Copycat weeks 100", "Copycat weeks 10"),
    auto_ensemble = FALSE,
    runners = runners
  )

  ensemble_forecast <- result$forecasts |>
    filter(model == "Ensemble")

  expect_equal(nrow(ensemble_forecast), 1)
  expect_equal(ensemble_forecast$value, 55)
  expect_true(file.exists(file.path(output_dir, "retrospective_ensemble_members.csv")))
  expect_false("Copycat weeks 50" %in% result$ensemble_models)
})

test_that("retrospective ensemble requires at least two selected members", {
  forecasts <- tibble(
    model = c("Regular Baseline", "Copycat"),
    reference_date = as.Date("2026-01-10"),
    horizon = 0L,
    target_end_date = as.Date("2026-01-10"),
    target_group = "Overall",
    output_type = "quantile",
    output_type_id = "0.5",
    value = c(10, 12)
  )

  expect_null(build_retrospective_ensemble(
    forecasts,
    ensemble_members = c("Copycat")
  ))
})

test_that("retrospective ensemble includes a baseline model only when explicitly selected", {
  forecasts <- tibble(
    model = c("Regular Baseline", "Copycat", "newGBQR"),
    reference_date = as.Date("2026-01-10"),
    horizon = 0L,
    target_end_date = as.Date("2026-01-10"),
    target_group = "Overall",
    output_type = "quantile",
    output_type_id = "0.5",
    value = c(10, 12, 14)
  )

  # Not selected -> excluded, same as today.
  non_baseline_ensemble <- build_retrospective_ensemble(
    forecasts,
    ensemble_members = c("Copycat", "newGBQR")
  )
  expect_equal(non_baseline_ensemble$value, median(c(12, 14)))

  # Explicitly selected -> included, mirroring the live Ensemble tab's
  # "selectable, just not the default" treatment of baseline models.
  with_baseline_ensemble <- build_retrospective_ensemble(
    forecasts,
    ensemble_members = c("Regular Baseline", "Copycat")
  )
  expect_equal(with_baseline_ensemble$value, median(c(10, 12)))

  # build_retrospective_nonbaseline_ensemble() -- the automatic/no-selection
  # path -- still excludes baselines regardless.
  auto_ensemble <- build_retrospective_nonbaseline_ensemble(list(forecasts))
  expect_equal(auto_ensemble$value, median(c(12, 14)))
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

test_that("retrospective forecast plot prefers the only non-baseline model over ensemble", {
  quantiles <- c("0.025", "0.25", "0.5", "0.75", "0.975")
  reference_dates <- as.Date("2026-01-03") + lubridate::weeks(0:1)

  forecasts <- tidyr::expand_grid(
    model = c("Regular Baseline", "parGBQR", "Ensemble"),
    reference_date = reference_dates,
    horizon = 0L,
    target_group = "Overall",
    output_type_id = quantiles
  ) |>
    mutate(
      target_end_date = reference_date,
      output_type = "quantile",
      value = case_when(
        model == "Regular Baseline" ~ 100,
        model == "parGBQR" ~ 90,
        TRUE ~ 95
      )
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
    date = reference_dates,
    target_group = "Overall",
    value = c(91, 92)
  )

  plot_data <- retrospective_ensemble_plot_data(
    forecasts = forecasts,
    actual_data = actual_data,
    forecast_stride = 1L
  )

  expect_equal(plot_data$model, "parGBQR")
  expect_false(plot_data$is_ensemble)
})

test_that("retrospective forecast plot respects selected model", {
  quantiles <- c("0.025", "0.25", "0.5", "0.75", "0.975")
  forecasts <- tidyr::expand_grid(
    model = c("Copycat", "parGBQR", "Ensemble"),
    reference_date = as.Date("2026-01-03"),
    horizon = 0L,
    target_group = "Overall",
    output_type_id = quantiles
  ) |>
    mutate(
      target_end_date = reference_date,
      output_type = "quantile",
      value = case_when(
        model == "Copycat" ~ 80,
        model == "parGBQR" ~ 90,
        TRUE ~ 85
      )
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
    date = as.Date("2026-01-03"),
    target_group = "Overall",
    value = 90
  )

  plot_data <- retrospective_ensemble_plot_data(
    forecasts = forecasts,
    actual_data = actual_data,
    forecast_stride = 1L,
    selected_model = "Copycat"
  )

  expect_equal(plot_data$model, "Copycat")
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

test_that("retrospective score plots remove and describe the selected scoring reference", {
  target_group_scores <- tibble(
    target_group = "Overall",
    model = c("Regular Baseline (default)", "Copycat"),
    mean_wis = c(10, 8),
    mean_relative_wis = c(1, 0.8),
    mean_log_wis = c(1, 0.8),
    mean_relative_log_wis = c(1, 0.8),
    coverage_50 = c(1, 1),
    coverage_95 = c(1, 1),
    n_forecast_targets = c(1, 1)
  )

  target_plot <- plot_retrospective_target_group_scores(
    target_group_scores,
    reference_model = "Regular Baseline (default)"
  )
  expect_false("Regular Baseline (default)" %in% as.character(target_plot$data$model))
  expect_match(target_plot$labels$subtitle, "Regular Baseline \\(default\\)")

  forecast_date_scores <- target_group_scores |>
    dplyr::select(-target_group) |>
    dplyr::mutate(reference_date = as.Date("2026-01-31"), .before = model)
  date_plot <- plot_retrospective_forecast_date_scores(
    forecast_date_scores,
    reference_model = "Regular Baseline (default)"
  )
  expect_false("Regular Baseline (default)" %in% as.character(date_plot$data$model))
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
  expect_true(all(c("reference_date", "model", "rows") %in% names(result$successes)))
  expect_true(all(c("reference_date", "model", "message") %in% names(result$failures)))
  expect_equal(nrow(result$files), 0)
  expect_equal(nrow(result$successes), 0)
  expect_equal(nrow(result$failures), 1)
  expect_true(file.exists(file.path(output_dir, "retrospective_failures.csv")))
  expect_true(file.exists(result$zip_path))
})

# --- Multi-group ("retrospective_group") retrospective runs ----------------

test_that("validate_data scopes duplicate-row checks per retrospective_group", {
  # Same (date, target_group) combination repeated across two different
  # groups is fine -- they're different logical series.
  ok_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(ok_csv), add = TRUE)
  readr::write_csv(
    tibble(
      date = rep(as.Date("2026-01-03") + lubridate::weeks(0:1), times = 2),
      retrospective_group = rep(c("Argentina", "Brazil"), each = 2),
      target_group = "Overall",
      value = c(10, 20, 100, 200)
    ),
    ok_csv
  )
  expect_length(validate_data(ok_csv), 0)

  # An actual duplicate *within* one group should still be caught.
  bad_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(bad_csv), add = TRUE)
  readr::write_csv(
    tibble(
      date = c(as.Date("2026-01-03"), as.Date("2026-01-03"), as.Date("2026-01-10")),
      retrospective_group = c("Argentina", "Argentina", "Argentina"),
      target_group = "Overall",
      value = c(10, 11, 20)
    ),
    bad_csv
  )
  errors <- validate_data(bad_csv)
  expect_true(any(grepl("Duplicate rows", unlist(errors))))
})

test_that("validate_data scopes the missing-week gap check per retrospective_group", {
  csv_path <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_path), add = TRUE)
  readr::write_csv(
    tibble(
      date = c(
        as.Date("2026-01-03") + lubridate::weeks(0:2),  # Argentina: gap at week 3
        as.Date("2026-01-03") + lubridate::weeks(c(0, 1, 3))
      ),
      retrospective_group = c(rep("Argentina", 3), rep("Brazil", 3)),
      target_group = "Overall",
      value = 1:6
    ),
    csv_path
  )
  errors <- validate_data(csv_path)
  gap_error <- unlist(errors)[grepl("Missing weeks", unlist(errors))]
  expect_length(gap_error, 1)
  expect_match(gap_error, "Brazil", fixed = TRUE)
})

test_that("validate_data requires identical week coverage across retrospective_group values", {
  csv_path <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_path), add = TRUE)
  readr::write_csv(
    tibble(
      date = c(
        as.Date("2026-01-03") + lubridate::weeks(0:2),
        as.Date("2026-01-03") + lubridate::weeks(0:1)
      ),
      retrospective_group = c(rep("Argentina", 3), rep("Brazil", 2)),
      target_group = "Overall",
      value = 1:5
    ),
    csv_path
  )
  errors <- validate_data(csv_path)
  coverage_error <- unlist(errors)[grepl("same set of weeks", unlist(errors))]
  expect_length(coverage_error, 1)
  expect_match(coverage_error, "Brazil", fixed = TRUE)
})

test_that("run_retrospective_forecasts isolates each retrospective_group's data completely", {
  df <- tibble(
    date = rep(as.Date("2026-01-03") + lubridate::weeks(0:2), times = 2),
    retrospective_group = rep(c("Argentina", "Brazil"), each = 3),
    target_group = "Overall",
    value = c(10, 20, 30, 100, 200, 300)
  )

  # A runner whose forecast is the mean of everything it was handed: if the
  # engine ever let one group's rows leak into another's fit, this value
  # would drift away from that group's own mean.
  mean_runner <- list(
    label = "Mean Runner",
    run = function(train_data, horizon, quantiles_needed, seasonality) {
      tibble(
        horizon = 1L,
        target_group = "Overall",
        output_type = "quantile",
        output_type_id = "0.5",
        value = mean(train_data$value)
      )
    }
  )

  output_dir <- file.path(tempdir(), paste0("retro-group-isolation-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  result <- run_retrospective_forecasts(
    data = df,
    reference_dates = as.Date("2026-01-17"),
    models = "model_a",
    horizon = 1,
    seasonality = "E",
    quantiles_needed = c(0.5),
    output_dir = output_dir,
    runners = list(model_a = mean_runner)
  )

  expect_equal(result$group_col, "retrospective_group")
  expect_setequal(result$groups, c("Argentina", "Brazil"))
  expect_true("retrospective_group" %in% names(result$forecasts))

  argentina_value <- result$forecasts |>
    filter(retrospective_group == "Argentina") |>
    pull(value)
  brazil_value <- result$forecasts |>
    filter(retrospective_group == "Brazil") |>
    pull(value)

  expect_equal(argentina_value, mean(c(10, 20)))
  expect_equal(brazil_value, mean(c(100, 200)))

  expect_true(dir.exists(file.path(output_dir, "Argentina")))
  expect_true(dir.exists(file.path(output_dir, "Brazil")))
  expect_true(file.exists(file.path(output_dir, "Argentina", "retrospective_2026-01-17.csv")))
  expect_true(file.exists(file.path(output_dir, "Brazil", "retrospective_2026-01-17.csv")))
  expect_true(file.exists(file.path(output_dir, "retrospective_score_overall.csv")))
  expect_true(file.exists(result$zip_path))

  overall_scores <- readr::read_csv(
    file.path(output_dir, "retrospective_score_overall.csv"),
    show_col_types = FALSE
  )
  expect_true("retrospective_group" %in% names(overall_scores))
  expect_setequal(overall_scores$retrospective_group, c("Argentina", "Brazil"))
})

test_that("run_retrospective_forecasts builds independent ensembles per retrospective_group", {
  df <- tibble(
    date = rep(as.Date("2026-01-03") + lubridate::weeks(0:2), times = 2),
    retrospective_group = rep(c("Argentina", "Brazil"), each = 3),
    target_group = "Overall",
    value = c(10, 20, 30, 100, 200, 300)
  )

  offset_runner <- function(offset) {
    list(
      label = paste0("Model offset ", offset),
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        tibble(
          horizon = 1L,
          target_group = "Overall",
          output_type = "quantile",
          output_type_id = "0.5",
          value = mean(train_data$value) + offset
        )
      }
    )
  }
  runners <- list(
    baseline_regular = offset_runner(0),
    model_a = offset_runner(-2),
    model_b = offset_runner(2)
  )

  output_dir <- file.path(tempdir(), paste0("retro-group-ensemble-test-", Sys.getpid()))
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

  ensemble_forecasts <- result$forecasts |> filter(model == "Ensemble")
  argentina_ensemble <- ensemble_forecasts |>
    filter(retrospective_group == "Argentina") |>
    pull(value)
  brazil_ensemble <- ensemble_forecasts |>
    filter(retrospective_group == "Brazil") |>
    pull(value)

  # Each group's ensemble should reflect only that group's own mean, not a
  # blend with the other group's much larger values.
  expect_equal(argentina_ensemble, mean(c(10, 20)))
  expect_equal(brazil_ensemble, mean(c(100, 200)))
})

test_that("run_retrospective_forecasts behaves exactly as before when retrospective_group is absent", {
  df <- tibble(
    date = as.Date("2026-01-03") + lubridate::weeks(0:2),
    target_group = "Overall",
    value = c(10, 20, 30)
  )

  runners <- list(
    model_a = list(
      label = "Model A",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        tibble(
          horizon = 1L,
          target_group = "Overall",
          output_type = "quantile",
          output_type_id = "0.5",
          value = mean(train_data$value)
        )
      }
    )
  )

  output_dir <- file.path(tempdir(), paste0("retro-ungrouped-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  result <- run_retrospective_forecasts(
    data = df,
    reference_dates = as.Date("2026-01-17"),
    models = "model_a",
    horizon = 1,
    seasonality = "E",
    quantiles_needed = c(0.5),
    output_dir = output_dir,
    runners = runners
  )

  expect_null(result$group_col)
  expect_false("retrospective_group" %in% names(result$forecasts))
  expect_true(file.exists(file.path(output_dir, "retrospective_2026-01-17.csv")))
})

test_that("retrospective_replace_group_rows splices one group's rows without touching others", {
  combined <- tibble(
    retrospective_group = c("Argentina", "Argentina", "Brazil"),
    model = c("A", "B", "A"),
    value = c(1, 2, 3)
  )
  new_rows <- tibble(model = "A", value = 99)

  updated <- retrospective_replace_group_rows(combined, "retrospective_group", "Argentina", new_rows)

  expect_equal(nrow(updated), 2)
  expect_equal(updated$value[updated$retrospective_group == "Brazil"], 3)
  expect_equal(updated$value[updated$retrospective_group == "Argentina"], 99)

  # Ungrouped (no group_col present) just returns new_rows untouched.
  ungrouped <- tibble(model = "A", value = 1)
  expect_equal(
    retrospective_replace_group_rows(ungrouped, NULL, NULL, new_rows),
    new_rows
  )
})

# --- Fixes from code review: engine-level coverage check, failure ----------
# --- isolation, folder-name collisions, whitespace trimming ----------------

test_that("run_retrospective_forecasts stops with a clear error on mismatched group week coverage", {
  df <- tibble(
    date = c(
      as.Date("2026-01-03") + lubridate::weeks(0:2),
      as.Date("2026-01-03") + lubridate::weeks(0:1)
    ),
    retrospective_group = c(rep("Argentina", 3), rep("Brazil", 2)),
    target_group = "Overall",
    value = 1:5
  )

  runners <- list(
    model_a = list(
      label = "Model A",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        tibble(
          horizon = 1L, target_group = "Overall",
          output_type = "quantile", output_type_id = "0.5",
          value = mean(train_data$value)
        )
      }
    )
  )

  output_dir <- file.path(tempdir(), paste0("retro-mismatch-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  expect_error(
    run_retrospective_forecasts(
      data = df,
      reference_dates = as.Date("2026-01-17"),
      models = "model_a",
      horizon = 1,
      seasonality = "E",
      quantiles_needed = c(0.5),
      output_dir = output_dir,
      runners = runners
    ),
    regexp = "same set of observed weeks"
  )
  # Nothing should have been written for a run that was rejected up front.
  expect_false(dir.exists(file.path(output_dir, "Argentina")))
})

test_that("run_retrospective_forecasts isolates one group's hard failure from the rest", {
  df <- tibble(
    date = rep(as.Date("2026-01-03") + lubridate::weeks(0:2), times = 2),
    retrospective_group = rep(c("Argentina", "Brazil"), each = 3),
    target_group = "Overall",
    value = c(10, 20, 30, 100, 200, 300)
  )

  mean_runner <- list(
    label = "Mean Runner",
    run = function(train_data, horizon, quantiles_needed, seasonality) {
      tibble(
        horizon = 1L, target_group = "Overall",
        output_type = "quantile", output_type_id = "0.5",
        value = mean(train_data$value)
      )
    }
  )

  # run_retrospective_forecasts_single() is a plain global function (this
  # app isn't a locked package namespace), so it can be swapped out for the
  # duration of this test to simulate a hard, structural failure for one
  # specific group -- something no per-model tryCatch would catch -- and
  # confirm it doesn't take the other group down with it.
  original_single <- run_retrospective_forecasts_single
  on.exit(assign("run_retrospective_forecasts_single", original_single, envir = .GlobalEnv), add = TRUE)
  assign(
    "run_retrospective_forecasts_single",
    function(data, ...) {
      if (any(data$value > 50)) {
        stop("simulated structural failure for this group")
      }
      original_single(data, ...)
    },
    envir = .GlobalEnv
  )

  output_dir <- file.path(tempdir(), paste0("retro-isolation-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  result <- run_retrospective_forecasts(
    data = df,
    reference_dates = as.Date("2026-01-17"),
    models = "model_a",
    horizon = 1,
    seasonality = "E",
    quantiles_needed = c(0.5),
    output_dir = output_dir,
    runners = list(model_a = mean_runner)
  )

  # Brazil (values > 50) hard-failed; Argentina must still have come through.
  expect_setequal(result$groups, c("Argentina", "Brazil"))
  argentina_forecast <- result$forecasts |> filter(retrospective_group == "Argentina")
  expect_equal(argentina_forecast$value, mean(c(10, 20)))
  expect_equal(nrow(result$forecasts |> filter(retrospective_group == "Brazil")), 0)

  brazil_failures <- result$failures |> filter(retrospective_group == "Brazil")
  expect_equal(nrow(brazil_failures), 1)
  expect_match(brazil_failures$message, "simulated structural failure")

  # The failed group must still be selectable/visible, not silently dropped.
  expect_true("Brazil" %in% result$failures$retrospective_group)
})

test_that("make_unique_retrospective_group_folder_names disambiguates collisions", {
  folders <- make_unique_retrospective_group_folder_names(c("Cote d'Ivoire", "Cote d Ivoire", "Chile"))

  expect_equal(length(unique(folders)), 3)
  expect_equal(unname(folders["Chile"]), "Chile")
  expect_true(unname(folders["Cote d'Ivoire"]) != unname(folders["Cote d Ivoire"]))
})

test_that("read_raw_data and validate_data trim whitespace in retrospective_group", {
  csv_path <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_path), add = TRUE)
  readr::write_csv(
    tibble(
      date = rep(as.Date("2026-01-03") + lubridate::weeks(0:1), times = 2),
      retrospective_group = c("Argentina", "Argentina ", "Argentina", "Argentina "),
      target_group = "Overall",
      value = c(10, 20, 10, 20),
      population = 1000
    ) |> dplyr::distinct(date, retrospective_group, .keep_all = TRUE),
    csv_path
  )

  # Two rows per week, one tagged "Argentina" and one "Argentina " (trailing
  # space) -- without trimming this would look like two groups each missing
  # half their weeks; validate_data must not flag it as a coverage mismatch
  # or a duplicate.
  errors <- validate_data(csv_path)
  expect_length(errors, 0)

  data <- read_raw_data(csv_path)
  expect_equal(unique(data$retrospective_group), "Argentina")
})

test_that("read_raw_data parses every date format accepted by validate_data", {
  csv_path <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_path), add = TRUE)
  readr::write_csv(
    tibble(
      date = c("2026-01-31", "07/02/2026", "14-02-2026"),
      target_group = "Overall",
      value = 1:3
    ),
    csv_path
  )

  expect_length(validate_data(csv_path), 0)

  data <- read_raw_data(csv_path)
  expect_equal(
    data$date,
    as.Date(c("2026-01-31", "2026-02-07", "2026-02-14"))
  )
})
# --- Regression tests for the September 2026 pre-release review fixes -----
# (folder-name collisions on rewrite, run_label rename dropping history,
# stale structural group-failure markers, reference-model fallback,
# id-column type coercion on reload, and previously-untested functions:
# add_retrospective_run_configs(), load_retrospective_run(),
# summarize_retrospective_scores_pooled_across_groups(),
# rewrite_retrospective_output_files(), retrospective_score_summary_table(),
# retrospective_group_health_label().)

test_that("rewrite_retrospective_output_files keeps colliding group names in separate folders", {
  df <- tibble(
    date = rep(as.Date("2026-01-03") + lubridate::weeks(0:2), times = 2),
    retrospective_group = rep(c("Cote d'Ivoire", "Cote d Ivoire"), each = 3),
    target_group = "Overall",
    value = c(10, 20, 30, 100, 200, 300)
  )
  fake_runner <- list(
    label = "Copycat",
    run = function(train_data, horizon, quantiles_needed, seasonality, params) {
      tibble(horizon = 1L, target_group = "Overall", output_type = "quantile",
             output_type_id = "0.5", value = mean(train_data$value))
    }
  )
  configs <- tibble(
    run_id = "copycat_1", model_id = "copycat", model_label = "Copycat", run_label = "Copycat",
    params = list(list(recent_weeks_touse = 10L, resp_week_range = 2L, share_groups = TRUE))
  )
  output_dir <- file.path(tempdir(), paste0("retro-rewrite-collision-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  result <- run_retrospective_forecasts(
    data = df, reference_dates = as.Date("2026-01-17"), horizon = 1, seasonality = "E",
    quantiles_needed = c(0.5), output_dir = output_dir, run_configs = configs,
    auto_ensemble = FALSE, runners = list(copycat = fake_runner)
  )

  manifest <- readr::read_csv(file.path(output_dir, "retrospective_group_folders.csv"), show_col_types = FALSE)
  expect_equal(length(unique(manifest$folder)), 2)

  rewritten <- rewrite_retrospective_output_files(result)

  ivoire_apostrophe_dir <- manifest$folder[manifest$group == "Cote d'Ivoire"]
  ivoire_space_dir <- manifest$folder[manifest$group == "Cote d Ivoire"]
  expect_false(identical(ivoire_apostrophe_dir, ivoire_space_dir))

  weekly_apostrophe <- readr::read_csv(
    file.path(output_dir, ivoire_apostrophe_dir, "retrospective_2026-01-17.csv"),
    show_col_types = FALSE
  )
  weekly_space <- readr::read_csv(
    file.path(output_dir, ivoire_space_dir, "retrospective_2026-01-17.csv"),
    show_col_types = FALSE
  )
  expect_equal(weekly_apostrophe$value, mean(c(10, 20)))
  expect_equal(weekly_space$value, mean(c(100, 200)))
})

test_that("add_retrospective_run_configs keeps forecast history when a run's label is renamed", {
  df <- tibble(
    date = as.Date("2026-01-03") + lubridate::weeks(0:2),
    target_group = "Overall",
    value = c(10, 20, 30)
  )
  fake_runner <- list(
    label = "Copycat",
    run = function(train_data, horizon, quantiles_needed, seasonality, params) {
      tibble(horizon = 1L, target_group = "Overall", output_type = "quantile",
             output_type_id = "0.5", value = mean(train_data$value))
    }
  )
  initial_configs <- tibble(
    run_id = "copycat_1", model_id = "copycat", model_label = "Copycat", run_label = "Copycat v1",
    params = list(list(recent_weeks_touse = 10L, resp_week_range = 2L, share_groups = TRUE))
  )
  output_dir <- file.path(tempdir(), paste0("retro-rename-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  existing_result <- run_retrospective_forecasts(
    data = df, reference_dates = as.Date("2026-01-17"), horizon = 1, seasonality = "E",
    quantiles_needed = c(0.5), output_dir = output_dir, run_configs = initial_configs,
    auto_ensemble = FALSE, runners = list(copycat = fake_runner)
  )
  expect_equal(existing_result$forecasts$model, "Copycat v1")

  renamed_configs <- initial_configs
  renamed_configs$run_label <- "Copycat v1 (renamed)"

  merged <- add_retrospective_run_configs(
    existing_result = existing_result, data = df, reference_dates = as.Date("2026-01-17"),
    horizon = 1, seasonality = "E", quantiles_needed = c(0.5), run_configs = renamed_configs,
    runners = list(copycat = fake_runner)
  )

  expect_equal(nrow(merged$forecasts), 1)
  expect_equal(merged$forecasts$model, "Copycat v1 (renamed)")
  expect_equal(merged$run_configs$run_label, "Copycat v1 (renamed)")
})

test_that("add_retrospective_run_configs clears a stale structural group-failure marker once that group succeeds", {
  df <- tibble(
    date = rep(as.Date("2026-01-03") + lubridate::weeks(0:2), times = 2),
    retrospective_group = rep(c("Argentina", "Brazil"), each = 3),
    target_group = "Overall",
    value = c(10, 20, 30, 100, 200, 300)
  )
  fake_runner <- list(
    label = "Copycat",
    run = function(train_data, horizon, quantiles_needed, seasonality, params) {
      tibble(horizon = 1L, target_group = "Overall", output_type = "quantile",
             output_type_id = "0.5", value = mean(train_data$value))
    }
  )
  initial_configs <- tibble(
    run_id = "copycat_1", model_id = "copycat", model_label = "Copycat", run_label = "Copycat 1",
    params = list(list(recent_weeks_touse = 10L, resp_week_range = 2L, share_groups = TRUE))
  )
  output_dir <- file.path(tempdir(), paste0("retro-stale-marker-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  original_single <- run_retrospective_forecasts_single
  on.exit(assign("run_retrospective_forecasts_single", original_single, envir = .GlobalEnv), add = TRUE)
  assign(
    "run_retrospective_forecasts_single",
    function(data, ...) {
      if (any(data$value > 50)) stop("simulated structural failure for this group")
      original_single(data, ...)
    },
    envir = .GlobalEnv
  )

  initial_result <- run_retrospective_forecasts(
    data = df, reference_dates = as.Date("2026-01-17"), horizon = 1, seasonality = "E",
    quantiles_needed = c(0.5), output_dir = output_dir, run_configs = initial_configs,
    auto_ensemble = FALSE, runners = list(copycat = fake_runner)
  )
  assign("run_retrospective_forecasts_single", original_single, envir = .GlobalEnv)

  brazil_marker <- initial_result$failures |>
    dplyr::filter(retrospective_group == "Brazil", is.na(run_id))
  expect_equal(nrow(brazil_marker), 1)

  added_configs <- dplyr::bind_rows(
    initial_configs,
    tibble(
      run_id = "copycat_2", model_id = "copycat", model_label = "Copycat", run_label = "Copycat 2",
      params = list(list(recent_weeks_touse = 5L, resp_week_range = 2L, share_groups = TRUE))
    )
  )

  merged <- add_retrospective_run_configs(
    existing_result = initial_result, data = df, reference_dates = as.Date("2026-01-17"),
    horizon = 1, seasonality = "E", quantiles_needed = c(0.5), run_configs = added_configs,
    runners = list(copycat = fake_runner)
  )

  expect_true(nrow(merged$forecasts |> dplyr::filter(retrospective_group == "Brazil")) > 0)
  stale_marker_still_present <- merged$failures |>
    dplyr::filter(retrospective_group == "Brazil", is.na(run_id))
  expect_equal(nrow(stale_marker_still_present), 0)
})

test_that("add_retrospective_run_configs adds a new model and removes a dropped model across two incremental calls", {
  df <- tibble(
    date = as.Date("2026-01-03") + lubridate::weeks(0:2),
    target_group = "Overall",
    value = c(10, 20, 30)
  )
  fake_runner <- list(
    label = "Copycat",
    run = function(train_data, horizon, quantiles_needed, seasonality, params) {
      tibble(horizon = 1L, target_group = "Overall", output_type = "quantile",
             output_type_id = "0.5", value = mean(train_data$value))
    }
  )

  configs_1 <- tibble(
    run_id = "copycat_1", model_id = "copycat", model_label = "Copycat", run_label = "Copycat 1",
    params = list(list(recent_weeks_touse = 10L, resp_week_range = 2L, share_groups = TRUE))
  )
  output_dir <- file.path(tempdir(), paste0("retro-add-remove-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  result_1 <- run_retrospective_forecasts(
    data = df, reference_dates = as.Date("2026-01-17"), horizon = 1, seasonality = "E",
    quantiles_needed = c(0.5), output_dir = output_dir, run_configs = configs_1,
    auto_ensemble = FALSE, runners = list(copycat = fake_runner)
  )
  expect_equal(result_1$forecasts$model, "Copycat 1")

  configs_2 <- dplyr::bind_rows(
    configs_1,
    tibble(run_id = "copycat_2", model_id = "copycat", model_label = "Copycat", run_label = "Copycat 2",
           params = list(list(recent_weeks_touse = 5L, resp_week_range = 2L, share_groups = TRUE)))
  )
  result_2 <- add_retrospective_run_configs(
    existing_result = result_1, data = df, reference_dates = as.Date("2026-01-17"),
    horizon = 1, seasonality = "E", quantiles_needed = c(0.5), run_configs = configs_2,
    runners = list(copycat = fake_runner)
  )
  expect_setequal(result_2$forecasts$model, c("Copycat 1", "Copycat 2"))
  expect_equal(
    result_2$forecasts$value[result_2$forecasts$model == "Copycat 1"],
    result_1$forecasts$value
  )

  configs_3 <- configs_2 |> dplyr::filter(run_id == "copycat_2")
  result_3 <- add_retrospective_run_configs(
    existing_result = result_2, data = df, reference_dates = as.Date("2026-01-17"),
    horizon = 1, seasonality = "E", quantiles_needed = c(0.5), run_configs = configs_3,
    runners = list(copycat = fake_runner)
  )
  expect_equal(result_3$forecasts$model, "Copycat 2")
  expect_false("Copycat 1" %in% result_3$successes$model)
})

test_that("retrospective_resolve_reference_model falls back to an available model instead of a non-existent literal", {
  forecasts <- tibble(
    model = c("ModelX", "ModelY"),
    reference_date = as.Date("2026-01-10"), horizon = 0L, target_end_date = as.Date("2026-01-10"),
    target_group = "Overall", output_type = "quantile", output_type_id = "0.5", value = c(10, 11)
  )
  resolved <- retrospective_resolve_reference_model(forecasts, run_configs = tibble())
  expect_true(resolved %in% c("ModelX", "ModelY"))

  baseline_forecasts <- dplyr::bind_rows(
    forecasts,
    tibble(model = "STArima Baseline", reference_date = as.Date("2026-01-10"), horizon = 0L,
           target_end_date = as.Date("2026-01-10"), target_group = "Overall",
           output_type = "quantile", output_type_id = "0.5", value = 12)
  )
  resolved_baseline_pref <- retrospective_resolve_reference_model(baseline_forecasts, run_configs = tibble())
  expect_equal(resolved_baseline_pref, "STArima Baseline")

  expect_true(is.na(retrospective_resolve_reference_model(forecasts[0, ], run_configs = tibble())))
})

test_that("load_retrospective_run preserves numeric-looking and boolean-looking group ids as character", {
  df <- tibble(
    date = rep(as.Date("2026-01-03") + lubridate::weeks(0:2), times = 2),
    retrospective_group = rep(c("111111111111111111", "TRUE"), each = 3),
    target_group = "Overall",
    value = c(10, 20, 30, 100, 200, 300)
  )
  fake_runner <- list(
    label = "Copycat",
    run = function(train_data, horizon, quantiles_needed, seasonality, params) {
      tibble(horizon = 1L, target_group = "Overall", output_type = "quantile",
             output_type_id = "0.5", value = mean(train_data$value))
    }
  )
  configs <- tibble(
    run_id = "copycat_1", model_id = "copycat", model_label = "Copycat", run_label = "Copycat",
    params = list(list(recent_weeks_touse = 10L, resp_week_range = 2L, share_groups = TRUE))
  )
  output_dir <- file.path(tempdir(), paste0("retro-idcols-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  run_retrospective_forecasts(
    data = df, reference_dates = as.Date("2026-01-17"), horizon = 1, seasonality = "E",
    quantiles_needed = c(0.5), output_dir = output_dir, run_configs = configs,
    auto_ensemble = FALSE, runners = list(copycat = fake_runner)
  )

  loaded <- load_retrospective_run(output_dir)

  expect_type(loaded$result$forecasts$retrospective_group, "character")
  expect_setequal(loaded$result$forecasts$retrospective_group, c("111111111111111111", "TRUE"))
  expect_type(loaded$result$scores$rows$retrospective_group, "character")
})

test_that("summarize_retrospective_scores_pooled_across_groups pools row-weighted means across groups", {
  score_rows <- tibble(
    retrospective_group = c("A", "A", "B"),
    model = c("Regular Baseline", "ModelX", "ModelX"),
    wis = c(10, 5, 15),
    log_wis = c(1, 0.5, 1.5),
    covered_50 = c(TRUE, TRUE, FALSE),
    covered_95 = c(TRUE, TRUE, TRUE)
  )

  summary_tbl <- summarize_retrospective_scores_pooled_across_groups(score_rows, reference_model = "Regular Baseline")

  expect_setequal(summary_tbl$model, c("Regular Baseline", "ModelX"))
  modelx <- summary_tbl |> dplyr::filter(model == "ModelX")
  expect_equal(modelx$mean_wis, mean(c(5, 15)))
  expect_equal(modelx$mean_relative_wis, mean(c(5, 15)) / 10)
  expect_equal(modelx$n_forecast_targets, 2)

  baseline_row <- summary_tbl |> dplyr::filter(model == "Regular Baseline")
  expect_equal(baseline_row$mean_relative_wis, 1)

  expect_equal(nrow(summarize_retrospective_scores_pooled_across_groups(tibble(), "Regular Baseline")), 0)

  no_ref <- summarize_retrospective_scores_pooled_across_groups(
    score_rows |> dplyr::filter(model != "Regular Baseline"),
    reference_model = "Regular Baseline"
  )
  expect_true(all(is.na(no_ref$mean_relative_wis)))
})

# --- server/retrospective.R pure helper functions (extracted by run_tests.R
# via extract_pure_server_functions() -- see that file for why a full
# source() of the server file isn't possible outside a live Shiny session) --

test_that("retrospective_group_country_input_id/zone_badge_output_id no longer collide on punctuation", {
  groups <- c("A B", "A-B", "A_B", "A.B")
  input_ids <- vapply(groups, retrospective_group_country_input_id, character(1), groups = groups)
  badge_ids <- vapply(groups, retrospective_group_zone_badge_output_id, character(1), groups = groups)

  expect_equal(length(unique(input_ids)), length(groups))
  expect_equal(length(unique(badge_ids)), length(groups))

  # A group value not present in `groups` at all still returns something
  # usable rather than erroring.
  expect_type(retrospective_group_country_input_id("Unknown", groups), "character")
})

test_that("retrospective_ensemble_members_co_occur_in_any_group requires 2+ members in the SAME group", {
  never_co_occur <- list(
    group_col = "retrospective_group",
    successes = tibble(retrospective_group = c("A", "B"), model = c("ModelX", "ModelY"))
  )
  expect_false(retrospective_ensemble_members_co_occur_in_any_group(c("ModelX", "ModelY"), never_co_occur))

  co_occurs_in_one <- list(
    group_col = "retrospective_group",
    successes = tibble(retrospective_group = c("A", "A", "B"), model = c("ModelX", "ModelY", "ModelX"))
  )
  expect_true(retrospective_ensemble_members_co_occur_in_any_group(c("ModelX", "ModelY"), co_occurs_in_one))

  expect_false(retrospective_ensemble_members_co_occur_in_any_group(character(), never_co_occur))
  expect_false(retrospective_ensemble_members_co_occur_in_any_group(c("ModelX"), never_co_occur))
  expect_false(retrospective_ensemble_members_co_occur_in_any_group(c("ModelX", "ModelY"), NULL))

  # Ungrouped result: falls back to a flat member count.
  ungrouped <- list(group_col = NULL, successes = tibble(model = c("ModelX", "ModelY")))
  expect_true(retrospective_ensemble_members_co_occur_in_any_group(c("ModelX", "ModelY"), ungrouped))
})

test_that("retrospective_group_health_label summarizes success/failure/total-failure states", {
  # Match on fixed, non-emoji substrings rather than the leading emoji
  # itself -- grepl()'s regex engine can choke on wide-string translation
  # for some locales even though the underlying UTF-8 bytes are correct.
  result <- list(
    successes = tibble(retrospective_group = c("A", "A"), model = c("M1", "M2")),
    failures = tibble(retrospective_group = c("A", "B"), model = c("M3", "M4"))
  )
  label_a <- retrospective_group_health_label(result, "retrospective_group", "A")
  label_b <- retrospective_group_health_label(result, "retrospective_group", "B")
  expect_true(grepl("A (1 failure)", label_a, fixed = TRUE))
  expect_true(grepl("B (run failed)", label_b, fixed = TRUE))

  healthy_result <- list(
    successes = tibble(retrospective_group = "C", model = "M1"),
    failures = tibble(retrospective_group = character(), model = character())
  )
  label_c <- retrospective_group_health_label(healthy_result, "retrospective_group", "C")
  expect_true(grepl("C", label_c, fixed = TRUE))
  expect_false(grepl("failure", label_c, fixed = TRUE))
})

test_that("retrospective_score_summary_table highlights the best row and handles all-NA relative WIS", {
  score_tbl <- tibble(
    model = c("A", "B"),
    mean_wis = c(2, 1),
    mean_relative_wis = c(NA_real_, NA_real_),
    mean_log_wis = c(0.2, 0.1),
    mean_relative_log_wis = c(NA_real_, NA_real_),
    coverage_50 = c(0.5, 0.6),
    coverage_95 = c(0.9, 0.95),
    n_forecast_targets = c(10, 10)
  )
  dt <- retrospective_score_summary_table(score_tbl)
  expect_s3_class(dt, "datatables")

  score_tbl_with_ref <- score_tbl
  score_tbl_with_ref$mean_relative_wis <- c(2, 1)
  dt2 <- retrospective_score_summary_table(score_tbl_with_ref)
  expect_s3_class(dt2, "datatables")
})

test_that("retrospective_scoring_reference_exceptions flags only groups that diverge from the global choice", {
  # retrospective_result_group_col() is a reactive normally defined
  # elsewhere in server/retrospective.R (not extracted -- it isn't a pure
  # function). Stub it in the SAME environment the extracted function's
  # closure resolves names in (globalenv()), since a local `<-` inside this
  # test_that block would not be visible to it.
  assign("retrospective_result_group_col", function() "retrospective_group", envir = .GlobalEnv)
  on.exit(rm("retrospective_result_group_col", envir = .GlobalEnv), add = TRUE)

  result <- list(scoring_reference = c(A = "ModelX", B = "Regular Baseline"))
  exceptions <- retrospective_scoring_reference_exceptions(result, "ModelX")
  expect_equal(names(exceptions), "B")
  expect_equal(unname(exceptions), "Regular Baseline")

  same_everywhere <- list(scoring_reference = c(A = "ModelX", B = "ModelX"))
  expect_length(retrospective_scoring_reference_exceptions(same_everywhere, "ModelX"), 0)

  assign("retrospective_result_group_col", function() NULL, envir = .GlobalEnv)
  expect_length(retrospective_scoring_reference_exceptions(result, "ModelX"), 0)
})

# --- Coverage-gap tests from a direct audit of "does every retrospective ----
# component have a test" -- added for functions that had zero coverage
# (direct or indirect) plus a set of indirectly-covered-but-important
# functions that are cheap to test directly and worth pinning down on their
# own: retrospective_format_param_value(), retrospective_params_label(),
# retrospective_make_run_label(), retrospective_make_run_id(),
# retrospective_build_run_configs(), retrospective_wrap_labels(),
# plot_retrospective_ensemble_forecasts(), retrospective_parameter_specs(),
# is_retrospective_baseline_model(), sanitize_retrospective_group_name(),
# retrospective_score_metric(), retrospective_run_config_metadata() /
# retrospective_run_configs_from_metadata() (round trip),
# retrospective_group_failure_result(), retrospective_validate_run_configs(),
# write_retrospective_group_scoring_reference(), call_retrospective_runner().

test_that("retrospective_format_param_value formats NA, logical, vector, and scalar params", {
  expect_equal(retrospective_format_param_value(NA), "default")
  expect_equal(retrospective_format_param_value(TRUE), "true")
  expect_equal(retrospective_format_param_value(FALSE), "false")
  expect_equal(retrospective_format_param_value(c("a", "b", "c")), "a+b+c")
  expect_equal(retrospective_format_param_value(5L), "5")
  expect_equal(retrospective_format_param_value("global"), "global")
})

test_that("retrospective_params_label joins formatted name=value pairs, or falls back to default", {
  expect_equal(retrospective_params_label(NULL), "default")
  expect_equal(retrospective_params_label(list()), "default")
  expect_equal(
    retrospective_params_label(list(recent_weeks_touse = 100L, share_groups = TRUE)),
    "recent_weeks_touse=100; share_groups=true"
  )
})

test_that("retrospective_make_run_label combines model and param labels, honoring compact_default", {
  expect_equal(
    retrospective_make_run_label("copycat", list(recent_weeks_touse = 12L)),
    paste0(retrospective_model_label("copycat"), " (recent_weeks_touse=12)")
  )
  # With no params, compact_default = FALSE still appends "(default)" ...
  expect_equal(
    retrospective_make_run_label("copycat", list()),
    paste0(retrospective_model_label("copycat"), " (default)")
  )
  # ... but compact_default = TRUE collapses that down to just the model label.
  expect_equal(
    retrospective_make_run_label("copycat", list(), compact_default = TRUE),
    retrospective_model_label("copycat")
  )
})

test_that("retrospective_make_run_id sanitizes param punctuation and appends an optional index", {
  expect_equal(retrospective_make_run_id("copycat", list()), "copycat__default")
  expect_equal(
    retrospective_make_run_id("newgbqr", list(model_type = "global", num_bags = 50L)),
    "newgbqr__model_type_global__num_bags_50"
  )
  # Non-alphanumeric characters in a formatted value get collapsed to "-".
  expect_equal(
    retrospective_make_run_id("copycat", list(label = "a/b c")),
    "copycat__label_a-b-c"
  )
  expect_equal(
    retrospective_make_run_id("copycat", list(), index = 2),
    "copycat__default__2"
  )
})

test_that("retrospective_build_run_configs builds one labeled row per model, or falls back to base-model labels", {
  empty <- retrospective_build_run_configs(character())
  expect_equal(nrow(empty), 0)
  expect_equal(names(empty), c("run_id", "model_id", "model_label", "run_label", "params"))

  configs <- retrospective_build_run_configs(
    c("copycat", "baseline_regular"),
    settings = retrospective_default_settings()
  )
  expect_equal(nrow(configs), 2)
  expect_equal(configs$model_id, c("copycat", "baseline_regular"))
  expect_true(all(nzchar(configs$run_id)))
  expect_true(anyDuplicated(configs$run_id) == 0)
  expect_true(anyDuplicated(configs$run_label) == 0)
  expect_equal(configs$model_label[configs$model_id == "baseline_regular"], retrospective_model_label("baseline_regular"))

  base_labeled <- retrospective_build_run_configs(
    c("copycat", "baseline_regular"),
    settings = retrospective_default_settings(),
    labels_use_base_model = TRUE
  )
  expect_equal(base_labeled$run_id, c("copycat", "baseline_regular"))
  expect_equal(base_labeled$run_label, base_labeled$model_label)
})

test_that("retrospective_wrap_labels wraps long labels onto multiple lines and leaves short ones alone", {
  # vapply()'s default USE.NAMES = TRUE labels an unnamed character input
  # with itself, so compare the unnamed value.
  expect_equal(unname(retrospective_wrap_labels("Short")), "Short")
  wrapped <- unname(retrospective_wrap_labels("A Fairly Long Model Label Here", width = 10L))
  expect_true(grepl("\n", wrapped))
  expect_true(all(nchar(strsplit(wrapped, "\n")[[1]]) <= 10))
})

test_that("plot_retrospective_ensemble_forecasts builds a faceted ggplot from forecast quantiles", {
  quantiles <- c("0.025", "0.25", "0.5", "0.75", "0.975")
  reference_dates <- as.Date("2026-01-03") + lubridate::weeks(0:3)

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
      value = 100 + as.integer(reference_date - min(reference_date)) + horizon + match(output_type_id, quantiles)
    ) |>
    select(model, reference_date, horizon, target_end_date, target_group, output_type, output_type_id, value)

  actual_data <- tibble(
    date = as.Date("2026-01-03") + lubridate::weeks(0:5),
    target_group = "Overall",
    value = 100 + seq_along(date)
  )

  p <- plot_retrospective_ensemble_forecasts(forecasts, actual_data, forecast_stride = 2L)

  expect_s3_class(p, "ggplot")
  expect_true(grepl("Ensemble", p$labels$caption))
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("retrospective_parameter_specs toggles the INLA population-offset default and defines model-specific params", {
  specs_no_pop <- retrospective_parameter_specs(has_population = FALSE)
  specs_with_pop <- retrospective_parameter_specs(has_population = TRUE)

  expect_false(specs_no_pop$inla$use_offset$default)
  expect_true(specs_with_pop$inla$use_offset$default)

  expect_equal(specs_no_pop$baseline_regular, list())
  expect_true("recent_weeks_touse" %in% names(specs_no_pop$copycat))
  expect_equal(specs_no_pop$copycat$recent_weeks_touse$min, 3L)
  expect_equal(specs_no_pop$copycat$recent_weeks_touse$max, 100L)
})

test_that("is_retrospective_baseline_model matches any model label containing 'Baseline', case-insensitively", {
  expect_equal(
    is_retrospective_baseline_model(c("Regular Baseline", "parGBQR", "seasonal baseline", "Ensemble")),
    c(TRUE, FALSE, TRUE, FALSE)
  )
})

test_that("sanitize_retrospective_group_name strips punctuation and never returns an empty string", {
  expect_equal(sanitize_retrospective_group_name("Cote d'Ivoire"), "Cote d_Ivoire")
  expect_equal(sanitize_retrospective_group_name("  Peru  "), "Peru")
  expect_equal(sanitize_retrospective_group_name("###"), "_")
  expect_equal(sanitize_retrospective_group_name(""), "group")
  # A literal NA group value round-trips to NA rather than "group" or the
  # string "NA" -- gsub()/trimws() propagate NA through untouched, and
  # nzchar(NA_character_) is TRUE by default, so the `!nzchar(cleaned)`
  # fallback never triggers. Documented here as real (if surprising)
  # behavior; a NA-valued retrospective_group column would need to be
  # caught upstream, not by this function.
  expect_true(is.na(sanitize_retrospective_group_name(NA)))
})

test_that("retrospective_score_metric prefers relative WIS when available and falls back to raw WIS", {
  with_relative <- tibble(mean_wis = c(1, 2), mean_relative_wis = c(NA_real_, 1.1))
  metric <- retrospective_score_metric(with_relative)
  expect_equal(metric$column, "mean_relative_wis")
  expect_true(metric$uses_relative)

  all_na_relative <- tibble(mean_wis = c(1, 2), mean_relative_wis = c(NA_real_, NA_real_))
  metric2 <- retrospective_score_metric(all_na_relative)
  expect_equal(metric2$column, "mean_wis")
  expect_false(metric2$uses_relative)
})

test_that("retrospective_run_config_metadata and retrospective_run_configs_from_metadata round-trip run_configs", {
  run_configs <- retrospective_build_run_configs(
    c("copycat", "baseline_regular"),
    settings = list(copycat = list(recent_weeks_touse = 100L, resp_week_range = 2L, share_groups = TRUE))
  )

  metadata <- retrospective_run_config_metadata(run_configs)
  expect_equal(sort(unique(metadata$run_id)), sort(run_configs$run_id))
  # The no-params model (baseline_regular) gets one NA parameter/value row.
  baseline_rows <- metadata[metadata$model_id == "baseline_regular", ]
  expect_equal(nrow(baseline_rows), 1)
  expect_true(is.na(baseline_rows$parameter))

  rebuilt <- retrospective_run_configs_from_metadata(metadata)
  expect_equal(sort(rebuilt$run_id), sort(run_configs$run_id))

  copycat_params <- rebuilt$params[[which(rebuilt$model_id == "copycat")]]
  expect_equal(copycat_params$recent_weeks_touse, "100")
  expect_equal(copycat_params$share_groups, "true")

  baseline_params <- rebuilt$params[[which(rebuilt$model_id == "baseline_regular")]]
  expect_equal(baseline_params, list())

  expect_equal(nrow(retrospective_run_configs_from_metadata(NULL)), 0)
})

test_that("retrospective_group_failure_result shapes a failure marker with empty forecasts/scores", {
  result <- retrospective_group_failure_result(
    group_value = "Chile",
    error_message = "boom",
    run_configs = tibble(run_id = "copycat__default"),
    ensemble_models = NULL,
    ensemble_method = "median"
  )

  expect_true(result$group_failed)
  expect_equal(nrow(result$forecasts), 0)
  expect_equal(nrow(result$scores$rows), 0)
  expect_equal(nrow(result$successes), 0)
  expect_equal(nrow(result$failures), 1)
  expect_true(grepl("Chile", result$failures$model, fixed = TRUE))
  expect_equal(result$failures$message, "boom")
  expect_equal(result$ensemble_models, character())
  expect_true(is.na(result$scoring_reference))
})

test_that("retrospective_validate_run_configs rejects missing columns, empty tables, and duplicate ids/labels", {
  base <- tibble(
    run_id = c("r1", "r2"),
    model_id = c("copycat", "copycat"),
    model_label = c("Copycat", "Copycat"),
    run_label = c("Copycat (a)", "Copycat (b)"),
    params = list(list(), list())
  )

  expect_error(
    retrospective_validate_run_configs(dplyr::select(base, -params)),
    "missing"
  )
  expect_error(
    retrospective_validate_run_configs(base[0, ]),
    "Select at least one"
  )

  dup_ids <- base
  dup_ids$run_id <- c("r1", "r1")
  expect_error(retrospective_validate_run_configs(dup_ids), "unique")

  dup_labels <- base
  dup_labels$run_label <- c("same", "same")
  expect_error(retrospective_validate_run_configs(dup_labels), "unique")

  expect_equal(nrow(retrospective_validate_run_configs(base)), 2)
})

test_that("write_retrospective_group_scoring_reference writes a CSV for named references and no-ops for unnamed ones", {
  output_dir <- file.path(tempdir(), paste0("retro-scoring-ref-test-", Sys.getpid()))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  named <- c(Chile = "ModelX", Peru = "Regular Baseline")
  written <- write_retrospective_group_scoring_reference(output_dir, named, "retrospective_group")
  csv_path <- file.path(output_dir, "retrospective_group_scoring_reference.csv")
  expect_true(file.exists(csv_path))
  on_disk <- readr::read_csv(csv_path, show_col_types = FALSE)
  expect_equal(on_disk$group, c("Chile", "Peru"))
  expect_equal(on_disk$scoring_reference, c("ModelX", "Regular Baseline"))
  expect_equal(written$group, c("Chile", "Peru"))

  # An unnamed scoring_reference (e.g. the ungrouped single-group case) is a
  # silent no-op: no file is written at all, by design.
  unlink(csv_path)
  result <- write_retrospective_group_scoring_reference(output_dir, "ModelX", "retrospective_group")
  expect_null(result)
  expect_false(file.exists(csv_path))
})

test_that("call_retrospective_runner forwards params only when the runner's run() accepts them", {
  captured <- new.env()

  runner_with_params <- list(run = function(train_data, horizon, quantiles_needed, seasonality, params) {
    captured$params <- params
    "ran-with-params"
  })
  out1 <- call_retrospective_runner(
    runner_with_params,
    train_data = tibble(x = 1),
    horizon = 4L,
    quantiles_needed = c(0.5),
    seasonality = FALSE,
    params = list(recent_weeks_touse = 10L)
  )
  expect_equal(out1, "ran-with-params")
  expect_equal(captured$params, list(recent_weeks_touse = 10L))

  runner_without_params <- list(run = function(train_data, horizon, quantiles_needed, seasonality) {
    "ran-without-params"
  })
  out2 <- call_retrospective_runner(
    runner_without_params,
    train_data = tibble(x = 1),
    horizon = 4L,
    quantiles_needed = c(0.5),
    seasonality = FALSE,
    params = list(recent_weeks_touse = 10L)
  )
  expect_equal(out2, "ran-without-params")

  runner_with_dots <- list(run = function(train_data, horizon, quantiles_needed, seasonality, ...) {
    dots <- list(...)
    captured$dots_params <- dots$params
    "ran-with-dots"
  })
  out3 <- call_retrospective_runner(
    runner_with_dots,
    train_data = tibble(x = 1),
    horizon = 4L,
    quantiles_needed = c(0.5),
    seasonality = FALSE,
    params = list(share_groups = TRUE)
  )
  expect_equal(out3, "ran-with-dots")
  expect_equal(captured$dots_params, list(share_groups = TRUE))
})

test_that("retrospective_load_validation_data_type uses the loaded run's own data_type, not the caller's", {
  # Ungrouped run saved as "proportion": must validate as "proportion" even
  # if the app's Data Type radio happens to currently be on something else --
  # this is the exact bug that made "Load Previous Run" reject a perfectly
  # valid saved file and silently never set retrospective$raw_data.
  expect_equal(
    retrospective_load_validation_data_type(list(group_col = NULL, data_type = "proportion")),
    "proportion"
  )
  expect_equal(
    retrospective_load_validation_data_type(list(group_col = NULL, data_type = "count")),
    "count"
  )
  # A run saved before data_type existed at all has no field for it --
  # falls back to "count".
  expect_equal(
    retrospective_load_validation_data_type(list(group_col = NULL, data_type = NULL)),
    "count"
  )
  # A grouped run always validates as "count", regardless of its saved
  # data_type -- mirrors the has_group_col-forces-"count" convention a fresh
  # grouped upload already uses (a single scalar validate_data() call can't
  # meaningfully apply different per-group data types anyway).
  expect_equal(
    retrospective_load_validation_data_type(list(group_col = "retrospective_group", data_type = "proportion")),
    "count"
  )
})

test_that("retrospective_parameter_input_widget doesn't crash on a spec that omits an optional numeric bound", {
  # This is exactly the shape of copycat's max_matches spec: no `max` field
  # at all, so spec$max resolves to NULL rather than NA. Passing that NULL
  # straight through to numericInput(max = ...) used to crash with "Error in
  # if: argument is of length zero" -- shiny::numericInput()'s own body does
  # `if (!is.na(max))`, and is.na(NULL) is logical(0). This was the real
  # cause of "no options shown" when selecting Copycat in Advanced Setup:
  # ALL of a model's parameter widgets are built in one lapply(), so one
  # crashing widget takes the whole renderUI down, not just itself.
  spec_missing_max <- list(
    label = "Max historical matches",
    type = "optional_numeric",
    default = NA_real_,
    min = 1L,
    step = 1L
  )
  widget <- retrospective_parameter_input_widget("test_max_matches", spec_missing_max)
  expect_s3_class(widget, "shiny.tag")

  # Same for the plain "numeric" branch, defensively -- a future spec that
  # omits min/max/step shouldn't crash either.
  spec_numeric_missing_bounds <- list(label = "Some setting", type = "numeric", default = 5L)
  expect_s3_class(
    retrospective_parameter_input_widget("test_numeric", spec_numeric_missing_bounds),
    "shiny.tag"
  )
})

test_that("retrospective_parameter_input_widget builds a valid widget for every real parameter spec", {
  # Blanket regression test: every parameter of every model, for both
  # has_population values, must build a widget without erroring. This is the
  # test that would have caught the max_matches crash immediately, and will
  # catch the same class of bug for any future model/param addition.
  for (has_population in c(FALSE, TRUE)) {
    specs_by_model <- retrospective_parameter_specs(has_population = has_population)
    for (model_id in names(specs_by_model)) {
      specs <- specs_by_model[[model_id]]
      for (param_name in names(specs)) {
        widget <- tryCatch(
          retrospective_parameter_input_widget(
            paste0("test_", model_id, "_", param_name),
            specs[[param_name]]
          ),
          error = function(e) {
            fail(paste0("model: ", model_id, " param: ", param_name, " -- ", conditionMessage(e)))
            NULL
          }
        )
        if (!is.null(widget)) expect_s3_class(widget, "shiny.tag")
      }
    }
  }
})

test_that("retrospective_parameter_input_widget builds the right kind of input per type", {
  numeric_widget <- retrospective_parameter_input_widget(
    "id_numeric",
    list(label = "Weeks", type = "numeric", default = 100L, min = 3L, max = 100L, step = 1L)
  )
  expect_true(grepl('type="number"', as.character(numeric_widget), fixed = TRUE))
  expect_true(grepl('value="100"', as.character(numeric_widget), fixed = TRUE))

  choice_widget <- retrospective_parameter_input_widget(
    "id_choice",
    list(label = "Model type", type = "choice", default = "global", choices = c("global", "individual"))
  )
  expect_true(grepl("selectize", as.character(choice_widget), fixed = TRUE))

  logical_widget <- retrospective_parameter_input_widget(
    "id_logical",
    list(label = "Share groups", type = "logical", default = TRUE)
  )
  expect_true(grepl('type="checkbox"', as.character(logical_widget), fixed = TRUE))
  expect_true(grepl("checked", as.character(logical_widget), fixed = TRUE))

  text_widget <- retrospective_parameter_input_widget(
    "id_text",
    list(label = "Label", type = "text", default = "hello")
  )
  expect_true(grepl('value="hello"', as.character(text_widget), fixed = TRUE))

  integer_vector_widget <- retrospective_parameter_input_widget(
    "id_vec",
    list(label = "Weeks", type = "integer_vector", default = c(1L, 2L, 3L))
  )
  expect_true(grepl('value="1,2,3"', as.character(integer_vector_widget), fixed = TRUE))
})

test_that("retrospective_sanitize_run_name cleans free text into a short, filesystem-safe slug", {
  expect_equal(retrospective_sanitize_run_name(NULL), "")
  expect_equal(retrospective_sanitize_run_name(NA), "")
  expect_equal(retrospective_sanitize_run_name(""), "")
  expect_equal(retrospective_sanitize_run_name("   "), "")
  expect_equal(retrospective_sanitize_run_name("Copycat noise test"), "Copycat-noise-test")
  # Punctuation that isn't already allowed gets dropped, not turned into "_"
  # (a run name reads better collapsed than underscore-riddled).
  expect_equal(retrospective_sanitize_run_name("50% noise, take #2!"), "50-noise-take-2")
  expect_equal(retrospective_sanitize_run_name("  leading/trailing spaces  "), "leadingtrailing-spaces")
  # Capped at 40 characters.
  long_name <- strrep("a", 60)
  expect_equal(nchar(retrospective_sanitize_run_name(long_name)), 40)
})

test_that("retrospective_run_folder_stamp bakes a sanitized run name into the timestamp, or falls back to it alone", {
  now <- as.POSIXct("2026-09-16 14:30:22", tz = "UTC")

  named <- retrospective_run_folder_stamp("Copycat noise test", now = now, random_suffix = "1234")
  expect_equal(named, "Copycat-noise-test__20260916143022000-1234")

  unnamed <- retrospective_run_folder_stamp(NULL, now = now, random_suffix = "1234")
  expect_equal(unnamed, "20260916143022000-1234")

  blank <- retrospective_run_folder_stamp("   ", now = now, random_suffix = "1234")
  expect_equal(blank, unnamed)

  # No name/no explicit suffix still produces *some* valid, non-empty stamp
  # (the real sample.int()-based default path).
  expect_true(nzchar(retrospective_run_folder_stamp()))
})

test_that("a run's Session Name round-trips through write/load, and is blank for a run saved before the feature existed", {
  df <- tibble(
    date = as.Date("2026-01-03") + lubridate::weeks(0:3),
    target_group = "Overall",
    value = c(10, 20, 30, 40)
  )
  fake_runner <- list(
    label = "Copycat",
    run = function(train_data, horizon, quantiles_needed, seasonality, params) {
      tibble(horizon = 1L, target_group = "Overall", output_type = "quantile",
             output_type_id = "0.5", value = mean(train_data$value))
    }
  )
  configs <- tibble(
    run_id = "copycat_1", model_id = "copycat", model_label = "Copycat", run_label = "Copycat",
    params = list(list(recent_weeks_touse = 10L, resp_week_range = 2L, share_groups = TRUE))
  )
  output_dir <- file.path(tempdir(), paste0("retro-runname-test-", Sys.getpid()))
  unlink(output_dir, recursive = TRUE)
  on.exit(unlink(c(output_dir, paste0(output_dir, ".zip")), recursive = TRUE), add = TRUE)

  run_retrospective_forecasts(
    data = df, reference_dates = as.Date("2026-01-24"), horizon = 1, seasonality = "E",
    quantiles_needed = c(0.5), output_dir = output_dir, run_configs = configs,
    auto_ensemble = FALSE, runners = list(copycat = fake_runner),
    run_name = "Copycat noise test"
  )

  loaded <- load_retrospective_run(output_dir)
  expect_equal(loaded$run_name, "Copycat noise test")

  # Simulate a run saved before retrospective_run_settings.csv had a
  # run_name column at all -- must fall back to "", not error or NA.
  settings_path <- file.path(output_dir, "retrospective_run_settings.csv")
  old_settings <- readr::read_csv(settings_path, show_col_types = FALSE)
  readr::write_csv(old_settings[old_settings$field != "run_name", ], settings_path)
  reloaded_old <- load_retrospective_run(output_dir)
  expect_equal(reloaded_old$run_name, "")
})
