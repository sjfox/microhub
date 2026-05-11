# Retrospective forecasting helpers ==========================================

retrospective_model_choices <- c(
  "Regular Baseline" = "baseline_regular",
  "Seasonal Baseline" = "baseline_seasonal",
  "Opt Baseline" = "baseline_opt",
  "INFLAenza" = "inla",
  "Copycat" = "copycat",
  "CalCopycat" = "calcopycat",
  "GBQR" = "gbqr",
  "newGBQR" = "newgbqr",
  "FourCAT" = "fourcat"
)

retrospective_default_settings <- function(has_population = FALSE) {
  list(
    inla = list(
      forecast_uncertainty = "default",
      use_offset = isTRUE(has_population)
    ),
    copycat = list(
      recent_weeks_touse = 100,
      resp_week_range = 2,
      share_groups = TRUE
    ),
    calcopycat = list(
      recent_weeks_touse = 12,
      resp_week_range = 2,
      share_groups = TRUE,
      ref_week_window = 1,
      nsamps_cal = 100
    ),
    gbqr = list(
      model_type = "global"
    ),
    newgbqr = list(
      model_type = "global"
    ),
    fourcat = list(
      seeds = c(41L, 42L, 43L)
    )
  )
}

available_retrospective_reference_dates <- function(data) {
  data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::distinct(date) |>
    dplyr::arrange(date) |>
    dplyr::pull(date) |>
    {\(x) x[-1]}()
}

retrospective_reference_range <- function(data, start_date, end_date) {
  dates <- available_retrospective_reference_dates(data)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  dates[dates >= start_date & dates <= end_date]
}

format_retrospective_forecasts <- function(forecast_df, model_name, reference_date) {
  reference_date <- as.Date(reference_date)

  forecast_df |>
    dplyr::mutate(
      model = model_name,
      reference_date = reference_date,
      horizon = as.integer(horizon) - 1L,
      target_end_date = reference_date + lubridate::weeks(horizon),
      output_type_id = as.character(output_type_id)
    ) |>
    dplyr::select(
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

retrospective_model_runners <- function(settings) {
  list(
    baseline_regular = list(
      label = "Regular Baseline",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_baseline_flat(
          df = train_data,
          weeks_ahead = horizon,
          quantiles_needed = quantiles_needed
        )
      }
    ),
    baseline_seasonal = list(
      label = "Seasonal Baseline",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_baseline_seasonal(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          seasonality = seasonality
        )
      }
    ),
    baseline_opt = list(
      label = "Opt Baseline",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_baseline_flat(
          df = train_data,
          weeks_ahead = horizon,
          quantiles_needed = quantiles_needed,
          window_size = 8
        )
      }
    ),
    inla = list(
      label = "INFLAenza",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_inla(
          df = train_data,
          weeks_ahead = horizon,
          quantiles_needed = quantiles_needed,
          forecast_uncertainty = settings$inla$forecast_uncertainty,
          use_offset = settings$inla$use_offset
        )
      }
    ),
    copycat = list(
      label = "Copycat",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_copycat(
          df = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          recent_weeks_touse = settings$copycat$recent_weeks_touse,
          resp_week_range = settings$copycat$resp_week_range,
          seasonality = seasonality,
          share_groups = settings$copycat$share_groups
        )
      }
    ),
    calcopycat = list(
      label = "CalCopycat",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_calcopycat(
          df = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          recent_weeks_touse = settings$calcopycat$recent_weeks_touse,
          resp_week_range = settings$calcopycat$resp_week_range,
          seasonality = seasonality,
          share_groups = settings$calcopycat$share_groups,
          ref_week_window = settings$calcopycat$ref_week_window,
          nsamps_cal = settings$calcopycat$nsamps_cal
        )
      }
    ),
    gbqr = list(
      label = "GBQR",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_gbqr(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          num_bags = 50,
          bag_frac_samples = 0.7,
          nrounds = 100,
          seasonality = seasonality,
          model_type = settings$gbqr$model_type
        )
      }
    ),
    newgbqr = list(
      label = "newGBQR",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_newgbqr(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          num_bags = 50,
          bag_frac_samples = 0.7,
          nrounds = 100,
          seasonality = seasonality,
          model_type = settings$newgbqr$model_type
        )
      }
    ),
    fourcat = list(
      label = "FourCAT",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_fourcat(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          zone = seasonality,
          seeds = settings$fourcat$seeds
        )
      }
    )
  )
}

write_retrospective_zip <- function(output_dir) {
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  zip_path <- paste0(output_dir, ".zip")
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dir.create(dirname(zip_path), recursive = TRUE, showWarnings = FALSE)
  setwd(output_dir)
  utils::zip(zipfile = zip_path, files = list.files(".", recursive = TRUE), flags = "-q")
  zip_path
}

run_retrospective_forecasts <- function(data,
                                        reference_dates,
                                        models,
                                        horizon,
                                        seasonality,
                                        quantiles_needed,
                                        output_dir,
                                        runners = NULL) {
  data <- data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::arrange(date)
  reference_dates <- sort(as.Date(reference_dates))
  horizon <- as.integer(horizon)

  if (length(reference_dates) == 0) {
    stop("Select at least one retrospective reference week.")
  }
  if (length(models) == 0) {
    stop("Select at least one model.")
  }
  if (length(horizon) != 1 || is.na(horizon) || horizon < 1) {
    stop("Forecast horizon must be a single positive integer.")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  settings <- retrospective_default_settings(has_population = "population" %in% names(data))
  if (is.null(runners)) {
    runners <- retrospective_model_runners(settings)
  }

  unknown_models <- setdiff(models, names(runners))
  if (length(unknown_models) > 0) {
    stop("Unknown retrospective model(s): ", paste(unknown_models, collapse = ", "))
  }

  success_rows <- list()
  failure_rows <- list()
  file_rows <- list()

  for (reference_date_index in seq_along(reference_dates)) {
    reference_date <- reference_dates[[reference_date_index]]
    train_data <- data |>
      dplyr::filter(date < reference_date)
    weekly_results <- list()

    for (model_id in models) {
      runner <- runners[[model_id]]
      model_label <- runner$label

      result <- tryCatch(
        {
          raw_forecast <- runner$run(
            train_data = train_data,
            horizon = horizon,
            quantiles_needed = quantiles_needed,
            seasonality = seasonality
          )

          formatted <- format_retrospective_forecasts(
            forecast_df = raw_forecast,
            model_name = model_label,
            reference_date = reference_date
          )

          list(ok = TRUE, data = formatted, message = "")
        },
        error = function(e) {
          list(ok = FALSE, data = NULL, message = conditionMessage(e))
        }
      )

      if (isTRUE(result$ok)) {
        weekly_results[[model_id]] <- result$data
        success_rows[[length(success_rows) + 1]] <- tibble::tibble(
          reference_date = reference_date,
          model = model_label,
          rows = nrow(result$data)
        )
      } else {
        failure_rows[[length(failure_rows) + 1]] <- tibble::tibble(
          reference_date = reference_date,
          model = model_label,
          message = result$message
        )
      }
    }

    weekly_output <- dplyr::bind_rows(weekly_results)
    if (nrow(weekly_output) > 0) {
      csv_path <- file.path(
        output_dir,
        paste0("retrospective_", format(reference_date, "%Y-%m-%d"), ".csv")
      )
      readr::write_csv(weekly_output, csv_path)
      file_rows[[length(file_rows) + 1]] <- tibble::tibble(
        reference_date = reference_date,
        file = csv_path,
        rows = nrow(weekly_output)
      )
    }
  }

  failures <- dplyr::bind_rows(failure_rows)
  successes <- dplyr::bind_rows(success_rows)
  files <- dplyr::bind_rows(file_rows)
  if (ncol(successes) == 0) {
    successes <- tibble::tibble(
      reference_date = as.Date(character()),
      model = character(),
      rows = integer()
    )
  }
  if (ncol(failures) == 0) {
    failures <- tibble::tibble(
      reference_date = as.Date(character()),
      model = character(),
      message = character()
    )
  }
  if (ncol(files) == 0) {
    files <- tibble::tibble(
      reference_date = as.Date(character()),
      file = character(),
      rows = integer()
    )
  }

  if (nrow(failures) > 0) {
    readr::write_csv(failures, file.path(output_dir, "retrospective_failures.csv"))
  }

  zip_path <- write_retrospective_zip(output_dir)

  list(
    output_dir = output_dir,
    zip_path = zip_path,
    files = files,
    successes = successes,
    failures = failures
  )
}
