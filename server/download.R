# Download tab ================================================================

normalize_download_format <- function(format) {
  if (is.null(format) || length(format) == 0 || is.na(format[[1]])) {
    return("csv")
  }

  format <- tolower(as.character(format)[[1]])
  if (identical(format, "parquet")) "parquet" else "csv"
}

download_file_extension <- function(format) {
  normalize_download_format(format)
}

sanitize_download_model_name <- function(model) {
  safe_name <- gsub("[^A-Za-z0-9]+", "_", as.character(model))
  safe_name <- gsub("^_+|_+$", "", safe_name)
  safe_name[is.na(safe_name) | nchar(safe_name) == 0] <- "model"
  substr(safe_name, 1, 80)
}

download_model_file_names <- function(models, reference_date_label, format) {
  safe_names <- make.unique(sanitize_download_model_name(models), sep = "_")
  paste0(
    safe_names,
    "_",
    reference_date_label,
    ".",
    download_file_extension(format)
  )
}

write_forecast_export <- function(df, path, format) {
  format <- normalize_download_format(format)

  if (identical(format, "parquet")) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("The arrow package is required to export Parquet files.", call. = FALSE)
    }
    arrow::write_parquet(df, path)
  } else {
    readr::write_csv(df, path)
  }

  invisible(path)
}

write_forecast_exports_by_model <- function(df, output_zip, format, reference_date_label) {
  format <- normalize_download_format(format)
  model_names <- unique(as.character(df$model))
  file_names <- download_model_file_names(model_names, reference_date_label, format)
  temp_dir <- tempfile("microhub-output-by-model_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  paths <- file.path(temp_dir, file_names)
  for (i in seq_along(model_names)) {
    model_df <- df |>
      dplyr::filter(as.character(model) == model_names[[i]])
    write_forecast_export(model_df, paths[[i]], format)
  }

  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(temp_dir)
  zip_status <- utils::zip(zipfile = output_zip, files = file_names)
  if (!identical(zip_status, 0L)) {
    stop("Failed to create model forecast zip file.", call. = FALSE)
  }

  invisible(output_zip)
}

# Combine all model results into one data frame
combined_results <- reactive({
  req(rv$raw_data)

  combined <- bind_rows(
    rv$baseline_regular,
    rv$baseline_seasonal,
    rv$baseline_opt,
    rv$inla,
    rv$copycat,
    rv$calcopycat,
    rv$fourcat,
    rv$newgbqr,
    rv$pargbqr,
    rv$starima,
    if (length(rv$outside_models) > 0) bind_rows(rv$outside_models) else NULL,
    rv$ensemble
  )

  req(nrow(combined) > 0)

  combined |>
    mutate(
      model           = factor(model),
      reference_date  = format(reference_date, "%Y-%m-%d"),
      horizon         = round(horizon, 0),
      target_end_date = format(target_end_date, "%Y-%m-%d"),
      # Every model's `value` column has already been correctly clipped/
      # rounded by finalize_forecast_value() inside format_forecasts() --
      # re-rounding to 0 decimals here unconditionally used to silently
      # destroy proportion forecasts (e.g. 0.15 -> 0) right before download.
      # Route through the same data-type-aware helper instead of a bare
      # round() so the exported/previewed value matches what every model
      # actually produced.
      value           = finalize_forecast_value(value, data_type())
    )
})

available_download_models <- reactive({
  req(nrow(combined_results()) > 0)
  combined_results() |>
    distinct(model) |>
    pull(model) |>
    as.character()
})

# Keep the download model selector in sync with available forecasts
observe({
  choices <- available_download_models()
  current_selection <- isolate(input$download_models)

  if (is.null(current_selection) || length(current_selection) == 0) {
    selected <- choices
  } else {
    selected <- intersect(current_selection, choices)
    newly_available <- setdiff(choices, current_selection)
    selected <- c(selected, newly_available)
  }

  updateSelectizeInput(
    session,
    "download_models",
    choices = choices,
    selected = selected,
    server = TRUE
  )
})

selected_download_results <- reactive({
  req(nrow(combined_results()) > 0)
  req(length(input$download_models) > 0)

  combined_results() |>
    filter(as.character(model) %in% input$download_models)
})

available_model_plot_results <- reactive({
  plot_results <- list(
    list(
      name = "Regular Baseline",
      forecast_df = rv$baseline_regular,
      caption = "Forecast with the Regular Baseline model."
    ),
    list(
      name = "Seasonal Baseline",
      forecast_df = rv$baseline_seasonal,
      caption = "Forecast with the Seasonal Baseline model."
    ),
    list(
      name = "Opt Baseline",
      forecast_df = rv$baseline_opt,
      caption = "Forecast with the Opt Baseline model."
    ),
    list(
      name = "INFLAenza",
      forecast_df = rv$inla,
      caption = "Forecast with the INFLAenza model."
    ),
    list(
      name = "Copycat",
      forecast_df = rv$copycat,
      caption = "Forecast with the Copycat model."
    ),
    list(
      name = "CalCopycat",
      forecast_df = rv$calcopycat,
      caption = "Forecast with the CalCopycat model."
    ),
    list(
      name = "FourCAT",
      forecast_df = rv$fourcat,
      caption = "Forecast with the FourCAT model."
    ),
    list(
      name = "newGBQR",
      forecast_df = rv$newgbqr,
      caption = "Forecast with the newGBQR model."
    ),
    list(
      name = "parGBQR",
      forecast_df = rv$pargbqr,
      caption = "Forecast with the parGBQR model."
    ),
    list(
      name = "STArima",
      forecast_df = rv$starima,
      caption = "Forecast with the STArima model."
    ),
    list(
      name = "Ensemble",
      forecast_df = rv$ensemble,
      caption = "Forecast with the Ensemble model."
    )
  )

  keep(plot_results, ~ has_forecast_rows(.x$forecast_df))
})

build_download_plot_specs <- function(plot_results) {
  map(plot_results, function(plot_result) {
    list(
      name = plot_result$name,
      plot = build_model_plot(plot_result$forecast_df, plot_result$caption)
    )
  })
}

# Enable download button when results are available
observe({
  if (!is.null(selected_download_results()) && nrow(selected_download_results()) > 0) {
    enable("download_results")
  } else {
    disable("download_results")
  }
})

# Enable plots PDF download button when app-produced plots are available
observe({
  if (length(available_model_plot_results()) > 0) {
    enable("download_plots_pdf")
  } else {
    disable("download_plots_pdf")
  }
})

# Results preview table
output$results_preview <- renderDT({
  req(nrow(selected_download_results()) > 0)
  datatable(
    selected_download_results(),
    rownames  = FALSE,
    filter    = "top",
    selection = "none",
    options   = list(columnDefs = list(list(targets = 0, width = "150px")))
  )
})

# Download forecast results
output$download_results <- downloadHandler(
  filename = function() {
    format <- normalize_download_format(input$download_format)
    reference_date_label <- get_reference_date_label(selected_download_results())

    if (identical(input$download_packaging, "individual")) {
      paste0("microhub-output-by-model_", reference_date_label, ".zip")
    } else {
      paste0(
        "microhub-output_",
        reference_date_label,
        ".",
        download_file_extension(format)
      )
    }
  },
  content = function(filename) {
    format <- normalize_download_format(input$download_format)
    results <- selected_download_results()
    reference_date_label <- get_reference_date_label(results)

    if (identical(input$download_packaging, "individual")) {
      write_forecast_exports_by_model(
        df = results,
        output_zip = filename,
        format = format,
        reference_date_label = reference_date_label
      )
    } else {
      write_forecast_export(results, filename, format)
    }
  }
)

# Download app-produced model plots as a multi-page PDF
output$download_plots_pdf <- downloadHandler(
  filename = function() {
    plot_results <- available_model_plot_results()
    paste0(
      "microhub-plots_",
      get_reference_date_label(plot_results[[1]]$forecast_df),
      ".pdf"
    )
  },
  content = function(filename) {
    plot_results <- available_model_plot_results()
    req(length(plot_results) > 0)

    write_model_plots_pdf(
      plot_specs = build_download_plot_specs(plot_results),
      file = filename
    )
  }
)

# Enable the report button once an Ensemble has been generated
observe({
  if (has_forecast_rows(rv$ensemble)) {
    enable("download_report_pdf")
  } else {
    disable("download_report_pdf")
  }
})

# Download the full ensemble forecast report (PDF)
output$download_report_pdf <- downloadHandler(
  filename = function() {
    paste0("microhub-report_", get_reference_date_label(rv$ensemble), "_",
           input$report_language %||% "en", ".pdf")
  },
  content = function(filename) {
    req(has_forecast_rows(rv$ensemble), rv$raw_data)
    lang <- input$report_language %||% "en"
    tryCatch({
      country <- country_from_upload_filename(
        rv$active_upload_name, epizone_data, default = "Country"
      )
      report <- build_forecast_report(
        country         = country,
        raw_data        = rv$raw_data,
        ensemble        = rv$ensemble,
        forecast_date   = input$forecast_date,
        data_to_drop    = input$data_to_drop,
        seasonality     = input$seasonality,
        # The models/method that actually produced rv$ensemble, snapshotted at
        # "Run Ensemble" time -- NOT input$ensemble_models/input$ensemble_method,
        # which reflect whatever is currently selected in the UI and may have
        # since changed without the ensemble being re-run.
        ensemble_models = rv$ensemble_members,
        ensemble_method = rv$ensemble_method,
        output_models   = setdiff(available_download_models(), "Ensemble"),
        quantiles       = rv$quantiles_needed,
        language        = lang,
        data_type       = data_type()
      )
      write_forecast_report_pdf(report, filename)
    }, error = function(e) {
      # Never let a report error escape the handler: an unhandled error here can tear
      # down the Shiny session (grey "disconnected" overlay that dims the whole app
      # until restart). Write a valid one-page fallback PDF and keep the app running.
      warning("Forecast report generation failed: ", conditionMessage(e))
      write_report_error_pdf(filename, language = lang)
    })
  }
)
