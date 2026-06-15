# Download tab ================================================================

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
      value           = round(value, 0)
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

# Download results CSV
output$download_results <- downloadHandler(
  filename = function() {
    paste0(
      "microhub-output_",
      get_reference_date_label(selected_download_results()),
      ".csv"
    )
  },
  content = function(filename) {
    write.csv(x = selected_download_results(), file = filename, row.names = FALSE)
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
        ensemble_models = input$ensemble_models,
        output_models   = setdiff(available_download_models(), "Ensemble"),
        quantiles       = rv$quantiles_needed,
        language        = lang
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
