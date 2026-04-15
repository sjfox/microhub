# GBQR model ==================================================================

observeEvent(input$run_gbqr, {
  req(rv$raw_data)
  withProgress(message = "GBQR", value = 0, {
    incProgress(0.3, detail = "Fitting model...")

    gbqr_results <- fit_process_gbqr(
      clean_data        = fcast_data(),
      fcast_horizon     = fcast_horizon(),
      quantiles_needed  = rv$quantiles_needed,
      num_bags          = 50,
      bag_frac_samples  = 0.7,
      nrounds           = 100,
      seasonality       = input$seasonality,
      model_type        = input$gbqr_model_type
    )

    gbqr_results_formatted <- format_forecasts(
      forecast_df     = gbqr_results,
      model_name      = "GBQR",
      data_df         = fcast_data(),
      data_to_drop    = input$data_to_drop,
      forecast_date   = input$forecast_date,
      forecast_output = input$forecast_output
    )

    rv$gbqr <- gbqr_results_formatted

    incProgress(0.8, detail = "Plotting results...")

    gbqr_plots <- target_groups() |>
      map(plot_forecasts,
          forecast_df = gbqr_results_formatted,
          data_df     = plot_data(),
          seasonality = input$seasonality)

    gbqr_grid <- plot_grid(plotlist = gbqr_plots, ncol = 1)
    gbqr_grid <- ggdraw(add_sub(
      gbqr_grid,
      "Forecast with the GBQR model.",
      x = 1, hjust = 1, size = 11, color = "gray20"
    ))

    gbqr_plot_path <- paste0(
      "figures/plot-gbqr_",
      get_reference_date_label(gbqr_results_formatted),
      ".png"
    )

    output$gbqr_plots <- renderPlot({
      ggsave(gbqr_plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("gbqr_plot_download")
      gbqr_grid
    })

    incProgress(1)
  })

  output$gbqr_plot_download <- downloadHandler(
    filename = function() gbqr_plot_path,
    content  = function(file) file.copy(gbqr_plot_path, file, overwrite = TRUE)
  )
})
