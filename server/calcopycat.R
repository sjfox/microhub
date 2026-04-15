# CalCopycat model =============================================================

observeEvent(input$run_calcopycat, {
  req(rv$raw_data)
  withProgress(message = "CalCopycat", value = 0, {
    incProgress(0.2, detail = "Running LOO calibration...")

    calcopycat_results <- fit_process_calcopycat(
      df                 = fcast_data(),
      fcast_horizon      = fcast_horizon(),
      quantiles_needed   = rv$quantiles_needed,
      recent_weeks_touse = input$recent_weeks_touse_cal,
      resp_week_range    = input$resp_week_range_cal,
      seasonality        = input$seasonality,
      share_groups       = input$calcopycat_share_groups == "shared",
      ref_week_window    = input$ref_week_window,
      nsamps_cal         = input$nsamps_cal
    )

    calcopycat_results_formatted <- format_forecasts(
      forecast_df  = calcopycat_results,
      model_name   = "CalCopycat",
      data_df      = fcast_data(),
      data_to_drop = input$data_to_drop
    )

    rv$calcopycat <- calcopycat_results_formatted

    incProgress(0.8, detail = "Plotting results...")

    calcopycat_plots <- target_groups() |>
      map(plot_forecasts,
          forecast_df = calcopycat_results_formatted,
          data_df     = plot_data(),
          seasonality = input$seasonality)

    calcopycat_grid <- plot_grid(plotlist = calcopycat_plots, ncol = 1)
    calcopycat_grid <- ggdraw(add_sub(
      calcopycat_grid,
      "Forecast with the CalCopycat model.",
      x = 1, hjust = 1, size = 11, color = "gray20"
    ))

    calcopycat_plot_path <- paste0("figures/plot-calcopycat_", Sys.Date(), ".png")

    output$calcopycat_plots <- renderPlot({
      ggsave(calcopycat_plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("calcopycat_plot_download")
      calcopycat_grid
    })

    incProgress(1)
  })

  output$calcopycat_plot_download <- downloadHandler(
    filename = function() calcopycat_plot_path,
    content  = function(file) file.copy(calcopycat_plot_path, file, overwrite = TRUE)
  )
})
