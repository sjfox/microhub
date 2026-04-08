# Copycat (Calibrated) model ===================================================

observeEvent(input$run_copycat_cal, {
  req(rv$raw_data)
  withProgress(message = "Copycat (Cal.)", value = 0, {
    incProgress(0.2, detail = "Running LOO calibration...")

    copycat_cal_results <- fit_process_copycat_cal(
      df                 = fcast_data(),
      fcast_horizon      = fcast_horizon(),
      quantiles_needed   = rv$quantiles_needed,
      recent_weeks_touse = input$recent_weeks_touse_cal,
      resp_week_range    = input$resp_week_range_cal,
      seasonality        = input$seasonality,
      share_groups       = input$copycat_cal_share_groups == "shared",
      ref_week_window    = input$ref_week_window,
      nsamps_cal         = input$nsamps_cal
    )

    copycat_cal_results_formatted <- format_forecasts(
      forecast_df  = copycat_cal_results,
      model_name   = "Copycat (Cal.)",
      data_df      = fcast_data(),
      data_to_drop = input$data_to_drop
    )

    rv$copycat_cal <- copycat_cal_results_formatted

    incProgress(0.8, detail = "Plotting results...")

    copycat_cal_plots <- target_groups() |>
      map(plot_forecasts,
          forecast_df = copycat_cal_results_formatted,
          data_df     = plot_data(),
          seasonality = input$seasonality)

    copycat_cal_grid <- plot_grid(plotlist = copycat_cal_plots, ncol = 1)
    copycat_cal_grid <- ggdraw(add_sub(
      copycat_cal_grid,
      "Forecast with the Copycat (Calibrated) model.",
      x = 1, hjust = 1, size = 11, color = "gray20"
    ))

    copycat_cal_plot_path <- paste0("figures/plot-copycat_cal_", Sys.Date(), ".png")

    output$copycat_cal_plots <- renderPlot({
      ggsave(copycat_cal_plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("copycat_cal_plot_download")
      copycat_cal_grid
    })

    incProgress(1)
  })

  output$copycat_cal_plot_download <- downloadHandler(
    filename = function() copycat_cal_plot_path,
    content  = function(file) file.copy(copycat_cal_plot_path, file, overwrite = TRUE)
  )
})
