# Copycat model ===============================================================

observeEvent(input$run_copycat, {
  req(rv$raw_data)
  withProgress(message = "Copycat", value = 0, {
    incProgress(0.3, detail = "Fitting model...")

    copycat_results <- fit_process_copycat(
      df               = fcast_data(),
      fcast_horizon    = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      recent_weeks_touse = input$recent_weeks_touse,
      resp_week_range  = input$resp_week_range,
      seasonality      = input$seasonality,
      share_groups     = input$copycat_share_groups == "shared"
    )

    copycat_results_formatted <- format_forecasts(
      forecast_df  = copycat_results,
      model_name   = "Copycat",
      data_df      = fcast_data(),
      data_to_drop = input$data_to_drop
    )

    rv$copycat <- copycat_results_formatted

    incProgress(0.8, detail = "Plotting results...")

    copycat_plots <- target_groups() |>
      map(plot_forecasts,
          forecast_df = copycat_results_formatted,
          data_df     = plot_data(),
          seasonality = input$seasonality)

    copycat_grid <- plot_grid(plotlist = copycat_plots, ncol = 1)
    copycat_grid <- ggdraw(add_sub(
      copycat_grid,
      "Forecast with the Copycat model.",
      x = 1, hjust = 1, size = 11, color = "gray20"
    ))

    copycat_plot_path <- paste0("figures/plot-copycat_", Sys.Date(), ".png")

    output$copycat_plots <- renderPlot({
      ggsave(copycat_plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("copycat_plot_download")
      copycat_grid
    })

    incProgress(1)
  })

  output$copycat_plot_download <- downloadHandler(
    filename = function() copycat_plot_path,
    content  = function(file) file.copy(copycat_plot_path, file, overwrite = TRUE)
  )
})
