# INFLAenza model =============================================================

observeEvent(input$run_inla, {
  req(rv$raw_data)
  withProgress(message = "INFLAenza", value = 0, {
    incProgress(0.3, detail = "Fitting model...")

    inla_results <- fit_process_inla(
      df                     = fcast_data(),
      weeks_ahead            = fcast_horizon(),
      quantiles_needed       = rv$quantiles_needed,
      forecast_uncertainty   = input$forecast_uncertainty_parameter,
      use_offset             = input$use_population_column == "Yes"
    )

    inla_results_formatted <- format_forecasts(
      forecast_df     = inla_results,
      model_name      = "INFLAenza",
      data_df         = fcast_data(),
      data_to_drop    = input$data_to_drop,
      forecast_date   = input$forecast_date,
      forecast_output = input$forecast_output
    )

    rv$inla <- inla_results_formatted

    inla_plots <- target_groups() |>
      map(plot_forecasts,
          forecast_df = inla_results_formatted,
          data_df     = plot_data(),
          seasonality = input$seasonality)

    inla_grid <- plot_grid(plotlist = inla_plots, ncol = 1)
    inla_grid <- ggdraw(add_sub(
      inla_grid,
      "Forecast with the INFLAenza model.",
      x = 1, hjust = 1, size = 11, color = "gray20"
    ))

    inla_plot_path <- paste0(
      "figures/plot-inla_",
      get_reference_date_label(inla_results_formatted),
      ".png"
    )

    output$inla_plots <- renderPlot({
      ggsave(inla_plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("inla_plot_download")
      inla_grid
    })

    incProgress(1)
  })

  output$inla_plot_download <- downloadHandler(
    filename = function() inla_plot_path,
    content  = function(file) file.copy(inla_plot_path, file, overwrite = TRUE)
  )
})
