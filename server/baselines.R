# Baseline models =============================================================

## Regular Baseline ------------------------------------------------------------

observeEvent(input$run_baseline_regular, {
  req(rv$raw_data)
  withProgress(message = "Regular Baseline", value = 0, {
    incProgress(0.3, detail = "Fitting model...")

    baseline_regular_results <- fit_process_baseline_flat(
      df = fcast_data(),
      weeks_ahead = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed
    )

    baseline_regular_results_formatted <- format_forecasts(
      forecast_df = baseline_regular_results,
      model_name  = "Regular Baseline",
      data_df     = fcast_data(),
      data_to_drop = input$data_to_drop
    )

    rv$baseline_regular <- baseline_regular_results_formatted

    incProgress(0.8, detail = "Plotting results...")

    baseline_regular_plots <- target_groups() |>
      map(plot_forecasts,
          forecast_df  = baseline_regular_results_formatted,
          data_df      = plot_data(),
          seasonality  = input$seasonality)

    baseline_regular_grid <- plot_grid(plotlist = baseline_regular_plots, ncol = 1)
    baseline_regular_grid <- ggdraw(add_sub(
      baseline_regular_grid,
      "Forecast with the Regular Baseline model.",
      x = 1, hjust = 1, size = 11, color = "gray20"
    ))

    baseline_regular_plot_path <- paste0("figures/plot-baseline_regular_", Sys.Date(), ".png")

    output$baseline_regular_plots <- renderPlot({
      ggsave(baseline_regular_plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("baseline_regular_plot_download")
      baseline_regular_grid
    })

    incProgress(1)
  })

  output$baseline_regular_plot_download <- downloadHandler(
    filename = function() baseline_regular_plot_path,
    content  = function(file) file.copy(baseline_regular_plot_path, file, overwrite = TRUE)
  )
})

## Seasonal Baseline -----------------------------------------------------------

observeEvent(input$run_baseline_seasonal, {
  req(rv$raw_data)
  withProgress(message = "Seasonal Baseline", value = 0, {
    incProgress(0.3, detail = "Fitting model...")

    baseline_seasonal_results <- fit_process_baseline_seasonal(
      clean_data       = fcast_data(),
      fcast_horizon    = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      seasonality      = input$seasonality
    )

    baseline_seasonal_results_formatted <- format_forecasts(
      forecast_df  = baseline_seasonal_results,
      model_name   = "Seasonal Baseline",
      data_df      = fcast_data(),
      data_to_drop = input$data_to_drop
    )

    rv$baseline_seasonal <- baseline_seasonal_results_formatted

    incProgress(0.8, detail = "Plotting results...")

    baseline_seasonal_plots <- target_groups() |>
      map(plot_forecasts,
          forecast_df = baseline_seasonal_results_formatted,
          data_df     = plot_data(),
          seasonality = input$seasonality)

    baseline_seasonal_grid <- plot_grid(plotlist = baseline_seasonal_plots, ncol = 1)
    baseline_seasonal_grid <- ggdraw(add_sub(
      baseline_seasonal_grid,
      "Forecast with the Seasonal Baseline model.",
      x = 1, hjust = 1, size = 11, color = "gray20"
    ))

    baseline_seasonal_plot_path <- paste0("figures/plot-baseline_seasonal_", Sys.Date(), ".png")

    output$baseline_seasonal_plots <- renderPlot({
      ggsave(baseline_seasonal_plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("baseline_seasonal_plot_download")
      baseline_seasonal_grid
    })

    incProgress(1)
  })

  output$baseline_seasonal_plot_download <- downloadHandler(
    filename = function() baseline_seasonal_plot_path,
    content  = function(file) file.copy(baseline_seasonal_plot_path, file, overwrite = TRUE)
  )
})

## Opt Baseline ----------------------------------------------------------------

observeEvent(input$run_baseline_opt, {
  req(rv$raw_data)
  withProgress(message = "Opt Baseline", value = 0, {
    incProgress(0.3, detail = "Fitting model...")

    baseline_opt_results <- fit_process_baseline_flat(
      df               = fcast_data(),
      weeks_ahead      = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      window_size      = 8
    )

    baseline_opt_results_formatted <- format_forecasts(
      forecast_df  = baseline_opt_results,
      model_name   = "Opt Baseline",
      data_df      = fcast_data(),
      data_to_drop = input$data_to_drop
    )

    rv$baseline_opt <- baseline_opt_results_formatted

    incProgress(0.8, detail = "Plotting results...")

    baseline_opt_plots <- target_groups() |>
      map(plot_forecasts,
          forecast_df = baseline_opt_results_formatted,
          data_df     = plot_data(),
          seasonality = input$seasonality)

    baseline_opt_grid <- plot_grid(plotlist = baseline_opt_plots, ncol = 1)
    baseline_opt_grid <- ggdraw(add_sub(
      baseline_opt_grid,
      "Forecast with the Opt Baseline model.",
      x = 1, hjust = 1, size = 11, color = "gray20"
    ))

    baseline_opt_plot_path <- paste0("figures/plot-baseline_opt_", Sys.Date(), ".png")

    output$baseline_opt_plots <- renderPlot({
      ggsave(baseline_opt_plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("baseline_opt_plot_download")
      baseline_opt_grid
    })

    incProgress(1)
  })

  output$baseline_opt_plot_download <- downloadHandler(
    filename = function() baseline_opt_plot_path,
    content  = function(file) file.copy(baseline_opt_plot_path, file, overwrite = TRUE)
  )
})
