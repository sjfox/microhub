## FourCAT -------------------------------------------------------------------

observeEvent(input$run_fourcat, {
  req(rv$raw_data)
  withProgress(message = "FourCAT", value = 0, {
    incProgress(0.1, detail = "Wrangling data...")
    
    incProgress(0.3, detail = "Running inference (3 seeds)...")
    
    # Fit
    fourcat_results <- fit_process_fourcat(
      clean_data       = fcast_data(),
      fcast_horizon    = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      zone              = input$seasonality, 
      seeds            = c(41L, 42L, 43L)
    )
    
    # Format
    fourcat_results_formatted <- format_forecasts(
      forecast_df  = fourcat_results,
      model_name   = "FourCAT",
      data_df      = fcast_data(),
      data_to_drop = input$data_to_drop
    )
    
    message("Formatted nrow: ", nrow(fourcat_results_formatted))
    message("Formatted horizons: ", paste(sort(unique(fourcat_results_formatted$horizon)), collapse = ", "))
    message("Formatted dup check: ",
            sum(duplicated(fourcat_results_formatted[, c("horizon", "target_group", "output_type_id", "target_end_date")])))
    
    # Save to reactive values
    rv$fourcat <- fourcat_results_formatted
    
    incProgress(0.8, detail = "Plotting results...")
    
    # Plot FourCAT
    fourcat_plots <- target_groups() |>
      map(
        plot_forecasts,
        forecast_df = fourcat_results_formatted,
        data_df     = plot_data(),
        seasonality = input$seasonality
      )
    
    fourcat_grid <- plot_grid(plotlist = fourcat_plots, ncol = 1)
    fourcat_grid <- ggdraw(add_sub(
      fourcat_grid,
      "Forecast with the FourCAT model.",
      x     = 1,
      hjust = 1,
      size  = 11,
      color = "gray20"
    ))
    
    fourcat_plot_path <- paste0("figures/plot-fourcat_", Sys.Date(), ".png")
    
    output$fourcat_plots <- renderPlot({
      ggsave(
        fourcat_plot_path,
        width  = 8,
        height = 8,
        dpi    = 300,
        bg     = "white"
      )
      
      # Enable plot download button once plot is saved
      enable("fourcat_plot_download")
      
      # Render the plot
      fourcat_grid
    })
    
    incProgress(1)
  })
  
  # Download plot
  output$fourcat_plot_download <- downloadHandler(
    filename = function() {
      fourcat_plot_path
    },
    content = function(file) {
      file.copy(
        fourcat_plot_path,
        file,
        overwrite = TRUE
      )
    }
  )
})

