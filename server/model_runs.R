# Shared model runners ========================================================

input_or_default <- function(value, default) {
  if (is.null(value) || length(value) == 0) {
    return(default)
  }

  value
}

default_model_settings <- function(has_population = FALSE) {
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

with_optional_model_progress <- function(message, show_progress, code) {
  if (isTRUE(show_progress)) {
    withProgress(message = message, value = 0, code)
  } else {
    force(code)
  }
}

model_progress <- function(amount, detail, show_progress) {
  if (isTRUE(show_progress)) {
    incProgress(amount, detail = detail)
  }
}

build_model_plot <- function(forecast_df, caption) {
  plots <- target_groups() |>
    map(
      plot_forecasts,
      forecast_df = forecast_df,
      data_df = plot_data(),
      seasonality = input$seasonality
    )

  grid <- plot_grid(plotlist = plots, ncol = 1)
  ggdraw(add_sub(
    grid,
    caption,
    x = 1,
    hjust = 1,
    size = 11,
    color = "gray20"
  ))
}

clear_model_run_output <- function(rv_key, plot_output_id, download_button_id) {
  rv[[rv_key]] <- NULL
  output[[plot_output_id]] <- renderPlot(NULL)
  disable(download_button_id)
}

clear_default_model_suite_outputs <- function() {
  clear_model_run_output(
    "baseline_regular",
    "baseline_regular_plots",
    "baseline_regular_plot_download"
  )
  clear_model_run_output(
    "baseline_seasonal",
    "baseline_seasonal_plots",
    "baseline_seasonal_plot_download"
  )
  clear_model_run_output(
    "baseline_opt",
    "baseline_opt_plots",
    "baseline_opt_plot_download"
  )
  clear_model_run_output("inla", "inla_plots", "inla_plot_download")
  clear_model_run_output("copycat", "copycat_plots", "copycat_plot_download")
  clear_model_run_output(
    "calcopycat",
    "calcopycat_plots",
    "calcopycat_plot_download"
  )
  clear_model_run_output("fourcat", "fourcat_plots", "fourcat_plot_download")
  clear_model_run_output("gbqr", "gbqr_plots", "gbqr_plot_download")
  clear_model_run_output("newgbqr", "newgbqr_plots", "newgbqr_plot_download")
  clear_model_run_output("ensemble", "ensemble_plots", "ensemble_plot_download")
}

run_baseline_regular_model <- function(show_progress = TRUE) {
  req(rv$raw_data)
  with_optional_model_progress("Regular Baseline", show_progress, {
    model_progress(0.3, "Fitting model...", show_progress)

    results <- fit_process_baseline_flat(
      df = fcast_data(),
      weeks_ahead = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed
    )

    formatted <- format_forecasts(
      forecast_df = results,
      model_name = "Regular Baseline",
      data_df = fcast_data(),
      data_to_drop = input$data_to_drop,
      forecast_date = input$forecast_date,
      forecast_output = input$forecast_output
    )

    rv$baseline_regular <- formatted
    model_progress(0.8, "Plotting results...", show_progress)

    plot_grid_obj <- build_model_plot(
      formatted,
      "Forecast with the Regular Baseline model."
    )
    plot_path <- paste0(
      "figures/plot-baseline_regular_",
      get_reference_date_label(formatted),
      ".png"
    )

    output$baseline_regular_plots <- renderPlot({
      ggsave(plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("baseline_regular_plot_download")
      plot_grid_obj
    })

    output$baseline_regular_plot_download <- downloadHandler(
      filename = function() plot_path,
      content = function(file) file.copy(plot_path, file, overwrite = TRUE)
    )

    model_progress(1, "Done", show_progress)
    invisible(formatted)
  })
}

run_baseline_seasonal_model <- function(show_progress = TRUE) {
  req(rv$raw_data)
  with_optional_model_progress("Seasonal Baseline", show_progress, {
    model_progress(0.3, "Fitting model...", show_progress)

    results <- fit_process_baseline_seasonal(
      clean_data = fcast_data(),
      fcast_horizon = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      seasonality = input$seasonality
    )

    formatted <- format_forecasts(
      forecast_df = results,
      model_name = "Seasonal Baseline",
      data_df = fcast_data(),
      data_to_drop = input$data_to_drop,
      forecast_date = input$forecast_date,
      forecast_output = input$forecast_output
    )

    rv$baseline_seasonal <- formatted
    model_progress(0.8, "Plotting results...", show_progress)

    plot_grid_obj <- build_model_plot(
      formatted,
      "Forecast with the Seasonal Baseline model."
    )
    plot_path <- paste0(
      "figures/plot-baseline_seasonal_",
      get_reference_date_label(formatted),
      ".png"
    )

    output$baseline_seasonal_plots <- renderPlot({
      ggsave(plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("baseline_seasonal_plot_download")
      plot_grid_obj
    })

    output$baseline_seasonal_plot_download <- downloadHandler(
      filename = function() plot_path,
      content = function(file) file.copy(plot_path, file, overwrite = TRUE)
    )

    model_progress(1, "Done", show_progress)
    invisible(formatted)
  })
}

run_baseline_opt_model <- function(show_progress = TRUE) {
  req(rv$raw_data)
  with_optional_model_progress("Opt Baseline", show_progress, {
    model_progress(0.3, "Fitting model...", show_progress)

    results <- fit_process_baseline_flat(
      df = fcast_data(),
      weeks_ahead = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      window_size = 8
    )

    formatted <- format_forecasts(
      forecast_df = results,
      model_name = "Opt Baseline",
      data_df = fcast_data(),
      data_to_drop = input$data_to_drop,
      forecast_date = input$forecast_date,
      forecast_output = input$forecast_output
    )

    rv$baseline_opt <- formatted
    model_progress(0.8, "Plotting results...", show_progress)

    plot_grid_obj <- build_model_plot(
      formatted,
      "Forecast with the Opt Baseline model."
    )
    plot_path <- paste0(
      "figures/plot-baseline_opt_",
      get_reference_date_label(formatted),
      ".png"
    )

    output$baseline_opt_plots <- renderPlot({
      ggsave(plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("baseline_opt_plot_download")
      plot_grid_obj
    })

    output$baseline_opt_plot_download <- downloadHandler(
      filename = function() plot_path,
      content = function(file) file.copy(plot_path, file, overwrite = TRUE)
    )

    model_progress(1, "Done", show_progress)
    invisible(formatted)
  })
}

run_inla_model <- function(
  forecast_uncertainty = input_or_default(input$forecast_uncertainty_parameter, "default"),
  use_offset = input_or_default(input$use_population_column, "No") == "Yes",
  show_progress = TRUE
) {
  req(rv$raw_data)
  with_optional_model_progress("INFLAenza", show_progress, {
    model_progress(0.3, "Fitting model...", show_progress)

    results <- fit_process_inla(
      df = fcast_data(),
      weeks_ahead = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      forecast_uncertainty = forecast_uncertainty,
      use_offset = isTRUE(use_offset)
    )

    formatted <- format_forecasts(
      forecast_df = results,
      model_name = "INFLAenza",
      data_df = fcast_data(),
      data_to_drop = input$data_to_drop,
      forecast_date = input$forecast_date,
      forecast_output = input$forecast_output
    )

    rv$inla <- formatted
    model_progress(0.8, "Plotting results...", show_progress)

    plot_grid_obj <- build_model_plot(
      formatted,
      "Forecast with the INFLAenza model."
    )
    plot_path <- paste0(
      "figures/plot-inla_",
      get_reference_date_label(formatted),
      ".png"
    )

    output$inla_plots <- renderPlot({
      ggsave(plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("inla_plot_download")
      plot_grid_obj
    })

    output$inla_plot_download <- downloadHandler(
      filename = function() plot_path,
      content = function(file) file.copy(plot_path, file, overwrite = TRUE)
    )

    model_progress(1, "Done", show_progress)
    invisible(formatted)
  })
}

run_copycat_model <- function(
  recent_weeks_touse = input_or_default(input$recent_weeks_touse, 100),
  resp_week_range = input_or_default(input$resp_week_range, 2),
  share_groups = input_or_default(input$copycat_share_groups, "shared") == "shared",
  show_progress = TRUE
) {
  req(rv$raw_data)
  with_optional_model_progress("Copycat", show_progress, {
    model_progress(0.3, "Fitting model...", show_progress)

    results <- fit_process_copycat(
      df = fcast_data(),
      fcast_horizon = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      recent_weeks_touse = recent_weeks_touse,
      resp_week_range = resp_week_range,
      seasonality = input$seasonality,
      share_groups = isTRUE(share_groups)
    )

    formatted <- format_forecasts(
      forecast_df = results,
      model_name = "Copycat",
      data_df = fcast_data(),
      data_to_drop = input$data_to_drop,
      forecast_date = input$forecast_date,
      forecast_output = input$forecast_output
    )

    rv$copycat <- formatted
    model_progress(0.8, "Plotting results...", show_progress)

    plot_grid_obj <- build_model_plot(
      formatted,
      "Forecast with the Copycat model."
    )
    plot_path <- paste0(
      "figures/plot-copycat_",
      get_reference_date_label(formatted),
      ".png"
    )

    output$copycat_plots <- renderPlot({
      ggsave(plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("copycat_plot_download")
      plot_grid_obj
    })

    output$copycat_plot_download <- downloadHandler(
      filename = function() plot_path,
      content = function(file) file.copy(plot_path, file, overwrite = TRUE)
    )

    model_progress(1, "Done", show_progress)
    invisible(formatted)
  })
}

run_calcopycat_model <- function(
  recent_weeks_touse = input_or_default(input$recent_weeks_touse_cal, 12),
  resp_week_range = input_or_default(input$resp_week_range_cal, 2),
  share_groups = input_or_default(input$calcopycat_share_groups, "shared") == "shared",
  ref_week_window = input_or_default(input$ref_week_window, 1),
  nsamps_cal = input_or_default(input$nsamps_cal, 100),
  show_progress = TRUE
) {
  req(rv$raw_data)
  with_optional_model_progress("CalCopycat", show_progress, {
    model_progress(0.2, "Running LOO calibration...", show_progress)

    results <- fit_process_calcopycat(
      df = fcast_data(),
      fcast_horizon = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      recent_weeks_touse = recent_weeks_touse,
      resp_week_range = resp_week_range,
      seasonality = input$seasonality,
      share_groups = isTRUE(share_groups),
      ref_week_window = ref_week_window,
      nsamps_cal = nsamps_cal
    )

    formatted <- format_forecasts(
      forecast_df = results,
      model_name = "CalCopycat",
      data_df = fcast_data(),
      data_to_drop = input$data_to_drop,
      forecast_date = input$forecast_date,
      forecast_output = input$forecast_output
    )

    rv$calcopycat <- formatted
    model_progress(0.8, "Plotting results...", show_progress)

    plot_grid_obj <- build_model_plot(
      formatted,
      "Forecast with the CalCopycat model."
    )
    plot_path <- paste0(
      "figures/plot-calcopycat_",
      get_reference_date_label(formatted),
      ".png"
    )

    output$calcopycat_plots <- renderPlot({
      ggsave(plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("calcopycat_plot_download")
      plot_grid_obj
    })

    output$calcopycat_plot_download <- downloadHandler(
      filename = function() plot_path,
      content = function(file) file.copy(plot_path, file, overwrite = TRUE)
    )

    model_progress(1, "Done", show_progress)
    invisible(formatted)
  })
}

run_gbqr_model <- function(
  model_type = input_or_default(input$gbqr_model_type, "global"),
  show_progress = TRUE
) {
  req(rv$raw_data)
  with_optional_model_progress("GBQR", show_progress, {
    model_progress(0.3, "Fitting model...", show_progress)

    results <- fit_process_gbqr(
      clean_data = fcast_data(),
      fcast_horizon = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      num_bags = 50,
      bag_frac_samples = 0.7,
      nrounds = 100,
      seasonality = input$seasonality,
      model_type = model_type
    )

    formatted <- format_forecasts(
      forecast_df = results,
      model_name = "GBQR",
      data_df = fcast_data(),
      data_to_drop = input$data_to_drop,
      forecast_date = input$forecast_date,
      forecast_output = input$forecast_output
    )

    rv$gbqr <- formatted
    model_progress(0.8, "Plotting results...", show_progress)

    plot_grid_obj <- build_model_plot(
      formatted,
      "Forecast with the GBQR model."
    )
    plot_path <- paste0(
      "figures/plot-gbqr_",
      get_reference_date_label(formatted),
      ".png"
    )

    output$gbqr_plots <- renderPlot({
      ggsave(plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("gbqr_plot_download")
      plot_grid_obj
    })

    output$gbqr_plot_download <- downloadHandler(
      filename = function() plot_path,
      content = function(file) file.copy(plot_path, file, overwrite = TRUE)
    )

    model_progress(1, "Done", show_progress)
    invisible(formatted)
  })
}

run_newgbqr_model <- function(
  model_type = input_or_default(input$newgbqr_model_type, "global"),
  show_progress = TRUE
) {
  req(rv$raw_data)
  with_optional_model_progress("newGBQR", show_progress, {
    model_progress(0.3, "Fitting model...", show_progress)

    results <- fit_process_newgbqr(
      clean_data = fcast_data(),
      fcast_horizon = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      num_bags = 50,
      bag_frac_samples = 0.7,
      nrounds = 100,
      seasonality = input$seasonality,
      model_type = model_type
    )

    formatted <- format_forecasts(
      forecast_df = results,
      model_name = "newGBQR",
      data_df = fcast_data(),
      data_to_drop = input$data_to_drop,
      forecast_date = input$forecast_date,
      forecast_output = input$forecast_output
    )

    rv$newgbqr <- formatted
    model_progress(0.8, "Plotting results...", show_progress)

    plot_grid_obj <- build_model_plot(
      formatted,
      "Forecast with the newGBQR model."
    )
    plot_path <- paste0(
      "figures/plot-newgbqr_",
      get_reference_date_label(formatted),
      ".png"
    )

    output$newgbqr_plots <- renderPlot({
      ggsave(plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("newgbqr_plot_download")
      plot_grid_obj
    })

    output$newgbqr_plot_download <- downloadHandler(
      filename = function() plot_path,
      content = function(file) file.copy(plot_path, file, overwrite = TRUE)
    )

    model_progress(1, "Done", show_progress)
    invisible(formatted)
  })
}

run_fourcat_model <- function(
  seeds = default_model_settings()$fourcat$seeds,
  show_progress = TRUE
) {
  req(rv$raw_data)
  with_optional_model_progress("FourCAT", show_progress, {
    model_progress(0.1, "Wrangling data...", show_progress)
    model_progress(0.3, "Running inference (3 seeds)...", show_progress)

    results <- fit_process_fourcat(
      clean_data = fcast_data(),
      fcast_horizon = fcast_horizon(),
      quantiles_needed = rv$quantiles_needed,
      zone = input$seasonality,
      seeds = seeds
    )

    formatted <- format_forecasts(
      forecast_df = results,
      model_name = "FourCAT",
      data_df = fcast_data(),
      data_to_drop = input$data_to_drop,
      forecast_date = input$forecast_date,
      forecast_output = input$forecast_output
    )

    rv$fourcat <- formatted
    model_progress(0.8, "Plotting results...", show_progress)

    plot_grid_obj <- build_model_plot(
      formatted,
      "Forecast with the FourCAT model."
    )
    plot_path <- paste0(
      "figures/plot-fourcat_",
      get_reference_date_label(formatted),
      ".png"
    )

    output$fourcat_plots <- renderPlot({
      ggsave(plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("fourcat_plot_download")
      plot_grid_obj
    })

    output$fourcat_plot_download <- downloadHandler(
      filename = function() plot_path,
      content = function(file) file.copy(plot_path, file, overwrite = TRUE)
    )

    model_progress(1, "Done", show_progress)
    invisible(formatted)
  })
}

run_default_model_suite <- function() {
  req(rv$raw_data, isTRUE(rv$valid_data))

  defaults <- default_model_settings(has_population = "population" %in% names(rv$raw_data))
  model_steps <- list(
    list(name = "Regular Baseline", run = function() run_baseline_regular_model(show_progress = FALSE)),
    list(name = "Seasonal Baseline", run = function() run_baseline_seasonal_model(show_progress = FALSE)),
    list(name = "Opt Baseline", run = function() run_baseline_opt_model(show_progress = FALSE)),
    list(
      name = "INFLAenza",
      run = function() run_inla_model(
        forecast_uncertainty = defaults$inla$forecast_uncertainty,
        use_offset = defaults$inla$use_offset,
        show_progress = FALSE
      )
    ),
    list(
      name = "CalCopycat",
      run = function() run_calcopycat_model(
        recent_weeks_touse = defaults$calcopycat$recent_weeks_touse,
        resp_week_range = defaults$calcopycat$resp_week_range,
        share_groups = defaults$calcopycat$share_groups,
        ref_week_window = defaults$calcopycat$ref_week_window,
        nsamps_cal = defaults$calcopycat$nsamps_cal,
        show_progress = FALSE
      )
    ),
    list(
      name = "Copycat",
      run = function() run_copycat_model(
        recent_weeks_touse = defaults$copycat$recent_weeks_touse,
        resp_week_range = defaults$copycat$resp_week_range,
        share_groups = defaults$copycat$share_groups,
        show_progress = FALSE
      )
    ),
    list(
      name = "GBQR",
      run = function() run_gbqr_model(
        model_type = defaults$gbqr$model_type,
        show_progress = FALSE
      )
    ),
    list(
      name = "newGBQR",
      run = function() run_newgbqr_model(
        model_type = defaults$newgbqr$model_type,
        show_progress = FALSE
      )
    ),
    list(
      name = "FourCAT",
      run = function() run_fourcat_model(
        seeds = defaults$fourcat$seeds,
        show_progress = FALSE
      )
    )
  )

  rv$run_all_results <- tibble(
    model = vapply(model_steps, `[[`, character(1), "name"),
    status = "Pending",
    message = ""
  )
  clear_default_model_suite_outputs()

  disable("run_all_default_models")
  on.exit(enable("run_all_default_models"), add = TRUE)

  withProgress(message = "Running all default models", value = 0, {
    total <- length(model_steps)

    for (i in seq_along(model_steps)) {
      step <- model_steps[[i]]
      rv$run_all_results$status[i] <- "Running"
      rv$run_all_results$message[i] <- ""
      incProgress(0, detail = paste("Running", step$name))

      result <- tryCatch(
        {
          step$run()
          list(status = "Complete", message = "")
        },
        error = function(e) {
          list(status = "Failed", message = conditionMessage(e))
        }
      )

      rv$run_all_results$status[i] <- result$status
      rv$run_all_results$message[i] <- result$message
      incProgress(1 / total)
    }
  })

  invisible(rv$run_all_results)
}

observeEvent(input$run_all_default_models, {
  run_default_model_suite()
})

output$run_all_status_ui <- renderUI({
  results <- rv$run_all_results
  if (is.null(results) || nrow(results) == 0) {
    return(NULL)
  }

  status_class <- if (all(results$status == "Complete")) {
    "alert alert-success"
  } else if (any(results$status == "Failed")) {
    "alert alert-warning"
  } else {
    "alert alert-info"
  }

  tags$div(
    class = status_class,
    style = "padding:8px 12px; margin-top:8px; margin-bottom:8px;",
    tags$strong("Run-all status"),
    tags$ul(
      style = "margin-bottom:0; padding-left:18px;",
      lapply(seq_len(nrow(results)), function(i) {
        msg <- if (nzchar(results$message[i])) {
          paste0(": ", results$message[i])
        } else {
          ""
        }

        tags$li(paste0(results$model[i], " - ", results$status[i], msg))
      })
    )
  )
})
