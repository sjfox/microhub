# Retrospective forecasting ===================================================

retrospective <- reactiveValues(
  raw_data = NULL,
  valid_data = FALSE,
  upload_errors = NULL,
  upload_name = NULL,
  result = NULL
)

disable("run_retrospective")
disable("download_retrospective_zip")

observeEvent(input$retrospective_country_select, {
  req(input$retrospective_country_select)
  zone <- epizone_data$epi_zone[epizone_data$COUNTRY == input$retrospective_country_select]
  if (length(zone) == 1 && !is.na(zone)) {
    updateRadioButtons(session, "retrospective_seasonality", selected = zone)
  }
}, ignoreInit = FALSE)

output$retrospective_zone_badge_ui <- renderUI({
  zone <- input$retrospective_seasonality
  req(zone)

  color <- zone_colors[zone]

  tags$div(
    style = "margin-bottom: 6px;",
    tags$span(
      style = paste0(
        "display:inline-block; background:", color,
        "; color:white; font-weight:600; padding:3px 12px;",
        " border-radius:12px; font-size:.85em;"
      ),
      paste0("Zone ", zone)
    )
  )
})

observeEvent(input$retrospective_file, {
  req(input$retrospective_file)

  retrospective$raw_data <- NULL
  retrospective$valid_data <- FALSE
  retrospective$upload_errors <- NULL
  retrospective$upload_name <- NULL
  retrospective$result <- NULL
  disable("run_retrospective")
  disable("download_retrospective_zip")
  updateSelectInput(session, "retrospective_start_week", choices = NULL)
  updateSelectInput(session, "retrospective_end_week", choices = NULL)

  validation_results <- tryCatch(
    validate_data(input$retrospective_file$datapath),
    error = function(e) list(error = paste("Error:", e$message))
  )

  if (length(validation_results) > 0) {
    retrospective$upload_errors <- unlist(validation_results, recursive = TRUE)
    return(invisible(NULL))
  }

  data <- read_raw_data(input$retrospective_file$datapath)
  reference_dates <- available_retrospective_reference_dates(data)

  if (length(reference_dates) == 0) {
    retrospective$upload_errors <- "The dataset must contain at least two observed weeks."
    return(invisible(NULL))
  }

  retrospective$raw_data <- data
  retrospective$valid_data <- TRUE
  retrospective$upload_name <- input$retrospective_file$name

  date_choices <- setNames(
    format(reference_dates, "%Y-%m-%d"),
    format(reference_dates, "%Y-%m-%d")
  )

  updateSelectInput(
    session,
    "retrospective_start_week",
    choices = date_choices,
    selected = date_choices[[1]]
  )
  updateSelectInput(
    session,
    "retrospective_end_week",
    choices = date_choices,
    selected = date_choices[[length(date_choices)]]
  )
  enable("run_retrospective")
})

output$retrospective_upload_status_ui <- renderUI({
  if (!is.null(retrospective$upload_errors)) {
    return(div(
      class = "alert alert-danger",
      style = "padding:8px 12px; margin-bottom:8px;",
      tags$strong("Validation errors:"),
      tags$ul(lapply(retrospective$upload_errors, tags$li))
    ))
  }

  if (isTRUE(retrospective$valid_data)) {
    return(div(
      class = "alert alert-success",
      style = "padding:8px 12px; margin-bottom:8px;",
      icon("circle-check", style = "margin-right:2px"),
      "Loaded ",
      tags$strong(retrospective$upload_name)
    ))
  }

  NULL
})

output$retrospective_data_preview <- renderDT({
  req(retrospective$raw_data)
  datatable(
    retrospective$raw_data |> arrange(desc(date)),
    rownames = FALSE,
    filter = "top",
    selection = "none"
  )
})

observeEvent(input$select_all_retrospective_models, {
  updateCheckboxGroupInput(
    session,
    "retrospective_models",
    selected = retrospective_model_choices
  )
})

observeEvent(input$clear_retrospective_models, {
  updateCheckboxGroupInput(
    session,
    "retrospective_models",
    selected = character(0)
  )
})

selected_retrospective_reference_dates <- reactive({
  req(retrospective$raw_data)
  req(input$retrospective_start_week, input$retrospective_end_week)

  retrospective_reference_range(
    retrospective$raw_data,
    input$retrospective_start_week,
    input$retrospective_end_week
  )
})

observe({
  can_run <- isTRUE(retrospective$valid_data) &&
    length(selected_retrospective_reference_dates()) > 0 &&
    length(input$retrospective_models) > 0

  toggleState("run_retrospective", condition = can_run)
})

observeEvent(input$run_retrospective, {
  req(retrospective$raw_data, isTRUE(retrospective$valid_data))
  req(length(input$retrospective_models) > 0)

  reference_dates <- selected_retrospective_reference_dates()
  req(length(reference_dates) > 0)

  run_stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  output_dir <- file.path("output", "retrospective", run_stamp)

  disable("run_retrospective")
  disable("download_retrospective_zip")
  on.exit(enable("run_retrospective"), add = TRUE)

  withProgress(message = "Running retrospective forecasts", value = 0, {
    setProgress(value = 0, detail = "Preparing run...")
    last_progress_bucket <- 0L

    result <- run_retrospective_forecasts(
      data = retrospective$raw_data,
      reference_dates = reference_dates,
      models = input$retrospective_models,
      horizon = input$retrospective_horizon,
      seasonality = input$retrospective_seasonality,
      quantiles_needed = rv$quantiles_needed,
      output_dir = output_dir,
      progress_callback = function(reference_date_index, total_reference_dates, reference_date) {
        percent_complete <- floor(reference_date_index / total_reference_dates * 100)
        progress_bucket <- floor(percent_complete / 10) * 10

        if (progress_bucket > last_progress_bucket || reference_date_index == 1L) {
          setProgress(
            value = progress_bucket / 100,
            detail = paste0(
              progress_bucket,
              "% complete; running reference date ",
              format(reference_date, "%Y-%m-%d"),
              " (",
              reference_date_index,
              " of ",
              total_reference_dates,
              ")"
            )
          )
          last_progress_bucket <<- progress_bucket
        }
      }
    )

    setProgress(value = 1, detail = "100% complete")
    retrospective$result <- result
  })

  if (!is.null(retrospective$result$zip_path) &&
      file.exists(retrospective$result$zip_path)) {
    enable("download_retrospective_zip")
  }
})

output$retrospective_run_summary_ui <- renderUI({
  result <- retrospective$result
  if (is.null(result)) {
    return(tags$p(
      class = "plot-helper-text",
      "Run retrospective forecasts to summarize model performance across past forecast dates."
    ))
  }

  completed_dates <- bind_rows(
    result$successes |> select(reference_date),
    result$failures |> select(reference_date)
  ) |>
    distinct(reference_date) |>
    arrange(reference_date) |>
    pull(reference_date)

  forecast_date_label <- if (length(completed_dates) == 0) {
    "No forecast dates completed"
  } else if (length(completed_dates) == 1) {
    format(completed_dates, "%Y-%m-%d")
  } else {
    paste0(
      format(min(completed_dates), "%Y-%m-%d"),
      " to ",
      format(max(completed_dates), "%Y-%m-%d"),
      " (",
      length(completed_dates),
      " dates)"
    )
  }

  target_dates <- result$forecasts |>
    distinct(target_end_date) |>
    arrange(target_end_date) |>
    pull(target_end_date) |>
    as.Date()

  target_date_label <- if (length(target_dates) == 0) {
    "No target dates predicted"
  } else if (length(target_dates) == 1) {
    format(target_dates, "%Y-%m-%d")
  } else {
    paste0(
      format(min(target_dates), "%Y-%m-%d"),
      " to ",
      format(max(target_dates), "%Y-%m-%d")
    )
  }

  attempted_models <- bind_rows(
    result$successes |> select(model),
    result$failures |> select(model)
  ) |>
    distinct(model) |>
    arrange(model) |>
    pull(model)

  successful_models <- result$successes |>
    distinct(model) |>
    arrange(model) |>
    pull(model)

  failed_models <- result$failures |>
    distinct(model) |>
    arrange(model) |>
    pull(model)

  n_failures <- nrow(result$failures)
  status_class <- if (n_failures > 0) "alert alert-warning" else "alert alert-success"
  failure_items <- if (n_failures > 0) {
    result$failures |>
      arrange(reference_date, model) |>
      mutate(
        summary = paste0(
          format(as.Date(reference_date), "%Y-%m-%d"),
          " - ",
          model,
          ": ",
          message
        )
      ) |>
      pull(summary)
  } else {
    character()
  }
  shown_failure_items <- head(failure_items, 6)
  remaining_failures <- max(0, length(failure_items) - length(shown_failure_items))

  div(
    class = status_class,
    style = "padding:10px 12px; margin-bottom:8px;",
    tags$strong("Retrospective run complete"),
    tags$dl(
      style = "display:grid; grid-template-columns:max-content 1fr; column-gap:12px; row-gap:4px; margin:8px 0 0 0;",
      tags$dt("Forecast dates run"),
      tags$dd(style = "margin:0;", forecast_date_label),
      tags$dt("Target dates predicted"),
      tags$dd(style = "margin:0;", target_date_label),
      tags$dt("Models run"),
      tags$dd(style = "margin:0;", if (length(attempted_models) > 0) paste(attempted_models, collapse = ", ") else "None"),
      tags$dt("Succeeded"),
      tags$dd(style = "margin:0;", if (length(successful_models) > 0) paste(successful_models, collapse = ", ") else "None"),
      tags$dt("Failures"),
      tags$dd(style = "margin:0;", if (n_failures > 0) paste0(n_failures, " failed model-date run(s)") else "None")
    ),
    if (length(failed_models) > 0) {
      tagList(
        tags$hr(style = "margin:8px 0;"),
        tags$strong("Failed forecasts"),
        tags$ul(
          style = "margin:6px 0 0 0; padding-left:18px;",
          lapply(shown_failure_items, tags$li)
        ),
        if (remaining_failures > 0) {
          tags$p(
            style = "margin:6px 0 0 0; font-size:.875em;",
            paste0("Plus ", remaining_failures, " additional failure(s).")
          )
        }
      )
    }
  )
})

output$retrospective_ensemble_forecast_plot <- renderPlot({
  req(retrospective$result, retrospective$raw_data)

  plot_retrospective_ensemble_forecasts(
    forecasts = retrospective$result$forecasts,
    actual_data = retrospective$raw_data,
    forecast_stride = 3L
  )
})

output$retrospective_forecast_plot_message_ui <- renderUI({
  req(retrospective$result, retrospective$raw_data)

  plot_data <- retrospective_ensemble_plot_data(
    forecasts = retrospective$result$forecasts,
    actual_data = retrospective$raw_data,
    forecast_stride = 3L
  )

  req(!is.na(plot_data$model))

  if (isTRUE(plot_data$is_ensemble)) {
    return(NULL)
  }

  div(
    class = "alert alert-info",
    style = "padding:8px 12px; margin-bottom:10px;",
    tags$strong("No ensemble forecast was generated. "),
    "Showing ",
    tags$strong(plot_data$model),
    " instead. An ensemble is only produced when at least two non-baseline models complete successfully."
  )
})

output$retrospective_status_table <- renderDT({
  result <- retrospective$result
  req(result)

  successes <- result$successes |>
    mutate(status = "Complete", message = "") |>
    select(reference_date, model, status, rows, message)

  failures <- result$failures |>
    mutate(status = "Failed", rows = 0L) |>
    select(reference_date, model, status, rows, message)

  status <- bind_rows(successes, failures) |>
    arrange(reference_date, model)

  datatable(status, rownames = FALSE, filter = "top", selection = "none")
})

output$retrospective_files_table <- renderDT({
  result <- retrospective$result
  req(result)

  datatable(result$files, rownames = FALSE, filter = "top", selection = "none")
})

format_retrospective_score_summary <- function(score_tbl) {
  score_tbl |>
    mutate(
      across(any_of(c("reference_date")), ~ format(as.Date(.x), "%Y-%m-%d")),
      mean_wis = round(mean_wis, 2),
      mean_relative_wis = round(mean_relative_wis, 3),
      mean_log_wis = round(mean_log_wis, 3),
      mean_relative_log_wis = round(mean_relative_log_wis, 3),
      coverage_50 = round(100 * coverage_50, 1),
      coverage_95 = round(100 * coverage_95, 1)
    ) |>
    rename(
      `Forecast date` = any_of("reference_date"),
      `Target group` = any_of("target_group"),
      Model = model,
      WIS = mean_wis,
      `Relative WIS` = mean_relative_wis,
      `Log WIS` = mean_log_wis,
      `Relative log WIS` = mean_relative_log_wis,
      `50% coverage` = coverage_50,
      `95% coverage` = coverage_95,
      `Forecast targets` = n_forecast_targets
    )
}

retrospective_score_summary_table <- function(score_tbl) {
  datatable(
    format_retrospective_score_summary(score_tbl),
    rownames = FALSE,
    filter = "top",
    selection = "none",
    options = list(pageLength = 10, scrollX = TRUE)
  )
}

output$retrospective_score_overall_table <- renderDT({
  req(retrospective$result)
  score_tbl <- retrospective$result$scores$overall
  req(nrow(score_tbl) > 0)

  retrospective_score_summary_table(score_tbl)
})

output$retrospective_score_target_group_table <- renderDT({
  req(retrospective$result)
  score_tbl <- retrospective$result$scores$by_target_group
  req(nrow(score_tbl) > 0)

  retrospective_score_summary_table(score_tbl)
})

output$retrospective_score_forecast_date_table <- renderDT({
  req(retrospective$result)
  score_tbl <- retrospective$result$scores$by_forecast_date
  req(nrow(score_tbl) > 0)

  retrospective_score_summary_table(score_tbl)
})

output$retrospective_score_target_group_plot <- renderPlot({
  req(retrospective$result)
  score_tbl <- retrospective$result$scores$by_target_group
  req(nrow(score_tbl) > 0)

  plot_retrospective_target_group_scores(score_tbl)
})

output$download_retrospective_score_target_group_plot <- downloadHandler(
  filename = function() {
    "retrospective-target-group-scores.png"
  },
  content = function(file) {
    req(retrospective$result)
    score_tbl <- retrospective$result$scores$by_target_group
    req(nrow(score_tbl) > 0)

    ggplot2::ggsave(
      filename = file,
      plot = plot_retrospective_target_group_scores(score_tbl),
      width = 12,
      height = 7,
      dpi = 300
    )
  }
)

output$retrospective_score_forecast_date_plot <- renderPlot({
  req(retrospective$result)
  score_tbl <- retrospective$result$scores$by_forecast_date
  req(nrow(score_tbl) > 0)

  plot_retrospective_forecast_date_scores(score_tbl)
})

output$download_retrospective_score_forecast_date_plot <- downloadHandler(
  filename = function() {
    "retrospective-forecast-date-scores.png"
  },
  content = function(file) {
    req(retrospective$result)
    score_tbl <- retrospective$result$scores$by_forecast_date
    req(nrow(score_tbl) > 0)

    ggplot2::ggsave(
      filename = file,
      plot = plot_retrospective_forecast_date_scores(score_tbl),
      width = 12,
      height = 7,
      dpi = 300
    )
  }
)

output$download_retrospective_zip <- downloadHandler(
  filename = function() {
    result <- retrospective$result
    if (is.null(result)) {
      return("retrospective.zip")
    }

    paste0(basename(result$output_dir), ".zip")
  },
  content = function(file) {
    req(retrospective$result$zip_path)
    file.copy(retrospective$result$zip_path, file, overwrite = TRUE)
  }
)
