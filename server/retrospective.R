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
      "Run retrospective forecasts to generate weekly CSV files."
    ))
  }

  n_files <- nrow(result$files)
  n_success <- nrow(result$successes)
  n_failures <- nrow(result$failures)
  status_class <- if (n_failures > 0) "alert alert-warning" else "alert alert-success"

  div(
    class = status_class,
    style = "padding:8px 12px; margin-bottom:8px;",
    tags$strong("Retrospective run complete. "),
    paste0(n_files, " weekly CSV file(s), ", n_success, " successful model run(s), ", n_failures, " failure(s)."),
    tags$br(),
    tags$span(style = "font-size:.875em;", paste("Saved to", result$output_dir))
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
