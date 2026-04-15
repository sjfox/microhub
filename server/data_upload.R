# Data upload and settings ====================================================

# Country selector → update hidden seasonality radio
observeEvent(input$country_select, {
  req(input$country_select)
  zone <- epizone_data$epi_zone[epizone_data$COUNTRY == input$country_select]
  if (length(zone) == 1 && !is.na(zone)) {
    updateRadioButtons(session, "seasonality", selected = zone)
  }
}, ignoreInit = FALSE)

# Zone badge rendered below the country selector
zone_colors <- c(A = "#0d6efd", B = "#6f42c1", C = "#198754",
                 D = "#fd7e14", E = "#dc3545")

output$zone_badge_ui <- renderUI({
  zone    <- input$seasonality
  country <- input$country_select
  req(zone, country)

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

# Download template
output$download_template <- downloadHandler(
  filename = function() {
    selected <- input$template_choice
    base <- tools::file_path_sans_ext(selected)
    paste0(base, "_", Sys.Date(), ".csv")
  },
  content = function(file) {
    file.copy(file.path("data", input$template_choice), file)
  }
)

clear_data_upload_messages <- function() {
  removeUI(selector = "#error_message > *", immediate = TRUE)
}

clear_pending_upload <- function() {
  rv$pending_upload <- NULL
}

set_upload_status <- function(message = NULL, type = "info") {
  rv$upload_status_message <- if (is.null(message)) {
    NULL
  } else {
    list(message = message, type = type)
  }
}

process_uploaded_data <- function(file_info) {
  clear_data_upload_messages()

  rv$raw_data <- NULL
  rv$valid_data <- NULL
  rv$active_upload_name <- NULL
  set_upload_status()

  validation_results <- tryCatch(
    validate_data(file_info$datapath),
    error = function(e) {
      insertUI(
        selector = "#error_message",
        where = "beforeEnd",
        ui = div(class = "alert alert-danger", paste("Error:", e$message))
      )
      return(NULL)
    }
  )

  if (is.null(validation_results)) {
    return(invisible(NULL))
  }

  if (length(validation_results) == 0) {
    rv$valid_data <- TRUE

    insertUI(
      selector = "#error_message",
      where = "beforeEnd",
      ui = div(
        class = "alert alert-success",
        icon("circle-check", style = "margin-right:2px"),
        "All data validation checks passed!"
      )
    )

    rv$raw_data <- read_raw_data(file_info$datapath)
    rv$active_upload_name <- file_info$name

    updateDateInput(
      session,
      "forecast_date",
      value = closest_wednesday(max(as.Date(rv$raw_data$date), na.rm = TRUE) + 3)
    )
  } else {
    rv$valid_data <- FALSE

    all_errors <- unlist(validation_results, recursive = TRUE)

    insertUI(
      selector = "#error_message",
      where = "beforeEnd",
      ui = div(
        class = "alert alert-danger",
        icon("circle-exclamation", style = "margin-right:2px"),
        tags$strong("Please review the following errors, correct them in your data, and re-upload."),
        tags$br(),
        tags$br(),
        tags$strong("Validation Errors:"),
        tags$ul(lapply(all_errors, tags$li))
      )
    )
  }
}

# Read uploaded data
observeEvent(input$dataframe, {
  req(input$dataframe)

  if (!is.null(rv$raw_data) && has_forecasts_in_memory(rv)) {
    rv$pending_upload <- list(
      name = input$dataframe$name,
      datapath = input$dataframe$datapath,
      size = input$dataframe$size,
      type = input$dataframe$type
    )

    showModal(modalDialog(
      title = "Replace Current Data?",
      tags$p("Uploading this dataset will clear all forecasts currently in memory."),
      tags$p("We recommend downloading your current forecasts before continuing."),
      footer = tagList(
        actionButton("cancel_replace_data", "Cancel", class = "btn btn-secondary"),
        actionButton("confirm_replace_data", "Continue", class = "btn btn-danger")
      ),
      easyClose = FALSE
    ))
  } else {
    clear_pending_upload()
    process_uploaded_data(input$dataframe)
  }
})

observeEvent(input$confirm_replace_data, {
  req(rv$pending_upload)

  removeModal()
  reset_forecast_state(rv, session)
  process_uploaded_data(rv$pending_upload)
  clear_pending_upload()
})

observeEvent(input$cancel_replace_data, {
  removeModal()
  clear_pending_upload()
  set_upload_status("Replacement canceled. The previously loaded dataset remains active.", "warning")
})

# Data preview
output$data_preview <- renderDT({
  req(rv$raw_data)
  datatable(rv$raw_data |> arrange(desc(date)), rownames = FALSE, filter = "top", selection = "none")
})

output$uploaded_time_series_plot <- renderPlot({
  req(rv$raw_data, input$forecast_date, input$data_to_drop)
  plot_uploaded_time_series(
    raw_data = rv$raw_data,
    forecast_date = input$forecast_date,
    data_to_drop = input$data_to_drop
  )
})

output$uploaded_resp_season_plot <- renderPlot({
  req(rv$raw_data, input$forecast_date, input$seasonality)
  plot_uploaded_resp_season_series(
    raw_data = rv$raw_data,
    forecast_date = input$forecast_date,
    seasonality = input$seasonality
  )
})

output$active_dataset_ui <- renderUI({
  if (is.null(rv$active_upload_name) && is.null(rv$upload_status_message)) {
    return(NULL)
  }

  panels <- list()

  if (!is.null(rv$active_upload_name)) {
    panels[[length(panels) + 1]] <- div(
      class = "alert alert-secondary",
      style = "padding:8px 12px; margin-bottom:8px;",
      tags$strong("Current dataset: "),
      rv$active_upload_name
    )
  }

  if (!is.null(rv$upload_status_message)) {
    status_class <- switch(
      rv$upload_status_message$type,
      "warning" = "alert alert-warning",
      "success" = "alert alert-success",
      "danger" = "alert alert-danger",
      "alert alert-info"
    )

    panels[[length(panels) + 1]] <- div(
      class = status_class,
      style = "padding:8px 12px; margin-bottom:8px;",
      rv$upload_status_message$message
    )
  }

  tagList(panels)
})

# Enable/disable run model buttons based on whether data is loaded and valid
observe({
  if (!is.null(input$dataframe) & isTRUE(rv$valid_data)) {
    enable("run_baseline_regular")
    enable("run_baseline_opt")
    enable("run_baseline_seasonal")
    enable("run_inla")
    enable("run_copycat")
    enable("run_copycat_cal")
    enable("run_gbqr")

    # For INLA, also enable and update population button if col exists
    suppressWarnings({
      if (!is.null(rv$raw_data$population)) {
        enable("use_population_column")
        updateRadioButtons(session, "use_population_column", selected = "Yes")
      } else {
        disable("use_population_column")
        updateRadioButtons(session, "use_population_column", selected = "No")
      }
    })
  } else {
    disable("run_baseline_regular")
    disable("run_baseline_opt")
    disable("run_baseline_seasonal")
    disable("run_inla")
    disable("run_copycat")
    disable("run_copycat_cal")
    disable("run_gbqr")
    disable("use_population_column")
    updateRadioButtons(session, "use_population_column", selected = "No")
  }
})
