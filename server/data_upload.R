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

# Read uploaded data
observeEvent(input$dataframe, {
  # Remove previous validation messages
  removeUI(selector = "#error_message > *", immediate = TRUE)

  # Reset previously uploaded data and checks
  rv$raw_data   <- NULL
  rv$valid_data <- NULL

  validation_results <- tryCatch(
    validate_data(input$dataframe$datapath),
    error = function(e) {
      insertUI(
        selector = "#error_message",
        where = "beforeEnd",
        ui = div(class = "alert alert-danger", paste("Error:", e$message))
      )
      return(NULL)
    }
  )

  # All checks passed
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

    rv$raw_data <- read_raw_data(input$dataframe$datapath)

    # Update forecast date to the Wednesday nearest to the last data date
    updateDateInput(
      session,
      "forecast_date",
      value = closest_wednesday(max(as.Date(rv$raw_data$date), na.rm = TRUE) + 3)
    )

  # Validation failed
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
})

# Data preview
output$data_preview <- renderDT({
  req(rv$raw_data)
  datatable(rv$raw_data |> arrange(desc(date)), rownames = FALSE, filter = "top", selection = "none")
})

# Enable/disable run model buttons based on whether data is loaded and valid
observe({
  if (!is.null(input$dataframe) & isTRUE(rv$valid_data)) {
    enable("run_baseline_regular")
    enable("run_baseline_opt")
    enable("run_baseline_seasonal")
    enable("run_inla")
    enable("run_copycat")
    enable("run_gbqr")

    # For INLA, also enable and update population button if col exists
    suppressWarnings({
      if (!is.null(rv$raw_data$population)) {
        enable("use_population_column")
        updateRadioButtons(session, "use_population_column", selected = "Yes")
      }
    })
  } else {
    disable("run_baseline_regular")
    disable("run_baseline_opt")
    disable("run_baseline_seasonal")
    disable("run_inla")
    disable("run_copycat")
    disable("run_gbqr")
  }
})
