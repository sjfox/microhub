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

selected_data_target_group <- reactiveVal(NULL)

observeEvent(target_groups(), {
  groups <- target_groups()
  req(length(groups) > 0)

  current_group <- isolate(selected_data_target_group())
  selected_group <- if (!is.null(current_group) && current_group %in% groups) {
    current_group
  } else if ("Overall" %in% groups) {
    "Overall"
  } else {
    groups[[1]]
  }

  selected_data_target_group(selected_group)
  updateSelectizeInput(
    session,
    "data_target_group_select",
    choices = groups,
    selected = selected_group,
    server = TRUE
  )
}, ignoreInit = FALSE)

observeEvent(input$data_target_group_select, {
  req(input$data_target_group_select)
  selected_data_target_group(input$data_target_group_select)
}, ignoreInit = TRUE)

move_data_target_group <- function(step) {
  groups <- target_groups()
  req(length(groups) > 0)

  current_group <- selected_data_target_group()
  current_index <- match(current_group, groups)
  if (is.na(current_index)) {
    current_index <- 1L
  }

  next_index <- ((current_index - 1L + step) %% length(groups)) + 1L
  next_group <- groups[[next_index]]

  selected_data_target_group(next_group)
  updateSelectizeInput(
    session,
    "data_target_group_select",
    selected = next_group
  )
}

observeEvent(input$previous_data_target_group, {
  move_data_target_group(-1L)
})

observeEvent(input$next_data_target_group, {
  move_data_target_group(1L)
})

output$data_target_group_position_ui <- renderUI({
  groups <- target_groups()
  current_group <- selected_data_target_group()
  req(length(groups) > 0, current_group)

  current_index <- match(current_group, groups)
  if (is.na(current_index)) {
    current_index <- 1L
  }

  tags$p(
    class = "plot-helper-text",
    style = "margin:0;",
    paste0(current_index, " of ", length(groups), ": ", current_group)
  )
})

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

# Neighbor graph upload =======================================================
# Feedback is rendered from reactive state rather than injected with insertUI
# (as the main data upload does), because the graph is re-validated whenever the
# primary dataset changes and insertUI would accumulate stale alerts.

output$neighbor_graph_template_ui <- renderUI({
  # Deliberately NOT gated on rv$raw_data: with no data loaded the handler
  # serves the static data/microhub-template-neighbor-graph.csv, which pairs
  # with data/microhub-template.csv, so someone exploring the bundled templates
  # can pick up both before uploading anything.
  downloadButton(
    "download_neighbor_graph_template",
    label = "Download Neighbor Template (.csv)",
    class = "btn-sm",
    style = "margin-bottom:8px;"
  )
})

# Built from the groups actually uploaded, so the names are guaranteed to match.
#
# Two shapes, because the useful template differs with the number of groups.
# Listing every possible pair turns the user's job into DELETION, and the number
# of pairs grows quadratically while real adjacency grows roughly linearly: with
# 5 age groups that is 10 rows to prune, but with the 255 countries in
# data/epizone_assignment_March2026.csv it is 32,385 rows of which a real
# neighbor list keeps only a few hundred. Nobody prunes that by hand. So above a
# threshold the template becomes a header plus a couple of example rows -- the
# user's job is then ADDITION, which is the shape the real answer has.
NEIGHBOR_TEMPLATE_MAX_PAIRS <- 200L

output$download_neighbor_graph_template <- downloadHandler(
  filename = function() paste0("microhub-template-neighbor-graph_", Sys.Date(), ".csv"),
  content = function(file) {
    static_template <- file.path("data", "microhub-template-neighbor-graph.csv")
    if (is.null(rv$raw_data)) {
      file.copy(static_template, file, overwrite = TRUE)
      return(invisible(NULL))
    }

    groups <- as.character(setdiff(target_groups(), "Overall"))
    n <- length(groups)

    template <- if (n < 2) {
      data.frame(
        target_group = character(0),
        neighbor = character(0),
        stringsAsFactors = FALSE
      )
    } else if (choose(n, 2) <= NEIGHBOR_TEMPLATE_MAX_PAIRS) {
      # Small panel: every pair, so the user deletes the ones that aren't
      # neighbors. Convenient at this size and impossible to mistype.
      pairs <- utils::combn(groups, 2)
      data.frame(
        target_group = pairs[1, ],
        neighbor = pairs[2, ],
        stringsAsFactors = FALSE
      )
    } else {
      # Large panel: a worked example using this dataset's own group names,
      # for the user to extend or replace.
      data.frame(
        target_group = groups[c(1, 2)],
        neighbor = groups[c(2, 3)],
        stringsAsFactors = FALSE
      )
    }

    utils::write.csv(template, file, row.names = FALSE)
  }
)

# The spatial structure is only offered once a graph is actually loaded.
# Without this the option is selectable with no graph, and the fit silently
# falls back -- in a retrospective run that produces rows labelled
# "besagproper" that were fit as "exchangeable".
INLA_INTERACTION_BASE_CHOICES <- c(
  "Exchangeable (default)" = "exchangeable",
  "Independent (iid)" = "iid",
  "None (shared trend)" = "none",
  # The "+ main trend" variants add a separate main temporal effect. They need
  # no graph, and exist so a spatial fit can be compared against a non-spatial
  # one that has the same main term -- see INLA_INTERACTION_CHOICES in R/inla.R.
  "Exchangeable + main trend" = "exchangeable_main",
  "Independent + main trend" = "iid_main"
)

observe({
  has_graph <- !is.null(rv$neighbor_graph) && nrow(rv$neighbor_graph) > 0

  choices <- INLA_INTERACTION_BASE_CHOICES
  if (has_graph) {
    choices <- c(choices, "Spatial (neighbor graph)" = "besagproper")
  }

  # Uploading a neighbor graph IS the opt-in: select the spatial structure
  # rather than merely offering it. This matches how a population column
  # auto-selects the offset above -- an optional upload that changes nothing
  # visible reads as broken. The user can still pick any other structure
  # afterwards; this observer only re-fires when the graph itself changes, so a
  # deliberate choice is not overwritten.
  current <- isolate(input$inla_interaction)
  selected <- if (has_graph) {
    "besagproper"
  } else if (!is.null(current) && current %in% choices) {
    current
  } else {
    "exchangeable"
  }

  updateSelectizeInput(
    session,
    "inla_interaction",
    choices = choices,
    selected = selected
  )
})

INLA_SEASONAL_BASE_CHOICES <- c(
  "Shared across all groups (default)" = "shared",
  "One curve per target group" = "target_group"
)

observe({
  has_groups <- !is.null(rv$season_groups) && nrow(rv$season_groups) > 0

  choices <- INLA_SEASONAL_BASE_CHOICES
  if (has_groups) {
    choices <- c(choices, "One curve per seasonal group" = "season_group")
  }

  # Same reasoning as the neighbor graph above: the upload is the opt-in.
  current <- isolate(input$inla_seasonal)
  selected <- if (has_groups) {
    "season_group"
  } else if (!is.null(current) && current %in% choices) {
    current
  } else {
    "shared"
  }

  updateSelectizeInput(session, "inla_seasonal", choices = choices, selected = selected)
})

observeEvent(input$neighbor_graph_file, {
  req(input$neighbor_graph_file)

  if (is.null(rv$raw_data)) {
    rv$neighbor_graph_validation <- list(
      errors = list(no_data = "Upload your time series data before the neighbor graph, so group names can be checked against it."),
      warnings = list(),
      data = NULL
    )
    return(invisible(NULL))
  }

  result <- validate_neighbor_graph(
    file = input$neighbor_graph_file$datapath,
    target_groups = target_groups()
  )

  rv$neighbor_graph_validation <- result

  if (!is.null(result$data)) {
    rv$neighbor_graph <- result$data
    rv$neighbor_graph_name <- input$neighbor_graph_file$name
  } else {
    rv$neighbor_graph <- NULL
    rv$neighbor_graph_name <- NULL
  }
})

output$neighbor_graph_status_ui <- renderUI({
  result <- rv$neighbor_graph_validation
  if (is.null(result)) {
    return(NULL)
  }

  errors   <- result$errors
  warnings <- result$warnings

  if (length(errors) > 0) {
    return(div(
      class = "alert alert-danger",
      icon("circle-exclamation", style = "margin-right:2px"),
      tags$strong(paste0(length(errors), " error(s) — neighbor graph was not added")),
      tags$ul(lapply(unlist(errors, recursive = TRUE), tags$li))
    ))
  }

  tagList(
    div(
      class = "alert alert-success",
      icon("circle-check", style = "margin-right:2px"),
      paste0(
        "Neighbor graph loaded",
        if (!is.null(rv$neighbor_graph_name)) paste0(" (", rv$neighbor_graph_name, ")") else "",
        ": ", nrow(rv$neighbor_graph), " neighbor pair(s). ",
        "INFLAenza's Group structure has been set to \"Spatial (neighbor graph)\"."
      )
    ),
    if (length(warnings) > 0) {
      div(
        class = "alert alert-warning",
        icon("triangle-exclamation", style = "margin-right:2px"),
        tags$ul(lapply(unlist(warnings, recursive = TRUE), tags$li))
      )
    }
  )
})

# Seasonal groups upload ======================================================
# Deliberately a separate declaration from the neighbor graph: spatial adjacency
# and seasonal regime are different things that merely correlate. Alaska is
# spatially isolated but temperate; south Florida borders Georgia but is
# subtropical. Deriving one from the other would make it impossible to say
# "Hawaii and Puerto Rico share a tropical curve".

output$season_groups_template_ui <- renderUI({
  downloadButton(
    "download_season_groups_template",
    label = "Download Seasonal Group Template (.csv)",
    class = "btn-sm",
    style = "margin-bottom:8px;"
  )
})

output$download_season_groups_template <- downloadHandler(
  filename = function() paste0("microhub-template-season-groups_", Sys.Date(), ".csv"),
  content = function(file) {
    if (is.null(rv$raw_data)) {
      file.copy(file.path("data", "microhub-template-season-groups.csv"), file, overwrite = TRUE)
      return(invisible(NULL))
    }

    groups <- as.character(setdiff(target_groups(), "Overall"))

    # Every group listed in one shared season group: a starting point the user
    # edits by changing the labels of the few groups that differ. Unlike the
    # neighbor template this stays linear in group count, so there is no need
    # for a separate large-panel shape.
    template <- data.frame(
      target_group = groups,
      season_group = rep("Group 1", length(groups)),
      stringsAsFactors = FALSE
    )

    utils::write.csv(template, file, row.names = FALSE)
  }
)

observeEvent(input$season_groups_file, {
  req(input$season_groups_file)

  if (is.null(rv$raw_data)) {
    rv$season_groups_validation <- list(
      errors = list(no_data = "Upload your time series data before the seasonal groups, so group names can be checked against it."),
      warnings = list(),
      data = NULL
    )
    return(invisible(NULL))
  }

  result <- validate_season_groups(
    file = input$season_groups_file$datapath,
    target_groups = target_groups()
  )

  rv$season_groups_validation <- result

  if (!is.null(result$data)) {
    rv$season_groups <- result$data
    rv$season_groups_name <- input$season_groups_file$name
  } else {
    rv$season_groups <- NULL
    rv$season_groups_name <- NULL
  }
})

output$season_groups_status_ui <- renderUI({
  result <- rv$season_groups_validation
  if (is.null(result)) return(NULL)

  if (length(result$errors) > 0) {
    return(div(
      class = "alert alert-danger",
      icon("circle-exclamation", style = "margin-right:2px"),
      tags$strong(paste0(length(result$errors), " error(s) — seasonal groups were not added")),
      tags$ul(lapply(unlist(result$errors, recursive = TRUE), tags$li))
    ))
  }

  n_curves <- length(unique(rv$season_groups$season_group))
  tagList(
    div(
      class = "alert alert-success",
      icon("circle-check", style = "margin-right:2px"),
      paste0(
        "Seasonal groups loaded",
        if (!is.null(rv$season_groups_name)) paste0(" (", rv$season_groups_name, ")") else "",
        ": ", nrow(rv$season_groups), " target group(s) assigned across ",
        n_curves, " named group(s). ",
        "INFLAenza's Seasonality has been set to \"One curve per seasonal group\"."
      )
    ),
    if (length(result$warnings) > 0) {
      div(
        class = "alert alert-warning",
        icon("triangle-exclamation", style = "margin-right:2px"),
        tags$ul(lapply(unlist(result$warnings, recursive = TRUE), tags$li))
      )
    }
  )
})

process_uploaded_data <- function(file_info) {
  clear_data_upload_messages()

  rv$raw_data <- NULL
  rv$valid_data <- NULL
  rv$active_upload_name <- NULL
  rv$run_all_results <- NULL
  set_upload_status()

  # Default the Data Type control to the scale of the file being uploaded.
  # Done before validation because validate_data() enforces the 0-1 bound for
  # "proportion": validating first would reject count data whenever the control
  # happened to be left on "proportion", and never reach the detection.
  detected_type <- tryCatch(
    detect_data_type(
      readr::read_csv(file_info$datapath, show_col_types = FALSE)$value
    ),
    error = function(e) isolate(data_type())
  )

  if (!identical(detected_type, isolate(data_type()))) {
    updateRadioButtons(session, "data_type", selected = detected_type)
  }

  validation_results <- tryCatch(
    validate_data(file_info$datapath, data_type = detected_type),
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
        paste0(
          "All data validation checks passed! Data Type set to \"",
          if (identical(detected_type, "proportion")) "Proportion (0-1)" else "Counts",
          "\" from the scale of your values -- change it above if that's wrong."
        )
      )
    )

    rv$raw_data <- read_raw_data(file_info$datapath)
    rv$active_upload_name <- file_info$name
    rv$active_upload_datapath <- file_info$datapath

    upload_country <- country_from_upload_filename(
      filename = file_info$name,
      epizone_data = epizone_data,
      default = "Paraguay"
    )

    updateSelectizeInput(
      session,
      "country_select",
      choices = epizone_choices,
      selected = upload_country
    )

    updateDateInput(
      session,
      "forecast_date",
      value = Sys.Date()
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

# Re-validate the currently-loaded dataset against a newly selected Data
# Type. ignoreInit = TRUE: on session start data_type() first resolves to its
# default with no data loaded yet, so there's nothing to re-check.
observeEvent(data_type(), {
  req(rv$raw_data, rv$active_upload_datapath)

  clear_data_upload_messages()

  validation_results <- tryCatch(
    validate_data(rv$active_upload_datapath, data_type = data_type()),
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
    rv$valid_data <- FALSE
    return(invisible(NULL))
  }

  if (length(validation_results) == 0) {
    rv$valid_data <- TRUE
  } else {
    rv$valid_data <- FALSE
    all_errors <- unlist(validation_results, recursive = TRUE)
    type_label <- if (identical(data_type(), "proportion")) "Proportion (0-1)" else "Counts"

    insertUI(
      selector = "#error_message",
      where = "beforeEnd",
      ui = div(
        class = "alert alert-danger",
        icon("circle-exclamation", style = "margin-right:2px"),
        tags$strong(sprintf(
          "Your currently loaded data does not pass validation for Data Type \"%s\".",
          type_label
        )),
        tags$br(),
        "Switch Data Type back, or re-upload data matching the selected type, before running any models.",
        tags$br(),
        tags$br(),
        tags$strong("Validation Errors:"),
        tags$ul(lapply(all_errors, tags$li))
      )
    )
  }
}, ignoreInit = TRUE)

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
  req(rv$raw_data, input$forecast_date, input$data_to_drop, selected_data_target_group())
  plot_uploaded_time_series(
    raw_data = rv$raw_data,
    forecast_date = input$forecast_date,
    data_to_drop = input$data_to_drop,
    target_group = selected_data_target_group()
  )
})

output$uploaded_resp_season_plot <- renderPlot({
  req(rv$raw_data, input$forecast_date, input$seasonality, input$data_to_drop, selected_data_target_group())
  plot_uploaded_resp_season_series(
    raw_data = rv$raw_data,
    forecast_date = input$forecast_date,
    seasonality = input$seasonality,
    data_to_drop = input$data_to_drop,
    target_group = selected_data_target_group()
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
    enable("run_calcopycat")
    enable("run_fourcat")
    enable("run_newgbqr")
    enable("run_pargbqr")
    enable("run_starima")
    enable("run_all_default_models")

    # For INLA, also enable and update population button if col exists
    suppressWarnings({
      # Population offset only makes sense for raw counts -- a value that's
      # already a proportion shouldn't also be divided/multiplied by a
      # population, so keep the offset control off in that mode regardless
      # of whether a population column was uploaded.
      if (!is.null(rv$raw_data$population) && identical(data_type(), "count")) {
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
    disable("run_calcopycat")
    disable("run_fourcat")
    disable("run_newgbqr")
    disable("run_pargbqr")
    disable("run_starima")
    disable("run_all_default_models")
    disable("use_population_column")
    updateRadioButtons(session, "use_population_column", selected = "No")
  }
})
