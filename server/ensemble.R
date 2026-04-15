# Ensemble and outside models =================================================

## Outside model upload -------------------------------------------------------

# Show template download button only once forecasts are available
output$outside_model_template_ui <- renderUI({
  req(combined_results())
  downloadButton(
    "download_outside_model_template",
    "Download Template (.csv)",
    class = "btn-sm btn-outline-secondary mb-2"
  )
})

# Generate template from first non-ensemble model's forecast structure
output$download_outside_model_template <- downloadHandler(
  filename = function() {
    paste0("outside-model-template_", Sys.Date(), ".csv")
  },
  content = function(file) {
    base_model <- combined_results() |>
      dplyr::filter(model != "Ensemble") |>
      dplyr::pull(model) |>
      as.character() |>
      unique() |>
      dplyr::first()

    template <- combined_results() |>
      dplyr::filter(model == base_model) |>
      dplyr::mutate(model = "Outside Model", value = NA_real_) |>
      dplyr::select(model, reference_date, horizon, target_end_date,
                    target_group, output_type, output_type_id, value)

    write.csv(template, file, row.names = FALSE)
  }
)

# Validate uploaded file and append/replace in the list
observeEvent(input$outside_model_file, {
  req(combined_results())

  result <- validate_outside_model(
    file         = input$outside_model_file$datapath,
    reference_df = combined_results() |> dplyr::filter(model != "Ensemble")
  )

  rv$last_outside_model_validation <- result

  if (!is.null(result$data)) {
    model_name <- unique(result$data$model)[1]

    if (model_name %in% names(rv$outside_models)) {
      rv$last_outside_model_validation$replaced <- TRUE
    }

    rv$outside_models[[model_name]]            <- result$data
    rv$outside_model_validations[[model_name]] <- result
  }
})

# Render validation feedback panel for the most recent upload
output$outside_model_validation_ui <- renderUI({
  req(input$outside_model_file)
  result <- rv$last_outside_model_validation
  if (is.null(result)) return(NULL)

  errors   <- result$errors
  warnings <- result$warnings
  panels   <- list()

  if (length(errors) == 0) {
    model_name <- unique(result$data$model)[1]
    n_rows     <- nrow(result$data)
    replaced   <- isTRUE(result$replaced)
    action_msg <- if (replaced) {
      paste0("Updated \u201c", model_name, "\u201d with ", n_rows, " new rows.")
    } else {
      paste0(n_rows, " rows loaded. Select \u201c", model_name,
             "\u201d in the model dropdown above to include it in the ensemble.")
    }
    panels[[1]] <- div(
      style = "background:#d4edda; border:1px solid #c3e6cb; border-radius:4px; padding:10px; margin-top:8px;",
      tags$b(shiny::icon("circle-check", style = "color:#155724;"),
             " ", model_name, " \u2014 passed all checks"),
      tags$p(style = "margin:4px 0 0 0; font-size:.875em; color:#155724;", action_msg)
    )
  } else {
    panels[[length(panels) + 1]] <- div(
      style = "background:#f8d7da; border:1px solid #f5c6cb; border-radius:4px; padding:10px; margin-top:8px;",
      tags$b(shiny::icon("circle-xmark", style = "color:#721c24;"),
             paste0(" ", length(errors), " error(s) \u2014 file was not added")),
      tags$ul(
        style = "margin:6px 0 0 0; padding-left:18px; font-size:.875em; color:#721c24;",
        lapply(unname(errors), tags$li)
      )
    )
  }

  if (length(warnings) > 0) {
    panels[[length(panels) + 1]] <- div(
      style = "background:#fff3cd; border:1px solid #ffeeba; border-radius:4px; padding:10px; margin-top:6px;",
      tags$b(shiny::icon("triangle-exclamation", style = "color:#856404;"),
             paste0(" ", length(warnings), " warning(s)")),
      tags$ul(
        style = "margin:6px 0 0 0; padding-left:18px; font-size:.875em; color:#856404;",
        lapply(unname(warnings), tags$li)
      )
    )
  }

  tagList(panels)
})

# Render the persistent loaded-models list with remove control
output$outside_models_loaded_ui <- renderUI({
  loaded <- rv$outside_models
  if (length(loaded) == 0) return(NULL)

  rows <- lapply(names(loaded), function(nm) {
    n <- nrow(loaded[[nm]])
    tags$div(
      style = "display:flex; align-items:center; gap:6px; padding:3px 0; font-size:.875em;",
      shiny::icon("circle-check", style = "color:#155724;"),
      tags$span(style = "flex:1;", tags$b(nm),
                tags$span(style = "color:#6c757d;", paste0(" \u2014 ", n, " rows")))
    )
  })

  tagList(
    tags$hr(style = "margin:10px 0 8px 0;"),
    tags$p(tags$b("Loaded outside models:"), style = "margin-bottom:6px; font-size:.875em;"),
    tagList(rows),
    tags$div(
      style = "margin-top:8px; display:flex; gap:6px; align-items:center;",
      selectInput(
        "remove_om_select",
        label    = NULL,
        choices  = c("\u2014 select to remove \u2014" = "", names(loaded)),
        selected = "",
        width    = "100%"
      ),
      actionButton(
        "remove_om_btn",
        label = NULL,
        icon  = shiny::icon("trash"),
        class = "btn-sm btn-outline-danger",
        title = "Remove selected model"
      )
    )
  )
})

# Remove a loaded outside model when the trash button is clicked
observeEvent(input$remove_om_btn, {
  nm <- input$remove_om_select
  req(nchar(trimws(nm)) > 0)
  rv$outside_models[[nm]]            <- NULL
  rv$outside_model_validations[[nm]] <- NULL
})

## Ensemble model --------------------------------------------------------------

# Update selectizeInput with models that have been run
observe({
  req(combined_results())

  model_choices <- combined_results() |>
    distinct(model) |>
    filter(!model == "Ensemble") |>
    pull(model)

  if (length(model_choices) >= 2) {
    current_selection    <- isolate(input$ensemble_models)
    non_baseline_choices <- model_choices[!grepl("Baseline", model_choices, ignore.case = TRUE)]
    new_selection        <- intersect(union(current_selection, non_baseline_choices), model_choices)
    updateSelectizeInput(session, "ensemble_models",
                         choices  = model_choices,
                         selected = new_selection,
                         server   = TRUE)
  } else {
    updateSelectizeInput(session, "ensemble_models", choices = NULL, server = TRUE)
  }
})

# Enable run_ensemble button once two models have been selected
observe({
  toggleState("run_ensemble", condition = length(input$ensemble_models) >= 2)
})

observeEvent(input$run_ensemble, {
  req(rv$raw_data)
  req(length(combined_results()) >= 2)
  withProgress(message = "Ensemble", value = 0, {
    incProgress(0.1, detail = "Processing...")

    ensemble_results <- combined_results() |>
      filter(model %in% input$ensemble_models) |>
      summarize(
        value = round(median(value), 0),
        .by = c(reference_date, horizon, target_end_date,
                target_group, output_type, output_type_id)
      ) |>
      mutate(
        reference_date  = as.Date(reference_date),
        target_end_date = as.Date(target_end_date)
      )

    rv$ensemble <- ensemble_results |> mutate(model = "Ensemble", .before = 1)

    incProgress(0.8, detail = "Plotting results...")

    ensemble_plots <- target_groups() |>
      map(plot_forecasts,
          forecast_df = ensemble_results,
          data_df     = plot_data(),
          seasonality = input$seasonality)

    ensemble_grid <- plot_grid(plotlist = ensemble_plots, ncol = 1)
    ensemble_grid <- ggdraw(add_sub(
      ensemble_grid,
      "Forecast with the Ensemble model.",
      x = 1, hjust = 1, size = 11, color = "gray20"
    ))

    ensemble_plot_path <- paste0(
      "figures/plot-ensemble_",
      get_reference_date_label(ensemble_results_formatted),
      ".png"
    )

    output$ensemble_plots <- renderPlot({
      ggsave(ensemble_plot_path, width = 8, height = 8, dpi = 300, bg = "white")
      enable("ensemble_plot_download")
      ensemble_grid
    })

    incProgress(1)
  })

  output$ensemble_plot_download <- downloadHandler(
    filename = function() ensemble_plot_path,
    content  = function(file) file.copy(ensemble_plot_path, file, overwrite = TRUE)
  )
})
