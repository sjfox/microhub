# Download tab ================================================================

# Combine all model results into one data frame
combined_results <- reactive({
  req(rv$raw_data)
  req(input$run_baseline_regular > 0 |
        input$run_baseline_seasonal > 0 |
        input$run_baseline_opt > 0 |
        input$run_inla > 0 |
        input$run_copycat > 0 |
        input$run_calcopycat > 0 |
        input$run_fourcat > 0 |
        input$run_gbqr > 0)

  combined <- bind_rows(
    rv$baseline_regular,
    rv$baseline_seasonal,
    rv$baseline_opt,
    rv$inla,
    rv$copycat,
    rv$calcopycat,
    rv$fourcat,
    rv$gbqr,
    if (length(rv$outside_models) > 0) bind_rows(rv$outside_models) else NULL,
    rv$ensemble
  )

  req(nrow(combined) > 0)

  combined |>
    mutate(
      model           = factor(model),
      reference_date  = format(reference_date, "%Y-%m-%d"),
      horizon         = round(horizon, 0),
      target_end_date = format(target_end_date, "%Y-%m-%d"),
      value           = round(value, 0)
    )
})

available_download_models <- reactive({
  req(nrow(combined_results()) > 0)
  combined_results() |>
    distinct(model) |>
    pull(model) |>
    as.character()
})

# Keep the download model selector in sync with available forecasts
observe({
  choices <- available_download_models()
  current_selection <- isolate(input$download_models)

  if (is.null(current_selection) || length(current_selection) == 0) {
    selected <- choices
  } else {
    selected <- intersect(current_selection, choices)
    newly_available <- setdiff(choices, current_selection)
    selected <- c(selected, newly_available)
  }

  updateSelectizeInput(
    session,
    "download_models",
    choices = choices,
    selected = selected,
    server = TRUE
  )
})

selected_download_results <- reactive({
  req(nrow(combined_results()) > 0)
  req(length(input$download_models) > 0)

  combined_results() |>
    filter(as.character(model) %in% input$download_models)
})

# Enable download button when results are available
observe({
  if (!is.null(selected_download_results()) && nrow(selected_download_results()) > 0) {
    enable("download_results")
  } else {
    disable("download_results")
  }
})

# Results preview table
output$results_preview <- renderDT({
  req(nrow(selected_download_results()) > 0)
  datatable(
    selected_download_results(),
    rownames  = FALSE,
    filter    = "top",
    selection = "none",
    options   = list(columnDefs = list(list(targets = 0, width = "150px")))
  )
})

# Download results CSV
output$download_results <- downloadHandler(
  filename = function() {
    paste0(
      "microhub-output_",
      get_reference_date_label(selected_download_results()),
      ".csv"
    )
  },
  content = function(filename) {
    write.csv(x = selected_download_results(), file = filename, row.names = FALSE)
  }
)
