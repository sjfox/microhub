# Download tab ================================================================

# Combine all model results into one data frame
combined_results <- reactive({
  req(rv$raw_data)
  req(input$run_baseline_regular > 0 |
        input$run_baseline_seasonal > 0 |
        input$run_baseline_opt > 0 |
        input$run_inla > 0 |
        input$run_copycat > 0 |
        input$run_fourcat > 0 |
        input$run_gbqr > 0)

  bind_rows(
    rv$baseline_regular,
    rv$baseline_seasonal,
    rv$baseline_opt,
    rv$inla,
    rv$copycat,
    rv$fourcat,
    rv$gbqr,
    if (length(rv$outside_models) > 0) bind_rows(rv$outside_models) else NULL,
    rv$ensemble
  ) |>
    mutate(
      model           = factor(model),
      reference_date  = format(reference_date, "%Y-%m-%d"),
      horizon         = round(horizon, 0),
      target_end_date = format(target_end_date, "%Y-%m-%d"),
      value           = round(value, 0)
    )
})

# Enable download button when results are available
observe({
  if (!is.null(combined_results()) && nrow(combined_results()) > 0) {
    enable("download_results")
  } else {
    disable("download_results")
  }
})

# Results preview table
output$results_preview <- renderDT({
  req(nrow(combined_results()) > 0)
  datatable(
    combined_results(),
    rownames  = FALSE,
    filter    = "top",
    selection = "none",
    options   = list(columnDefs = list(list(targets = 0, width = "150px")))
  )
})

# Download results CSV
output$download_results <- downloadHandler(
  filename = function() {
    paste0("microhub-model-output_", Sys.Date(), ".csv")
  },
  content = function(filename) {
    write.csv(x = combined_results(), file = filename, row.names = FALSE)
  }
)
