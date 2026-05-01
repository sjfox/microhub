# Baseline models =============================================================

observeEvent(input$run_baseline_regular, {
  run_baseline_regular_model()
})

observeEvent(input$run_baseline_seasonal, {
  run_baseline_seasonal_model()
})

observeEvent(input$run_baseline_opt, {
  run_baseline_opt_model()
})
