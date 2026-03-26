# Modal observers =============================================================
# All help-icon modals are consolidated here.
# Content is loaded from www/content/modal-*.md

# Data Upload & Settings tab --------------------------------------------------

observeEvent(input$modal_template, {
  show_modal(title = "Target Data", id = "modal-template", md = "modal-template")
})

observeEvent(input$modal_forecast_date, {
  show_modal(title = "Forecast Date", id = "modal-forecast-date", md = "modal-forecast-date")
})

observeEvent(input$modal_data_drop, {
  show_modal(title = "Data to Drop", id = "modal-data-drop", md = "modal-data-drop")
})

observeEvent(input$modal_seasonality, {
  show_modal(title = "Seasonality", id = "modal-seasonality", md = "modal-seasonality")
})

observeEvent(input$modal_forecast_horizon, {
  show_modal(title = "Forecast Horizon (Weeks)", id = "modal-forecast-horizon", md = "modal-forecast-horizon")
})

# INFLAenza tab ---------------------------------------------------------------

observe({
  onclick("inla_methodology", {
    show_modal(title = "INFLAenza Methodology", id = "modal-inflaenza", md = "modal-inflaenza")
  })
})

observeEvent(input$modal_ar, {
  show_modal(title = "Order of Autoregression", id = "modal-ar", md = "modal-ar")
})

observeEvent(input$modal_rw, {
  show_modal(title = "Order of Random Walk", id = "modal-rw", md = "modal-rw")
})

observeEvent(input$modal_seasonal_smoothness, {
  show_modal(title = "Seasonal Smoothness", id = "modal-seasonal-smoothness", md = "modal-seasonal-smoothness")
})

observeEvent(input$modal_forecast_uncertainty, {
  show_modal(title = "Forecast Uncertainty", id = "modal-forecast-uncertainty", md = "modal-forecast-uncertainty")
})

observeEvent(input$modal_population, {
  show_modal(title = "Population offset", id = "modal-population", md = "modal-population")
})

# Copycat tab -----------------------------------------------------------------

observe({
  onclick("copycat_methodology", {
    show_modal(title = "Copycat Methodology", id = "modal-copycat", md = "modal-copycat")
  })
})

observeEvent(input$modal_recent_weeks, {
  show_modal(title = "Recent Weeks to Use", id = "modal-recent-weeks", md = "modal-recent-weeks")
})

observeEvent(input$modal_resp_week_range, {
  show_modal(title = "Respiratory Week Range", id = "modal-resp-week-range", md = "modal-resp-week-range")
})
