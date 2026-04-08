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

# Baseline tab ----------------------------------------------------------------

observeEvent(input$modal_baseline_regular_methodology, {
  show_modal(
    title = "Regular Baseline Methodology",
    id = "modal-baseline-regular-methodology",
    md = "baseline-regular"
  )
})

observeEvent(input$modal_baseline_seasonal_methodology, {
  show_modal(
    title = "Seasonal Baseline Methodology",
    id = "modal-baseline-seasonal-methodology",
    md = "baseline-seasonal"
  )
})

observeEvent(input$modal_baseline_opt_methodology, {
  show_modal(
    title = "Opt Baseline Methodology",
    id = "modal-baseline-opt-methodology",
    md = "baseline-opt"
  )
})

# INFLAenza tab ---------------------------------------------------------------

observeEvent(input$modal_inla_methodology, {
  show_modal(title = "INFLAenza Methodology", id = "modal-inflaenza", md = "inflaenza")
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

# GBQR tab -------------------------------------------------------------------

observeEvent(input$modal_gbqr_methodology, {
  show_modal(
    title = "GBQR Methodology",
    id    = "modal-gbqr-methodology",
    md    = "gbqr"
  )
})

observeEvent(input$modal_ensemble_methodology, {
  show_modal(
    title = "Ensemble Methodology",
    id = "modal-ensemble-methodology",
    md = "ensemble"
  )
})

observeEvent(input$modal_gbqr_model_type, {
  show_modal(
    title = "Model Fitting",
    id    = "modal-gbqr-model-type",
    md    = "modal-gbqr-model-type"
  )
})

# Copycat tab -----------------------------------------------------------------

observeEvent(input$modal_copycat_methodology, {
  show_modal(title = "Copycat Methodology", id = "modal-copycat", md = "copycat")
})

observeEvent(input$modal_copycat_cal_methodology, {
  show_modal(
    title = "Copycat (Calibrated) Methodology",
    id = "modal-copycat-cal",
    md = "copycat-cal"
  )
})

observeEvent(input$modal_recent_weeks, {
  show_modal(title = "Recent Weeks to Use", id = "modal-recent-weeks", md = "modal-recent-weeks")
})

observeEvent(input$modal_resp_week_range, {
  show_modal(title = "Respiratory Week Range", id = "modal-resp-week-range", md = "modal-resp-week-range")
})

observeEvent(input$modal_copycat_share_groups, {
  show_modal(title = "Group Trajectories", id = "modal-copycat-share-groups", md = "modal-copycat-share-groups")
})

observeEvent(input$modal_ref_week_window, {
  show_modal(title = "Reference Week Window", id = "modal-ref-week-window", md = "modal-ref-week-window")
})

observeEvent(input$modal_nsamps_cal, {
  show_modal(title = "Calibration Samples", id = "modal-nsamps-cal", md = "modal-nsamps-cal")
})
