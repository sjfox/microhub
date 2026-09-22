# Copycat model ===============================================================

observeEvent(input$run_copycat, {
  run_copycat_model()
})

# Theoretical ceiling for "Max Historical Matches", recomputed whenever the
# settings it depends on change (uploaded data, respiratory week range,
# whether groups share trajectories). This only updates the field's `max`
# attribute -- never its `value` -- so a blank field (meaning "use all") and
# a user-chosen cap are both left alone; only the ceiling shown/enforced
# around them moves.
copycat_matches_ceiling <- reactive({
  req(fcast_data(), input$seasonality)

  share_groups <- input_or_default(input$copycat_share_groups, "shared") == "shared"
  resp_week_range <- input_or_default(input$resp_week_range, 2)

  tryCatch(
    copycat_max_possible_matches(
      df = fcast_data(),
      seasonality = input$seasonality,
      resp_week_range = resp_week_range,
      share_groups = isTRUE(share_groups)
    ),
    error = function(e) NA_integer_
  )
})

observe({
  ceiling_val <- copycat_matches_ceiling()
  if (is.na(ceiling_val) || ceiling_val < 1) ceiling_val <- 1L
  updateNumericInput(session, "copycat_max_matches", max = ceiling_val)
})

output$copycat_max_matches_hint <- renderText({
  ceiling_val <- copycat_matches_ceiling()

  if (is.na(ceiling_val)) {
    return("Upload data to see how many historical matches are available.")
  }

  paste0(
    "Up to ", ceiling_val, " historical matches are available with the current settings. ",
    "Leave this blank to use all of them."
  )
})
