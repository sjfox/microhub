test_that("plot function works with INLA", {
  df <- readRDS(test_path("data", "example-df.RDS"))

  state_order <- c("Pediatric", "Adult", "Overall")

  inla_results <- readRDS(test_path("data", "inla-results.RDS"))

  inla_plot_df <- prepare_historic_data(
    data = df,
    cleaned_forecasts_quantiles = inla_results,
    forecast_date = "2024-07-13"
  )

  inla_plots <- state_order |>
    map(
      plot_state_forecast_try,
      forecast_date = "2024-07-13",
      curr_season_data = inla_plot_df$curr_season_data,
      forecast_df = inla_plot_df$forecast_df,
      historic_data = inla_plot_df$historic_data
    )

  inla_plot_grid <- plot_grid(plotlist = inla_plots)

  vdiffr::expect_doppelganger(
    title = "inla_plot_grid",
    fig = inla_plot_grid
  )
})

testthat("plot function works with SIRsea", {
  df <- readRDS(test_path("data", "example-df.RDS"))

  state_order <- c("Pediatric", "Adult", "Overall")

  sirsea_results <- readRDS(test_path("data", "sirsea-results.RDS"))

  sirsea_plot_df <- prepare_historic_data(
    data = df,
    cleaned_forecasts_quantiles = sirsea_results,
    forecast_date = "2024-07-13"
  )

  sirsea_plots <- state_order |>
    map(
      plot_state_forecast_try,
      forecast_date = "2024-07-13",
      curr_season_data = sirsea_plot_df$curr_season_data,
      forecast_df = sirsea_plot_df$forecast_df,
      historic_data = sirsea_plot_df$historic_data
    )

  sirsea_plot_grid <- plot_grid(plotlist = sirsea_plots)

  vdiffr::expect_doppelganger(
    title = "sirsea_plot_grid",
    fig = sirsea_plot_grid
  )
})
