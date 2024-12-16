test_that("wrangle_inla works", {
  df <- readRDS(test_path("data", "example-df.RDS"))
  fitted_inla <- readRDS(test_path("data", "fitted-inla.RDS"))

  expect_equal(
    wrangle_inla(
      dataframe = df,
      forecast_date = "2024-07-13",
      data_to_drop = "2 week",
      forecast_horizons = 3
    ),
    fitted_inla
  )
})

test_that("fit_process_inla works", {
  fitted_inla <- readRDS(test_path("data", "fitted-inla.RDS"))
  inla_results <- readRDS(test_path("data", "inla-results.RDS"))

  expect_equal(
    fit_process_inla(
      fit_df = fitted_inla,
      forecast_date = as.Date("2024-07-13"),
      ar_order = 1,
      rw_order = 2,
      seasonal_smoothness = "default",
      forecast_uncertainty_parameter = "default"
    ),
    inla_results
  )
})
