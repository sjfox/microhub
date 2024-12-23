test_that("wrangle_inla works", {
  df <- readRDS(test_path("data", "example-df.RDS"))
  inla_input <- readRDS(test_path("data", "inla-input.RDS"))

  expect_equal(
    wrangle_inla(
      dataframe = df,
      forecast_date = "2024-07-13",
      data_to_drop = "2 week",
      forecast_horizons = 3
    ),
    inla_input
  )
})

test_that("fit_process_inla works", {
  inla_input <- readRDS(test_path("data", "inla-input.RDS"))
  inla_results <- readRDS(test_path("data", "inla-results.RDS"))

  expect_equal(
    fit_process_inla(
      fit_df = inla_input,
      forecast_date = "2024-07-13",
      ar_order = 1,
      rw_order = 2,
      seasonal_smoothness = "default",
      forecast_uncertainty_parameter = "default"
    ),
    inla_results
  )
})
