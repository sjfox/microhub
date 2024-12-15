df <- read.csv("data/fake-target-sari-admissions.csv")

source("R/inla.R")

fitted_data <- wrangle_inla(
  dataframe = df,
  forecast_date = "2023-07-13",
  data_to_drop = "2 week",
  forecast_horizons = 3
)

forecast_results <- fit_process_inla(
  fit_df = fitted_data,
  forecast_date = as.Date("2023-07-13"),
  ar_order = 1,
  rw_order = 2,
  seasonal_smoothness = "default",
  forecast_uncertainty_parameter = "default"
)

testthat::test_that("wrangle_inla works", {
  testthat::expect_equal(
    wrangle_inla(
      dataframe = df,
      forecast_date = "2023-07-13",
      data_to_drop = "2 week",
      forecast_horizons = 3
    ),
    fitted_data
  )
})

testthat::test_that("fit_process_inla works", {
  testthat::expect_equal(
    fit_process_inla(
      fit_df = fitted_data,
      forecast_date = as.Date("2023-07-13"),
      ar_order = 1,
      rw_order = 2,
      seasonal_smoothness = "default",
      forecast_uncertainty_parameter = "default"
    ),
    forecast_results,
    tolerance = 0.01
  )
})
