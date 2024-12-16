test_that("wrangle_sirsea works", {
  df <- readRDS(test_path("data", "example-df.RDS"))
  stan_input <- readRDS(test_path("data", "stan-input.RDS"))

  expect_equal(
    wrangle_sirsea(
      dataframe = df,
      forecast_date = "2024-07-13",
      data_to_drop = "2 week",
      forecast_horizons = 1
    ),
    stan_input
  )
})

test_that("fit_process_sirsea works", {
  df <- readRDS(test_path("data", "example-df.RDS"))
  stan_input <- readRDS(test_path("data", "stan-input.RDS"))
  sirsea_results <- readRDS(test_path("data", "sirsea-results.RDS"))

  expect_equal(
    fit_process_sirsea(
      dataframe = stan_input$subset_data,
      stan_dat = stan_input$stan_dat,
      forecast_date = "2024-07-13",
      data_to_drop = "2 week",
      cmdstan_path = "C:/Users/jryan/.cmdstan/cmdstan-2.36.0"
    ),
    sirsea_results
  )
})
