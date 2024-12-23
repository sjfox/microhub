test_that("wrangle_copycat works", {
  df <- readRDS(test_path("data", "example-df.RDS"))
  copycat_input <- readRDS(test_path("data", "copycat-input.RDS"))

  expect_equal(
    wrangle_copycat(
      dataframe = df,
      forecast_date = "2024-07-13"
    ),
    copycat_input
  )
})

test_that("fit_process_copycat works", {
  copycat_input <- readRDS(test_path("data", "copycat-input.RDS"))
  copycat_results <- readRDS(test_path("data", "copycat-results.RDS"))

  expect_equal(
    fit_process_copycat(
      fit_df = copycat_input$recent_sari,
      historic_df = copycat_input$historic_sari,
      forecast_date = "2024-07-13",
      data_to_drop = "1 week",
      forecast_horizon = 4,
      recent_weeks_touse = 10,
      nsamps = 1000,
      resp_week_range = 0
    ),
    copycat_results
  )
})
