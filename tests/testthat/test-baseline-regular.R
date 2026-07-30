suppressPackageStartupMessages(library(simplets))
source(test_path("../../R/baseline-regular.R"))

test_that("flat baseline forecasts are nonnegative on the original scale", {
  df <- tibble(
    target_group = "Overall",
    value = c(0, 1, 0, 2, 0, 1, 0, 1, 0, 1)
  )

  regular <- fit_process_baseline_flat(
    df,
    weeks_ahead = 3,
    quantiles_needed = c(0.01, 0.5, 0.99)
  )
  optimal <- fit_process_baseline_flat(
    df,
    weeks_ahead = 3,
    quantiles_needed = c(0.01, 0.5, 0.99),
    window_size = 8
  )

  expect_true(all(regular$value >= 0))
  expect_true(all(optimal$value >= 0))
})
