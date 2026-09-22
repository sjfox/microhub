test_that("STArima returns quantile forecasts for each target group", {
  set.seed(123)
  dates <- seq.Date(as.Date("2020-01-04"), by = "week", length.out = 120)
  example <- tidyr::expand_grid(
    date = dates,
    target_group = c("Overall", "Ages 0-4")
  ) |>
    dplyr::mutate(
      week = as.integer(format(date, "%U")),
      value = pmax(
        0,
        10 + 5 * sin(2 * pi * week / 52) +
          dplyr::if_else(target_group == "Overall", 4, 0) +
          stats::rnorm(dplyr::n(), sd = 1)
      )
    ) |>
    dplyr::select(date, target_group, value)

  result <- fit_process_starima(
    clean_data = example,
    fcast_horizon = 4,
    quantiles_needed = c(0.025, 0.5, 0.975),
    n_sim = 100
  )

  expect_named(
    result,
    c("horizon", "target_group", "output_type", "output_type_id", "value")
  )
  expect_equal(sort(unique(result$horizon)), 1:4)
  expect_equal(sort(unique(result$target_group)), c("Ages 0-4", "Overall"))
  expect_equal(sort(unique(result$output_type_id)), c("0.025", "0.5", "0.975"))
  expect_true(all(result$value >= 0))
})

test_that("STArima falls back to ARIMA when fewer than 104 weeks are available", {
  set.seed(456)
  dates <- seq.Date(as.Date("2023-01-07"), by = "week", length.out = 40)
  example <- tibble::tibble(
    date = dates,
    target_group = "Overall",
    value = pmax(0, 8 + stats::rnorm(length(dates), sd = 1))
  )

  result <- fit_process_starima(
    clean_data = example,
    fcast_horizon = 4,
    quantiles_needed = c(0.025, 0.5, 0.975),
    n_sim = 100
  )

  expect_equal(sort(unique(result$horizon)), 1:4)
  expect_equal(unique(result$target_group), "Overall")
  expect_equal(sort(unique(result$output_type_id)), c("0.025", "0.5", "0.975"))
  expect_true(all(result$value >= 0))
})

test_that("STArima returns flat forecasts for flat short histories", {
  dates <- seq.Date(as.Date("2024-01-06"), by = "week", length.out = 20)
  example <- tibble::tibble(
    date = dates,
    target_group = "Overall",
    value = 5
  )

  result <- fit_process_starima(
    clean_data = example,
    fcast_horizon = 4,
    quantiles_needed = c(0.025, 0.5, 0.975),
    n_sim = 100
  )

  expect_equal(unique(result$value), 5)
})
