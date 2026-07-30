source(test_path("../../R/copycat.R"))
source(test_path("../../R/CalCopycat.R"))

test_that("Copycat can forecast during the first weeks of a respiratory season", {
  set.seed(123)

  dates <- seq(as.Date("2021-01-02"), as.Date("2024-01-13"), by = "week")
  df <- tibble(
    date = dates,
    target_group = "Overall",
    value = round(25 + 10 * sin(seq_along(dates) / 8) + seq_along(dates) %% 7)
  )

  forecasts <- fit_process_copycat(
    df = df,
    fcast_horizon = 2,
    quantiles_needed = c(0.5),
    seasonality = "E",
    recent_weeks_touse = 5,
    nsamps = 50,
    resp_week_range = 2,
    share_groups = TRUE
  )

  expect_equal(forecasts$horizon, c(1L, 2L))
  expect_equal(forecasts$output_type_id, c("0.5", "0.5"))
  expect_true(all(is.finite(forecasts$value)))
})

test_that("Copycat no-shift matcher uses respiratory season week", {
  set.seed(123)

  curr_data <- tibble(
    resp_season_week = c(-1L, 0L, 1L, 2L),
    value = c(10, 12, 14, 16),
    curr_weekly_change = c(log(12 / 10), log(14 / 12), log(16 / 14), NA_real_)
  )

  db <- tibble(
    target_group = "Overall",
    resp_season_year = 2023L,
    resp_season_week = c(-1L, 0L, 1L, 2L, 3L),
    pred = c(log(12 / 10), log(14 / 12), log(16 / 14), log(18 / 16), log(20 / 18)),
    pred_se = rep(0.01, 5)
  )

  forecasts <- copycat_fxn(
    curr_data = curr_data,
    forecast_horizon = 2,
    recent_weeks_touse = 5,
    nsamps = 10,
    resp_week_range = 0,
    db = db
  )

  expect_equal(sort(unique(forecasts$resp_season_week)), c(3L, 4L))
  expect_true(all(is.finite(forecasts$forecast)))
})

test_that("CalCopycat no-shift matcher accepts prepended respiratory weeks", {
  set.seed(123)

  curr_data <- tibble(
    resp_season_week = c(-1L, 0L, 1L, 2L),
    value = c(10, 12, 14, 16),
    curr_weekly_change = c(log(12 / 10), log(14 / 12), log(16 / 14), NA_real_)
  )

  db <- tibble(
    target_group = "Overall",
    resp_season_year = 2023L,
    resp_season_week = c(-1L, 0L, 1L, 2L, 3L),
    pred = c(log(12 / 10), log(14 / 12), log(16 / 14), log(18 / 16), log(20 / 18)),
    pred_se = rep(0.01, 5)
  )

  forecasts <- calcopycat_fxn(
    curr_data = curr_data,
    forecast_horizon = 2,
    recent_weeks_touse = 5,
    nsamps = 10,
    resp_week_range = 0,
    db = db
  )

  expect_equal(sort(unique(forecasts$resp_season_week)), c(3L, 4L))
  expect_true(all(is.finite(forecasts$forecast)))
})
