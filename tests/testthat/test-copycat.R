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

test_that("fit_process_copycat forwards nsamps to copycat_fxn", {
  dates <- seq(as.Date("2021-01-02"), as.Date("2024-01-13"), by = "week")
  df <- tibble(
    date = dates,
    target_group = "Overall",
    value = round(25 + 10 * sin(seq_along(dates) / 8) + seq_along(dates) %% 7)
  )

  captured_nsamps <- NULL
  original_copycat_fxn <- copycat_fxn
  copycat_fxn <<- function(curr_data,
                           forecast_horizon = 4,
                           recent_weeks_touse = 100,
                           nsamps = 1000,
                           resp_week_range = 2,
                           db,
                           ...) {
    captured_nsamps <<- nsamps
    tibble(
      id = rep(seq_len(nsamps), each = forecast_horizon),
      resp_season_week = rep(seq_len(forecast_horizon) + max(curr_data$resp_season_week), times = nsamps),
      forecast = 1
    )
  }
  on.exit(copycat_fxn <<- original_copycat_fxn, add = TRUE)

  fit_process_copycat(
    df = df,
    fcast_horizon = 2,
    quantiles_needed = c(0.5),
    seasonality = "E",
    recent_weeks_touse = 5,
    nsamps = 7,
    resp_week_range = 2,
    share_groups = TRUE
  )

  expect_equal(captured_nsamps, 7)
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

test_that("copycat_fxn skips Poisson noise when add_poisson_noise = FALSE", {
  set.seed(1)

  curr_data <- tibble(
    resp_season_week = c(-1L, 0L, 1L, 2L),
    value = c(10, 10.5, 11, 11.5),
    curr_weekly_change = c(log(10.5 / 10), log(11 / 10.5), log(11.5 / 11), NA_real_)
  )

  db <- tibble(
    target_group = "Overall",
    resp_season_year = 2023L,
    resp_season_week = c(-1L, 0L, 1L, 2L, 3L),
    pred = log(1.05),
    pred_se = 0
  )

  no_poisson <- copycat_fxn(
    curr_data = curr_data,
    forecast_horizon = 2,
    recent_weeks_touse = 5,
    nsamps = 5,
    resp_week_range = 0,
    db = db,
    add_poisson_noise = FALSE
  )

  expected <- 11.5 * cumprod(rep(1.05, 2))
  expect_equal(unique(round(no_poisson$forecast, 6)), round(expected, 6))
  expect_true(any(no_poisson$forecast != round(no_poisson$forecast)))

  with_poisson <- copycat_fxn(
    curr_data = curr_data,
    forecast_horizon = 2,
    recent_weeks_touse = 5,
    nsamps = 5,
    resp_week_range = 0,
    db = db,
    add_poisson_noise = TRUE
  )

  expect_true(all(with_poisson$forecast == round(with_poisson$forecast)))
})

test_that("copycat_fxn's weight_exponent concentrates resampling on the closer match", {
  set.seed(42)

  curr_data <- tibble(
    resp_season_week = c(-2L, -1L, 0L, 1L),
    value = c(10, 11, 12.1, 13.31),
    curr_weekly_change = c(log(11 / 10), log(12.1 / 11), log(13.31 / 12.1), NA_real_)
  )

  # Season "close" matches curr_data's ~10% weekly growth almost exactly;
  # season "far" grows much faster and should match poorly.
  db <- bind_rows(
    tibble(
      target_group = "Overall",
      resp_season_year = 2020L,
      resp_season_week = c(-2L, -1L, 0L, 1L, 2L),
      pred = log(1.10),
      pred_se = 0.001
    ),
    tibble(
      target_group = "Overall",
      resp_season_year = 2021L,
      resp_season_week = c(-2L, -1L, 0L, 1L, 2L),
      pred = log(1.60),
      pred_se = 0.001
    )
  )

  share_of_close_match <- function(weight_exponent) {
    result <- copycat_fxn(
      curr_data = curr_data,
      forecast_horizon = 1,
      recent_weeks_touse = 5,
      nsamps = 2000,
      resp_week_range = 0,
      db = db,
      weight_exponent = weight_exponent,
      add_poisson_noise = FALSE
    )
    mean(result$forecast < 18) # season 2020's forecast (~14.6) is well below 2021's (~21.3)
  }

  low_exponent_share <- share_of_close_match(1)
  high_exponent_share <- share_of_close_match(3)

  expect_true(high_exponent_share >= low_exponent_share)
  expect_true(high_exponent_share > 0.9)
})

test_that("fit_process_copycat runs with points_per_knot at the extremes of its supported range", {
  dates <- seq(as.Date("2021-01-02"), as.Date("2024-01-13"), by = "week")
  df <- tibble(
    date = dates,
    target_group = "Overall",
    value = round(25 + 10 * sin(seq_along(dates) / 8) + seq_along(dates) %% 7)
  )

  for (knot_setting in c(3, 6)) {
    forecasts <- fit_process_copycat(
      df = df,
      fcast_horizon = 2,
      quantiles_needed = c(0.5),
      seasonality = "E",
      recent_weeks_touse = 5,
      nsamps = 50,
      resp_week_range = 2,
      share_groups = TRUE,
      points_per_knot = knot_setting
    )

    expect_true(all(is.finite(forecasts$value)))
  }
})


test_that("copycat_fxn's max_matches restricts resampling to the closest-matching seasons", {
  set.seed(7)

  curr_data <- tibble(
    resp_season_week = c(-2L, -1L, 0L, 1L),
    value = c(10, 11, 12.1, 13.31),
    curr_weekly_change = c(log(11 / 10), log(12.1 / 11), log(13.31 / 12.1), NA_real_)
  )

  # Three synthetic seasons at increasing distance from curr_data's ~10% growth.
  db <- bind_rows(
    tibble(target_group = "Overall", resp_season_year = 2019L,
           resp_season_week = c(-2L, -1L, 0L, 1L, 2L), pred = log(1.10), pred_se = 0),
    tibble(target_group = "Overall", resp_season_year = 2020L,
           resp_season_week = c(-2L, -1L, 0L, 1L, 2L), pred = log(1.30), pred_se = 0),
    tibble(target_group = "Overall", resp_season_year = 2021L,
           resp_season_week = c(-2L, -1L, 0L, 1L, 2L), pred = log(1.60), pred_se = 0)
  )

  closest <- 13.31 * 1.10
  middle  <- 13.31 * 1.30
  farthest <- 13.31 * 1.60

  run <- function(max_matches, weight_exponent = 2) {
    copycat_fxn(
      curr_data = curr_data,
      forecast_horizon = 1,
      recent_weeks_touse = 5,
      nsamps = 500,
      resp_week_range = 0,
      db = db,
      weight_exponent = weight_exponent,
      add_poisson_noise = FALSE,
      max_matches = max_matches
    )
  }

  # max_matches = 1 keeps only the single closest-matching season.
  only_best <- run(max_matches = 1)
  expect_equal(unique(round(only_best$forecast, 4)), round(closest, 4))

  # max_matches = 2 keeps the two closest seasons, excluding the farthest.
  top_two <- run(max_matches = 2)
  expect_true(all(top_two$forecast < farthest - 1))
  expect_true(any(round(top_two$forecast, 4) == round(middle, 4)))

  # Default (Inf) keeps every eligible season, including the farthest.
  all_matches <- run(max_matches = Inf, weight_exponent = 1)
  expect_true(any(round(all_matches$forecast, 4) == round(farthest, 4)))
})

test_that("fit_process_copycat forwards max_matches to copycat_fxn", {
  dates <- seq(as.Date("2021-01-02"), as.Date("2024-01-13"), by = "week")
  df <- tibble(
    date = dates,
    target_group = "Overall",
    value = round(25 + 10 * sin(seq_along(dates) / 8) + seq_along(dates) %% 7)
  )

  captured_max_matches <- NULL
  original_copycat_fxn <- copycat_fxn
  copycat_fxn <<- function(curr_data,
                           forecast_horizon = 4,
                           recent_weeks_touse = 100,
                           nsamps = 1000,
                           resp_week_range = 2,
                           db,
                           max_matches = Inf,
                           ...) {
    captured_max_matches <<- max_matches
    tibble(
      id = rep(seq_len(nsamps), each = forecast_horizon),
      resp_season_week = rep(seq_len(forecast_horizon) + max(curr_data$resp_season_week), times = nsamps),
      forecast = 1
    )
  }
  on.exit(copycat_fxn <<- original_copycat_fxn, add = TRUE)

  fit_process_copycat(
    df = df,
    fcast_horizon = 2,
    quantiles_needed = c(0.5),
    seasonality = "E",
    recent_weeks_touse = 5,
    nsamps = 7,
    resp_week_range = 2,
    share_groups = TRUE,
    max_matches = 15
  )

  expect_equal(captured_max_matches, 15)

  fit_process_copycat(
    df = df,
    fcast_horizon = 2,
    quantiles_needed = c(0.5),
    seasonality = "E",
    recent_weeks_touse = 5,
    nsamps = 7,
    resp_week_range = 2,
    share_groups = TRUE
  )

  expect_equal(captured_max_matches, Inf)
})

test_that("copycat_max_possible_matches computes the theoretical ceiling from the other settings", {
  # Three full historical seasons (2021-2023, >= 50 weeks each) plus a
  # partial, too-short current season (2024) that must be excluded.
  dates <- seq(as.Date("2021-01-03"), as.Date("2024-02-04"), by = "week")
  df <- tibble(
    date = dates,
    target_group = "Overall",
    value = 1
  )

  # resp_week_range = 0 -> one shift per series; = 2 -> five shifts per series.
  expect_equal(
    copycat_max_possible_matches(df, seasonality = "E", resp_week_range = 0, share_groups = TRUE),
    3L
  )
  expect_equal(
    copycat_max_possible_matches(df, seasonality = "E", resp_week_range = 2, share_groups = TRUE),
    3L * 5L
  )

  # Two target groups: Pediatric's 2023 season is truncated to 22 weeks. The
  # year_too_short rule is shared across groups (mirroring get_full_year_df()),
  # so a too-short Pediatric 2023 knocks 2023 out for Overall too, leaving only
  # 2021 and 2022 eligible for each group (4 series total when shared; 2 when
  # matched per group, since both groups have 2 eligible seasons each).
  df_two_groups <- bind_rows(
    df,
    df |> mutate(target_group = "Pediatric") |> filter(date <= as.Date("2023-06-01"))
  )

  expect_equal(
    copycat_max_possible_matches(df_two_groups, seasonality = "E", resp_week_range = 0, share_groups = TRUE),
    4L
  )
  expect_equal(
    copycat_max_possible_matches(df_two_groups, seasonality = "E", resp_week_range = 0, share_groups = FALSE),
    2L
  )
})


# CalCopycat ====================================================================
#
# calcopycat_fxn() operates on already-prepared (date, target_group, value,
# growth) data -- this helper mirrors the prep step fit_process_calcopycat()
# does internally (log week-over-week growth), except it does NOT add the +1
# count shift, so tests below can use clean, round expected values the way
# the original copycat_fxn tests do (those also test the low-level matcher
# directly, unshifted). There's no epiweek/week-of-year column anymore --
# candidates are found by exact 52-week-multiple calendar arithmetic instead.
#
# Several tests below compare forecasts against a target with a tolerance
# rather than exact equality: each sampled trajectory now gets its own random
# growth-rate perturbation (see R/CalCopycat.R's header comment, point 4), so
# even a single "perfect" historical match no longer produces a bit-exact
# forecast -- it produces a forecast tightly clustered around the expected
# value instead.

add_growth_cols <- function(df) {
  df |>
    group_by(target_group) |>
    arrange(date, .by_group = TRUE) |>
    mutate(growth = log(lead(value) / value)) |>
    ungroup()
}

test_that("fit_process_calcopycat produces finite quantile forecasts across several years of data", {
  set.seed(123)
  dates <- seq(as.Date("2019-01-06"), as.Date("2024-01-14"), by = "week")
  df <- tibble(
    date = dates,
    target_group = "Overall",
    value = round(30 + 20 * sin(seq_along(dates) / 8.6) + (seq_along(dates) %% 5))
  )

  forecasts <- fit_process_calcopycat(
    df = df,
    fcast_horizon = 3,
    quantiles_needed = c(0.1, 0.5, 0.9),
    recent_weeks_touse = 8,
    nsamps = 100,
    resp_week_range = 2,
    share_groups = TRUE
  )

  expect_equal(sort(unique(forecasts$horizon)), 1:3)
  expect_true(all(is.finite(forecasts$value)))
  expect_true(all(forecasts$value >= 0))
})

test_that("calcopycat_fxn only matches within the requested calendar week range", {
  set.seed(1)

  curr_data <- add_growth_cols(tibble(
    date = as.Date(c("2023-10-08", "2023-10-15", "2023-10-22", "2023-10-29", "2023-11-05")),
    target_group = "Overall",
    value = c(10, 11, 12, 13, 14)
  ))

  # Exactly 52 weeks before curr_data's most recent date -- the only
  # candidate that SHOULD be usable.
  hist_close <- tibble(
    date = as.Date(c("2022-10-09", "2022-10-16", "2022-10-23", "2022-10-30", "2022-11-06", "2022-11-13", "2022-11-20")),
    target_group = "hist_close",
    value = c(10, 11, 12, 13, 14, 15, 16)
  )

  # An equally perfect trend match, but anchored ~26 weeks earlier (roughly
  # the opposite time of year, nowhere near any 52-week-multiple anchor)
  # with wildly different future values -- must never be drawn.
  hist_far <- tibble(
    date = as.Date(c("2023-04-09", "2023-04-16", "2023-04-23", "2023-04-30", "2023-05-07", "2023-05-14", "2023-05-21")),
    target_group = "hist_far",
    value = c(10, 11, 12, 13, 14, 100, 200)
  )

  db <- add_growth_cols(bind_rows(hist_close, hist_far))

  forecasts <- calcopycat_fxn(
    curr_data = curr_data,
    db = db,
    forecast_horizon = 2,
    recent_weeks_touse = 4,
    nsamps = 500,
    resp_week_range = 2
  )

  # The single eligible candidate (hist_close) is a near-perfect match, so
  # forecasts cluster tightly around its implied values (15, then 16) --
  # never anywhere near hist_far's wildly different future (100, 200).
  expect_true(all(abs(forecasts$forecast[forecasts$horizon == 1] - 15) < 2))
  expect_true(all(abs(forecasts$forecast[forecasts$horizon == 2] - 16) < 3))
  expect_false(any(forecasts$forecast > 50))
})

test_that("calcopycat_fxn never uses a historical week whose real data doesn't reach the full forecast horizon", {
  set.seed(1)

  curr_data <- add_growth_cols(tibble(
    date = as.Date(c("2023-10-08", "2023-10-15", "2023-10-22", "2023-10-29", "2023-11-05")),
    target_group = "Overall",
    value = c(10, 11, 12, 13, 14)
  ))

  hist_long <- tibble(
    date = as.Date(c("2022-10-09", "2022-10-16", "2022-10-23", "2022-10-30", "2022-11-06", "2022-11-13", "2022-11-20")),
    target_group = "hist_long",
    value = c(10, 11, 12, 13, 14, 15, 16) ## real data for both forecast weeks
  )

  hist_short <- tibble(
    date = as.Date(c("2022-10-09", "2022-10-16", "2022-10-23", "2022-10-30", "2022-11-06", "2022-11-13")),
    target_group = "hist_short",
    value = c(10, 11, 12, 13, 14, 999) ## identical (perfect) trend match, but only ONE real future week
  )

  db_both <- add_growth_cols(bind_rows(hist_long, hist_short))

  forecasts <- calcopycat_fxn(
    curr_data = curr_data,
    db = db_both,
    forecast_horizon = 2,
    recent_weeks_touse = 4,
    nsamps = 500,
    resp_week_range = 2
  )

  expect_true(all(abs(forecasts$forecast[forecasts$horizon == 1] - 15) < 2))
  expect_true(all(abs(forecasts$forecast[forecasts$horizon == 2] - 16) < 3))
  expect_false(any(forecasts$forecast > 50)) ## hist_short's 999 must never leak through

  db_short_only <- add_growth_cols(hist_short)

  expect_error(
    calcopycat_fxn(
      curr_data = curr_data,
      db = db_short_only,
      forecast_horizon = 2,
      recent_weeks_touse = 4,
      nsamps = 10,
      resp_week_range = 2
    ),
    "no historical analogs"
  )
})

test_that("calcopycat_fxn's gap guard excludes an in-ring candidate that's still too close to now", {
  # The calendar-week ring around the nearest (k=1) anchor can get as close
  # as (52 - Respiratory Week Range) weeks before "now". With the range at
  # its max (10) and a Recent-Weeks-to-Use + forecast horizon that exceeds
  # that 42-week floor (35 + 10 = 45), even a candidate that's a perfect,
  # fully-eligible trend/future match must still be excluded -- this is the
  # gap guard catching it, not the calendar ring (which happily includes it).
  most_recent_date   <- as.Date("2024-01-07")
  recent_weeks_touse <- 35
  forecast_horizon   <- 10
  resp_week_range    <- 10

  dates    <- seq(most_recent_date - 96 * 7, most_recent_date - 15 * 7, by = "week")
  cand_row <- which(dates == most_recent_date - 42 * 7)

  growth_factors <- rep(1.01, length(dates) - 1)
  growth_factors[cand_row + 1] <- 50 ## an unmistakable spike, planted immediately
                                      ## after the too-close candidate's date -- if
                                      ## it ever leaked through, horizon 1 would jump
                                      ## from ~143 into the thousands

  hist_data <- add_growth_cols(tibble(
    date = dates,
    target_group = "hist",
    value = 100 * cumprod(c(1, growth_factors))
  ))

  curr_data <- add_growth_cols(tibble(
    date = seq(most_recent_date - recent_weeks_touse * 7, most_recent_date, by = "week"),
    target_group = "Overall",
    value = 100 * 1.01 ^ (0:recent_weeks_touse)
  ))

  forecasts <- calcopycat_fxn(
    curr_data = curr_data,
    db = hist_data,
    forecast_horizon = forecast_horizon,
    recent_weeks_touse = recent_weeks_touse,
    nsamps = 300,
    resp_week_range = resp_week_range
  )

  expect_true(nrow(forecasts) > 0)
  expect_true(all(forecasts$forecast[forecasts$horizon == 1] < 1000)) ## the spike would be ~7083
})

test_that("calcopycat_fxn requires a full, real, gap-free trailing window -- a partial-overlap candidate is never eligible even when it's a perfect trend match", {
  # A single historical series that only has a handful of real weeks leading
  # up to its 52-week-back anchor point, while the current window needs 8.
  # Under the old ">= 4" partial-overlap fallback, a 4-, 5-, or 6-point
  # candidate here would have been accepted (and, being a perfect trend
  # match, would have dominated resampling); under the new strict
  # "n_overlap == n_current" rule none of them qualify, leaving no eligible
  # candidates at all.
  most_recent_date <- as.Date("2024-01-07")

  curr_data <- add_growth_cols(tibble(
    date = seq(most_recent_date - 8 * 7, most_recent_date, by = "week"),
    target_group = "Overall",
    value = 100 * 1.02 ^ (0:8) ## 9 points -> 8 real growth steps
  ))

  anchor_date <- most_recent_date - as.difftime(364, units = "days")
  hist_data <- add_growth_cols(tibble(
    date = seq(anchor_date - 3 * 7, anchor_date + 2 * 7, by = "week"),
    target_group = "hist",
    value = 100 * 1.02 ^ (0:5) ## identical trend to curr_data -- a "perfect" match
  ))

  expect_error(
    calcopycat_fxn(
      curr_data = curr_data,
      db = hist_data,
      forecast_horizon = 1,
      recent_weeks_touse = 8,
      nsamps = 10,
      resp_week_range = 2
    ),
    "no historical analogs"
  )
})

test_that("calcopycat_fxn's growth-rate perturbation shakes a poorly-matching candidate more than a well-matching one", {
  # Both scenarios have exactly one eligible candidate, so any spread in the
  # resampled forecasts comes entirely from the per-trajectory growth-rate
  # perturbation (R/CalCopycat.R point 4), not from resampling across
  # different candidates. A candidate whose trend clearly departs from the
  # current window (high match weight) should get shaken around far more
  # than one that matches almost exactly (weight near zero, floored only by
  # the noise floor).
  most_recent_date <- as.Date("2024-01-07")
  anchor_date       <- most_recent_date - as.difftime(364, units = "days")

  curr_data <- add_growth_cols(tibble(
    date = seq(most_recent_date - 4 * 7, most_recent_date, by = "week"),
    target_group = "Overall",
    value = 100 * 1.02 ^ (0:4) ## constant ~2% weekly growth
  ))

  make_hist <- function(growth_factor) {
    add_growth_cols(tibble(
      date = seq(anchor_date - 3 * 7, anchor_date + 2 * 7, by = "week"),
      target_group = "hist",
      value = 100 * growth_factor ^ (0:5)
    ))
  }

  run <- function(growth_factor, seed) {
    set.seed(seed)
    calcopycat_fxn(
      curr_data = curr_data,
      db = make_hist(growth_factor),
      forecast_horizon = 1,
      recent_weeks_touse = 4,
      nsamps = 3000,
      resp_week_range = 2
    )
  }

  good_match <- run(1.02, seed = 11)  ## same ~2% growth as curr_data
  poor_match <- run(1.10, seed = 12)  ## a clearly different ~10% growth trend

  sd_good <- sd(good_match$forecast)
  sd_poor <- sd(poor_match$forecast)

  expect_true(sd_good < 1)
  expect_true(sd_poor > 3)
  expect_true(sd_poor > 10 * sd_good)
})

test_that("calcopycat_fxn's growth-rate perturbation widens like sqrt(horizon), not linearly, as horizon increases", {
  # Regression test: the perturbation used to draw ONE random shift per
  # trajectory and reuse it at every horizon step, so uncertainty compounded
  # linearly in horizon inside an exponential and blew up by several orders
  # of magnitude at longer horizons on real, noisy count data (observed in
  # production on real state-level RSV hospitalization data). The fix draws
  # an independent shift at each horizon step and scales its size down by
  # n_current (the standard error of the window's average growth rate, not
  # the raw single-week noise level), so spread should grow roughly like
  # sqrt(horizon) instead.
  set.seed(21)

  most_recent_date <- as.Date("2024-01-07")
  anchor_date <- most_recent_date - as.difftime(364, units = "days")

  curr_data <- add_growth_cols(tibble(
    date = seq(most_recent_date - 4 * 7, most_recent_date, by = "week"),
    target_group = "Overall",
    value = 100 * 1.02 ^ (0:4) ## constant ~2% weekly growth
  ))

  # A single eligible candidate whose own trend clearly differs from
  # curr_data's (a real, nonzero match weight) and whose future growth is
  # also constant, so any variability across sampled trajectories comes
  # entirely from the growth-rate perturbation, not from real data variety.
  hist_data <- add_growth_cols(tibble(
    date = seq(anchor_date - 3 * 7, anchor_date + 5 * 7, by = "week"),
    target_group = "hist",
    value = 100 * 1.10 ^ (0:8) ## a clearly different ~10% growth trend
  ))

  forecasts <- calcopycat_fxn(
    curr_data = curr_data,
    db = hist_data,
    forecast_horizon = 4,
    recent_weeks_touse = 4,
    nsamps = 5000,
    resp_week_range = 2
  )

  sds <- forecasts |>
    group_by(horizon) |>
    summarize(sd = sd(forecast), .groups = "drop") |>
    arrange(horizon) |>
    pull(sd)

  ratio <- sds[4] / sds[1]

  ## sqrt(4) = 2 is the ideal target; the old single-reused-shift design
  ## measured ~5.7 on this exact scenario (and much worse -- multiple orders
  ## of magnitude -- on real, noisier data). Give enough headroom for Monte
  ## Carlo noise while still catching a regression back toward linear-in-
  ## horizon compounding.
  expect_true(ratio > 1.2)
  expect_true(ratio < 4)
})

test_that("calcopycat_fxn's resolution-aware noise floor keeps a fully-flat (rounded-to-the-floor) proportion window from collapsing to near-zero uncertainty", {
  # A rounded percentage can report the exact same value for many
  # consecutive weeks once true prevalence falls below the reporting
  # resolution (e.g. RSV ED-visit % pinned at its smallest representable
  # value all summer) -- a real seasonal trough, not evidence that nothing
  # is changing. That gives var_current == 0, the same signature as a
  # window with genuinely zero volatility. Without resolution_q, the noise
  # floor collapses to its 1e-6 minimum and the forecast becomes nearly
  # deterministic right when uncertainty about a season turning should be
  # highest; with resolution_q set to the data's own rounding step, the
  # floor stays at a sensible, data-derived level instead.
  most_recent_date <- as.Date("2024-01-07")
  anchor_date <- most_recent_date - as.difftime(364, units = "days")
  floor_value <- 0.0002 ## a proportion's floor value (0.0001) already shifted by +1e-4

  curr_data <- add_growth_cols(tibble(
    date = seq(most_recent_date - 12 * 7, most_recent_date, by = "week"),
    target_group = "Overall",
    value = rep(floor_value, 13) ## 13 points -> 12 flat (zero) growth steps
  ))

  hist_data <- add_growth_cols(tibble(
    date = seq(anchor_date - 11 * 7, anchor_date + 2 * 7, by = "week"),
    target_group = "hist",
    value = rep(floor_value, 14) ## an equally flat historical analog, with 2 extra weeks of real future data
  ))

  run <- function(resolution_q, seed) {
    set.seed(seed)
    calcopycat_fxn(
      curr_data = curr_data,
      db = hist_data,
      forecast_horizon = 1,
      recent_weeks_touse = 12,
      nsamps = 3000,
      resp_week_range = 2,
      resolution_q = resolution_q
    )
  }

  without_floor <- run(NA_real_, seed = 31)
  with_floor    <- run(1e-4, seed = 31)

  sd_without <- sd(without_floor$forecast)
  sd_with    <- sd(with_floor$forecast)

  expect_true(sd_without < 1e-4) ## collapses toward deterministic without the fix
  expect_true(sd_with > 20 * sd_without) ## resolution floor meaningfully widens it back out
})
