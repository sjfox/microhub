# Setup ========================================================================

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(mgcv)

# Wrangle data for seasonal baseline ===========================================

wrangle_baseline_seasonal <- function(
  dataframe,
  forecast_date
) {
  forecast_year <- year(forecast_date)
  forecast_week <- isoweek(forecast_date)

  data <- dataframe |>
    mutate(
      year = year(date),
      week = isoweek(date),
      count = ifelse(value == 0, 0.5, value),
      season_week = week
    ) |>
    filter(year < forecast_year |
      (year == forecast_year & week < forecast_week))

  return(data)
}

# Fit and process opt baseline =================================================

fit_process_baseline_seasonal <- function(
  clean_data,
  forecast_date,
  forecast_horizons = 1:5,
  n_sim = 100000,
  data_to_drop = "0 weeks"
) {
  drop_config <- switch(
    data_to_drop,
    "0 weeks" = list(days_before = 0, weeks_ahead = 4),
    "1 week"  = list(days_before = 4, weeks_ahead = 5),
    "2 week"  = list(days_before = 11, weeks_ahead = 6),
    stop("Invalid data_to_drop option")
  )

  drop_cutoff <- forecast_date - drop_config$days_before
  clean_data <- clean_data |> filter(date < drop_cutoff)

  target_end_dates <- seq.Date(
    from = forecast_date - 7,
    by = "1 week",
    length.out = drop_config$weeks_ahead
  )
  season_weeks <- isoweek(target_end_dates)
  quantiles <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)

  fit_gam <- function(df) {
    gam(log(count) ~ s(season_week, bs = "cc"), data = df, method = "REML")
  }

  predict_gam <- function(fm) {
    beta <- coef(fm)
    Vb <- vcov(fm)
    nb <- length(beta)
    Cv <- chol(Vb)
    br <- t(Cv) %*% matrix(rnorm(n_sim * nb), nb, n_sim) + beta
    Xp <- predict(fm, newdata = data.frame(season_week = season_weeks), type = "lpmatrix")
    lp <- Xp %*% br
    tmp <- matrix(rnorm(length(lp), mean = lp, sd = sqrt(fm$sig2)), nrow = nrow(lp))
    apply(tmp, 1, function(x) quantile(exp(x), probs = quantiles))
  }

  create_output <- function(pred_matrix, group) {
    pred_matrix <- t(pred_matrix) + 1 / n_sim

    df <- expand.grid(
      target_end_date = target_end_dates,
      output_type_id = quantiles
    ) |>
      mutate(
        value = as.vector(pred_matrix),
        horizon = as.integer(difftime(target_end_date, forecast_date, units = "weeks")),
        target_group = group,
        output_type = "quantile"
      )

    point_df <- df |>
      filter(output_type_id == 0.5) |>
      mutate(output_type = "point")

    bind_rows(df, point_df) |>
      distinct() |>
      mutate(reference_date = forecast_date) |>
      select(
        reference_date,
        horizon,
        target_end_date,
        target_group,
        output_type,
        output_type_id,
        value
      )
  }

  target_groups <- unique(clean_data$target_group)
  output_list <- map(target_groups, function(group) {
    group_data <- clean_data |> filter(target_group == group)

    if (nrow(group_data) >= 10) {
      model <- fit_gam(group_data)
      preds <- predict_gam(model)
      create_output(preds, group)
    } else {
      NULL
    }
  })

  bind_rows(output_list)
}
