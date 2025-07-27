# Setup ========================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(gam)
library(forcats)
library(splines)

# Wrangle data for Copycat =====================================================

wrangle_copycat <- function(
  dataframe,
  forecast_date
) {
  forecast_date <- as.Date(forecast_date)
  curr_resp_season <- year(forecast_date)

  recent_sari <- dataframe |>
    filter(
      year == curr_resp_season,
      week >= 1,
      date < forecast_date
    ) |>
    group_by(target_group) |>
    arrange(week)

  historic_sari <- dataframe |>
    filter(
      year != curr_resp_season,
      week >= 1
    ) |>
    group_by(target_group) |>
    arrange(week)

  list(recent_sari = recent_sari, historic_sari = historic_sari)
}

# Fit and process Copycat ======================================================

fit_process_copycat <- function(
  fit_df,
  historic_df,
  forecast_date,
  data_to_drop,
  forecast_horizon = 5, ## How many weeks forecast and plotted?
  recent_weeks_touse = 5, ## 100 means all data from season are used
  nsamps = 1000,
  resp_week_range = 0
) {
  forecast_date <- as.Date(forecast_date)

  quantiles_needed <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

  config <- switch(
    data_to_drop,
    "0 weeks" = list(days_before = 2, weeks_ahead = 4, weeks_to_drop = 0),
    "1 week" = list(days_before = 2, weeks_ahead = 5, weeks_to_drop = 1),
    "2 week" = list(days_before = 7, weeks_ahead = 6, weeks_to_drop = 2),
    stop("Invalid data_to_drop option")
  )

  weeks_to_drop <- config$weeks_to_drop

  # Helper function to create seasonal trajectory splines
  get_seasonal_spline_vals <- function(season_weeks, value) {
    padding <- 5
    new_value <- c(
      rep(head(value, 1), padding),
      value,
      rep(tail(value, 1), padding)
    )
    new_season_weeks <- c(
      rev(min(season_weeks) - 1:padding),
      season_weeks,
      max(season_weeks) + 1:padding
    )
    weekly_change <- lead(new_value + 1) / (new_value + 1)
    weekly_change <- ifelse(is.na(weekly_change), 1, weekly_change)
    df <- tibble(new_season_weeks, weekly_change)

    mod <- gam(
      log(weekly_change) ~ s(new_season_weeks, length(season_weeks) / 4),
      data = df
    )

    tibble(
      weeks = new_season_weeks,
      pred = mod$fitted.values,
      pred_se = as.numeric(predict(mod, se = TRUE)$se.fit)
    ) |>
      filter(weeks %in% season_weeks)
  }

  # Function to augment annual data for forecasting
  augment_annual_data <- function(year, target_group, df) {
    df |>
      filter(
        target_group == .env$target_group,
        year == .env$year | (year == .env$year + 1 & week < 10),
        !(year == 2015 & week == 53),
        !(year == 2021 & week == 53)
      ) |>
      arrange(date) |>
      mutate(year = .env$year, week = seq_along(week))
  }

  # Build trajectory database
  traj_db <- historic_df |>
    distinct(year, target_group) |>
    pmap(augment_annual_data, df = historic_df) |>
    bind_rows() |>
    group_by(target_group, year) |>
    arrange(week) |>
    mutate(get_seasonal_spline_vals(week, value)) |>
    ungroup() |>
    select(target_group, year, week, pred, pred_se)

  # Forecast processing
  groups <- unique(fit_df$target_group)
  group_forecasts <- vector("list", length = length(groups))

  for (curr_group in groups) {
    fit_df |>
      ungroup() |>
      filter(
        target_group == curr_group,
        year == max(fit_df$year),
        week <= max(fit_df$week) - weeks_to_drop
      ) |>
      mutate(value = value + 1) |>
      mutate(curr_weekly_change = log(lead(value) / value)) |>
      select(week, value, curr_weekly_change) |>
      paraguay_copycat(
        db = traj_db,
        recent_weeks_touse = recent_weeks_touse,
        resp_week_range = resp_week_range,
        forecast_horizon = forecast_horizon + weeks_to_drop
      ) |>
      mutate(forecast = forecast - 1) |>
      mutate(forecast = ifelse(forecast < 0, 0, forecast)) -> forecast_trajectories

    cleaned_forecasts_quantiles <- forecast_trajectories |>
      group_by(week) |>
      summarize(qs = list(
        value = quantile(forecast, probs = quantiles_needed)
      )) |>
      mutate(horizon = seq_along(week) - weeks_to_drop - 1) |>
      unnest_wider(qs) |>
      gather(quantile, value, -week, -horizon) |>
      ungroup() |>
      # pivot_longer(
      #   cols = -c(week, horizon),
      #   names_to = "quantile",
      #   values_to = "value"
      # ) |>
      mutate(
        quantile = as.numeric(gsub("[\\%,]", "", quantile)) / 100,
        target_group = curr_group,
        # commented out since we changed "inc sari hosp" to "value
        # target = paste0("inc sari hosp"),
        reference_date = forecast_date + 3,
        target_end_date = forecast_date + 3 + horizon * 7,
        output_type_id = as.numeric(quantile),
        output_type = "quantile",
        value = round(value)
      ) |>
      select(
        reference_date,
        # target, # commented out since we changed "inc sari hosp" to "value
        horizon,
        target_end_date,
        target_group,
        output_type,
        output_type_id,
        value
      )

    group_forecasts[[match(curr_group, groups)]] <- cleaned_forecasts_quantiles |>
      mutate(output_type_id = as.character(output_type_id))
  }

  final_forecasts <- bind_rows(group_forecasts) |>
    filter(horizon >= 0) |>
    mutate(horizon = horizon, target_end_date = target_end_date) |>
    arrange(target_group, horizon, output_type_id)

  # final_forecasts |>
  # write_csv(paste0("processed-data/paraguay-rt-forecasts/", forecast_date + 3, "-UGA_flucast-Copycat.csv"))

  return(final_forecasts)
}

paraguay_copycat <- function(
  curr_data,
  forecast_horizon = 5, ## How many weeks forecast and plotted?
  recent_weeks_touse = 5, ## 100 means all data from season are used
  nsamps = 1000,
  resp_week_range = 0,
  db = traj_db
) {
  most_recent_week <- max(curr_data$week)
  most_recent_value <- tail(curr_data$value, 1)

  cleaned_data <- curr_data |>
    select(week, curr_weekly_change) |>
    filter(!is.na(curr_weekly_change)) |>
    tail(recent_weeks_touse)

  if (resp_week_range != 0) {
    matching_data <- cleaned_data |>
      mutate(week_change = list(
        c(-(1:resp_week_range), 0, (1:resp_week_range))
      )) |>
      unnest(week_change) |>
      mutate(week = week + week_change) |>
      filter(week > 0)
  } else {
    matching_data <- cleaned_data |>
      mutate(week_change = 0) |>
      filter(week > 0)
  }

  db |>
    inner_join(
      matching_data,
      by = "week",
      relationship = "many-to-many"
    ) |>
    group_by(week_change, target_group, year) |>
    filter(
      n() == nrow(cleaned_data) | n() >= 4
    ) |> ## Makes sure you've matched as many as the cleaned data or at least a full month
    # filter(metric == 'flusurv') |>  ## If you want to limit to specific database
    summarize(
      weight = sum((pred - curr_weekly_change)^2) / n(),
      .groups = "drop"
    ) |>
    ungroup() |>
    filter(!is.na(weight)) -> traj_temp

  min_allowed_weight <- 0.02

  traj_temp |>
    mutate(weight = ifelse(
      weight < min_allowed_weight,
      min_allowed_weight,
      weight
    )) |>
    arrange(weight) |>
    slice(1:20) |>
    sample_n(size = nsamps, replace = T, weight = 1 / weight^2) |>
    mutate(id = seq_along(weight)) |>
    select(id, target_group, year, week_change) -> trajectories

  trajectories |>
    left_join(
      db |>
        nest(data = c("week", "pred", "pred_se")),
      by = c("target_group", "year")
    ) |>
    unnest(data) |>
    mutate(week = week - week_change) |>
    filter(
      week %in% most_recent_week:(most_recent_week + forecast_horizon - 1)
    ) |>
    mutate(weekly_change = exp(rnorm(n(), pred, pred_se))) |>
    group_by(id) |>
    arrange(week) |>
    mutate(mult_factor = cumprod(weekly_change)) |>
    ungroup() |>
    mutate(
      forecast = most_recent_value * mult_factor
    ) |> ## Want more dispersion than poisson distribution
    # mutate(forecast = rnbinom(n(), mu = most_recent_value*mult_factor, size = 100)) |> ##Want more dispersion than poisson distribution
    mutate(
      forecast = rpois(n = n(), lambda = forecast)
    ) |> ## Want poisson dispersion
    mutate(week = week + 1) |>
    select(id, week, forecast)
}
