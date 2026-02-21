# Wrangle data for Copycat =====================================================

wrangle_copycat <- function(
  df,
  seasonality
) {
  browser()

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

fit_process_copycat <- function(df,
                                fcast_horizon, ## How many weeks forecast and plotted?
                                quantiles_needed, ## The desired quantiles for the output
                                seasonality,
                                recent_weeks_touse = 5, ## 100 means all data from season are used
                                nsamps = 1000,
                                resp_week_range = 0) {

  # Copycat internal functions ----------------------------------------------
  get_full_year_df <- function(curr_year, full_df){
    ## This returns a full influenza season worth of weekly data.
    ## It extends the year by 10 weeks, so that forecasts can go into the next year (this enables year round forecasting efforts)
    ## It also removes years of data where there are less than 50 weeks (not ideal, but ensures that the years are aligned well)

    full_df |>
      filter(resp_season_year == curr_year) |>
      count(target_group) |>
      pull(n) |> min() -> weeks_in_year

    full_df |>
      filter(resp_season_year>=curr_year) |>
      group_by(target_group) |>
      mutate(resp_season_week = seq_along(value)) |>
      filter(resp_season_week <= 62) |>
      mutate(resp_season_year = curr_year) |>
      ungroup() -> df_to_return


    return(df_to_return |> mutate(year_too_short = ifelse(weeks_in_year < 50, TRUE, FALSE)))

  }


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
      log(weekly_change) ~ s(new_season_weeks, length(season_weeks) / 5),
      data = df
    )

    tibble(
      weeks = new_season_weeks,
      pred = mod$fitted.values,
      pred_se = as.numeric(predict(mod, se = TRUE)$se.fit)
    ) |>
      filter(weeks %in% season_weeks) #|>
      # ggplot(aes(weeks, pred)) + geom_line() +
      # geom_point(data = df, aes(x = new_season_weeks, log(weekly_change)), inherit.aes=F)
  }



  # Start the code ----------------------------------------------------------

  if(seasonality == 'SH' | seasonality == 'T'){
    df |>
      mutate(resp_season_year = MMWRweek(date)$MMWRyear) -> df
  } else{
    ## Still need to double check this one works
    df |>
      mutate(year = MMWRweek(date)$MMWRyear,
             week = MMWRweek(date)$MMWRweek) |>
      mutate(resp_season_year = ifelse(week >= 40, year, year-1)) |>
      select(-year, -week) -> df
  }

  ## Expand influenza seasons and align everything by weeks up to 62
  unique(df$resp_season_year) |>
    map(get_full_year_df, full_df = df) |>
    bind_rows() -> temp

  most_recent_year <- max(df$resp_season_year)

  temp |>
    filter(resp_season_year == most_recent_year) -> recent_df

  temp |>
    filter(resp_season_year != most_recent_year,
           !year_too_short) -> historic_df

  # Build trajectory database
  traj_db <- historic_df |>
    group_by(target_group, resp_season_year) |>
    arrange(resp_season_week) |>
    mutate(get_seasonal_spline_vals(resp_season_week, value)) |>
    ungroup() |>
    select(target_group, resp_season_year, resp_season_week, pred, pred_se)

  ## Plotting trajectory database for debuggin
  # traj_db |>
  #   ggplot(aes(resp_season_week, pred, color = interaction(resp_season_year, target_group))) +
  #   geom_line()
  #

  ## Need to add functionality to either share information across target groups or not
  ## Also need to add functionality for selecting count vs percentage forecasts

  # Forecast processing
  groups <- unique(recent_df$target_group)
  group_forecasts <- vector("list", length = length(groups))
  # browser()
  for (curr_group in groups) {
    recent_df |>
      ungroup() |>
      filter(
        target_group == curr_group) |>
      mutate(value = value + 1) |>
      mutate(curr_weekly_change = log(lead(value) / value)) |>
      select(resp_season_week, value, curr_weekly_change) |>
      copycat_fxn(
        db = traj_db,
        recent_weeks_touse = recent_weeks_touse,
        resp_week_range = resp_week_range,
        forecast_horizon = fcast_horizon
      ) |>
      mutate(forecast = forecast - 1) |>
      mutate(forecast = ifelse(forecast < 0, 0, forecast)) -> forecast_trajectories

    ## Plot the forecasts with the data - just used for debugging
    # recent_df |>
    #   filter(target_group == curr_group) |>
    #   ggplot(aes(resp_season_week, value)) +
    #   geom_point() +
    #   geom_line(data = forecast_trajectories, aes(resp_season_week, forecast, group = as.factor(id)), inherit.aes=F, alpha = .1)

    cleaned_forecasts_quantiles <- forecast_trajectories |>
      group_by(resp_season_week) |>
      summarize(qs = list(
        value = quantile(forecast, probs = quantiles_needed)
      )) |>
      mutate(horizon = seq_along(resp_season_week)) |>
      unnest_wider(qs) |>
      gather(quantile, value, -resp_season_week, -horizon) |>
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
        # reference_date = forecast_date + 3,
        # target_end_date = forecast_date + 3 + horizon * 7,
        output_type_id = as.numeric(quantile),
        output_type = "quantile",
        value = value
      ) |>
      select(
        horizon,
        target_group,
        output_type,
        output_type_id,
        value
      )

    group_forecasts[[match(curr_group, groups)]] <- cleaned_forecasts_quantiles |>
      mutate(output_type_id = as.character(output_type_id))
  }

  final_forecasts <- bind_rows(group_forecasts) |>
    arrange(target_group, horizon, output_type_id)

  return(final_forecasts)
}

copycat_fxn <- function(
  curr_data,
  forecast_horizon = 5, ## How many weeks forecast and plotted?
  recent_weeks_touse = 5, ## 100 means all data from season are used
  nsamps = 1000,
  resp_week_range = 0,
  db = traj_db
) {
  most_recent_week <- max(curr_data$resp_season_week)
  most_recent_value <- tail(curr_data$value, 1)

  cleaned_data <- curr_data |>
    select(resp_season_week, curr_weekly_change) |>
    filter(!is.na(curr_weekly_change)) |>
    tail(recent_weeks_touse)

  if (resp_week_range != 0) {
    matching_data <- cleaned_data |>
      mutate(week_change = list(
        c(-(1:resp_week_range), 0, (1:resp_week_range))
      )) |>
      unnest(week_change) |>
      mutate(resp_season_week = resp_season_week + week_change) |>
      filter(resp_season_week > 0)
  } else {
    matching_data <- cleaned_data |>
      mutate(week_change = 0) |>
      filter(week > 0)
  }

  db |>
    inner_join(
      matching_data,
      by = "resp_season_week",
      relationship = "many-to-many"
    ) |>
    group_by(week_change, target_group, resp_season_year) |>
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
    select(id, target_group, resp_season_year, week_change) -> trajectories

  trajectories |>
    left_join(
      db |>
        nest(data = c("resp_season_week", "pred", "pred_se")),
      by = c("target_group", "resp_season_year")
    ) |>
    unnest(data) |>
    mutate(resp_season_week = resp_season_week - week_change) |>
    filter(
      resp_season_week %in% most_recent_week:(most_recent_week + forecast_horizon - 1)
    ) |>
    mutate(weekly_change = exp(rnorm(n(), pred, pred_se))) |>
    group_by(id) |>
    arrange(resp_season_week) |>
    mutate(mult_factor = cumprod(weekly_change)) |>
    ungroup() |>
    mutate(
      forecast = most_recent_value * mult_factor
    ) |> ## Want more dispersion than poisson distribution
    # mutate(forecast = rnbinom(n(), mu = most_recent_value*mult_factor, size = 100)) |> ##Want more dispersion than poisson distribution
    mutate(
      forecast = rpois(n = n(), lambda = forecast)
    ) |> ## Want poisson dispersion
    mutate(resp_season_week = resp_season_week + 1) |>
    select(id, resp_season_week, forecast)
}
