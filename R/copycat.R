# Wrangle data for Copycat =====================================================

wrangle_copycat <- function(
  df,
  seasonality
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

fit_process_copycat <- function(df,
                                fcast_horizon, ## How many weeks forecast and plotted?
                                quantiles_needed, ## The desired quantiles for the output
                                seasonality,
                                recent_weeks_touse = 5, ## 100 means all data from season are used
                                nsamps = 1000,
                                resp_week_range = 0,
                                share_groups = TRUE,
                                weight_exponent = 2, ## Exponent applied to 1/weight when resampling analogs
                                add_poisson_noise = TRUE, ## Whether to add Poisson observation noise
                                points_per_knot = 5, ## Roughly how many data points per GAM spline knot
                                max_matches = Inf, ## Cap on how many closest-matching historical trajectories are eligible for resampling; Inf (default) uses every eligible match
                                data_type = "count", ## "count" (unbounded, Poisson noise) or "proportion" (bounded 0-1, Beta noise)
                                noise_dispersion = 100) { ## Beta-noise concentration used only when data_type == "proportion"; higher = tighter around the simulated trajectory

  # Copycat internal functions ----------------------------------------------
  get_full_year_df <- function(curr_year, full_df){
    ## This returns a full influenza season worth of weekly data.
    ## It extends the year by 10 weeks, so that forecasts can go into the next year (this enables year round forecasting efforts)
    ## It also prepends the last 10 weeks of the previous season so early-season matching has enough recent changes.
    ## It also removes years of data where there are less than 50 weeks (not ideal, but ensures that the years are aligned well)

    full_df |>
      filter(resp_season_year == curr_year) |>
      count(target_group) |>
      pull(n) |> min() -> weeks_in_year

    prior_df <- full_df |>
      filter(resp_season_year == curr_year - 1) |>
      group_by(target_group) |>
      arrange(date, .by_group = TRUE) |>
      slice_tail(n = 10) |>
      mutate(resp_season_week = as.integer(row_number() - n())) |>
      mutate(resp_season_year = curr_year) |>
      ungroup()

    current_and_future_df <- full_df |>
      filter(resp_season_year>=curr_year) |>
      group_by(target_group) |>
      arrange(date, .by_group = TRUE) |>
      mutate(resp_season_week = seq_along(value)) |>
      filter(resp_season_week <= 62) |>
      mutate(resp_season_year = curr_year) |>
      ungroup()

    bind_rows(prior_df, current_and_future_df) |>
      arrange(target_group, resp_season_week) -> df_to_return


    return(df_to_return |> mutate(year_too_short = ifelse(weeks_in_year < 50, TRUE, FALSE)))

  }


  # Helper function to create seasonal trajectory splines
  get_seasonal_spline_vals <- function(season_weeks, value, points_per_knot = 5) {
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

    spline_k <- max(4, min(length(season_weeks), floor(length(season_weeks) / points_per_knot)))

    mod <- mgcv::gam(
      log(weekly_change) ~ s(new_season_weeks, k = spline_k),
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

  if(seasonality == 'D' | seasonality == 'E'){
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
    mutate(get_seasonal_spline_vals(resp_season_week, value, points_per_knot = points_per_knot)) |>
    ungroup() |>
    select(target_group, resp_season_year, resp_season_week, pred, pred_se)

  # ## Plotting trajectory database for debuggin
  # traj_db |>
  #   ggplot(aes(x = resp_season_week, y = pred)) +
  #   geom_ribbon(aes(ymin = pred - 1.96 * pred_se, ymax = pred + 1.96 * pred_se),
  #               alpha = 0.2, fill = "steelblue") +
  #   geom_line(color = "steelblue") +
  #   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  #   facet_grid(rows = vars(target_group), cols = vars(resp_season_year)) +
  #   labs(x = "Respiratory Season Week", y = "Log Weekly Growth Rate (fitted)",
  #        title = "GAM-fitted growth trajectories by group and season") +
  #   theme_bw()


  # Forecast processing
  groups <- unique(recent_df$target_group)
  group_forecasts <- vector("list", length = length(groups))
  for (curr_group in groups) {
    recent_df |>
      ungroup() |>
      filter(
        target_group == curr_group) |>
      mutate(value = value + 1) |>
      mutate(curr_weekly_change = log(lead(value) / value)) |>
      select(resp_season_week, value, curr_weekly_change) |>
      copycat_fxn(
        db = if (share_groups) traj_db else filter(traj_db, target_group == curr_group),
        recent_weeks_touse = recent_weeks_touse,
        nsamps = nsamps,
        resp_week_range = resp_week_range,
        forecast_horizon = fcast_horizon,
        weight_exponent = weight_exponent,
        add_poisson_noise = add_poisson_noise,
        max_matches = max_matches,
        data_type = data_type,
        noise_dispersion = noise_dispersion
      ) |>
      mutate(forecast = forecast - 1) |>
      mutate(forecast = if (identical(data_type, "proportion")) {
        pmin(pmax(forecast, 0), 1)
      } else {
        pmax(forecast, 0)
      }) -> forecast_trajectories

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
  db = traj_db,
  weight_exponent = 2, ## Exponent applied to 1/weight when resampling analogs
  add_poisson_noise = TRUE, ## Whether to add observation noise (Poisson for counts, Beta for proportions)
  max_matches = Inf, ## Cap on how many closest-matching historical trajectories are eligible for resampling; Inf uses every eligible match
  data_type = "count", ## "count" or "proportion"
  noise_dispersion = 100 ## Beta-noise concentration, used only when data_type == "proportion"
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
      mutate(resp_season_week = resp_season_week + week_change)
  } else {
    matching_data <- cleaned_data |>
      mutate(week_change = 0)
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
    arrange(weight) -> traj_temp

  if (is.finite(max_matches)) {
    ## Restrict resampling to the top `max_matches` closest-matching historical
    ## trajectories (lowest weight = lowest matching error). Inf (default)
    ## keeps every trajectory that passed the overlap filter above.
    traj_temp <- traj_temp |> slice_head(n = max(1, floor(max_matches)))
  }

  traj_temp |>
    sample_n(size = nsamps, replace = T, weight = 1 / weight^weight_exponent) |>
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
    ) -> trajectories_out ## Want more dispersion than poisson distribution
    # mutate(forecast = rnbinom(n(), mu = most_recent_value*mult_factor, size = 100)) |> ##Want more dispersion than poisson distribution

  if (isTRUE(add_poisson_noise)) {
    trajectories_out <- if (identical(data_type, "proportion")) {
      # Beta noise centered on the simulated trajectory value -- the
      # proportion analog of Poisson noise centered on a count trajectory.
      # `noise_dispersion` is the Beta concentration (phi = shape1 + shape2):
      # higher values draw tightly around `forecast`, lower values spread the
      # draw out and widen the resulting forecast interval.
      trajectories_out |>
        mutate(
          forecast_mu = pmin(pmax(forecast, 1e-4), 1 - 1e-4),
          forecast = rbeta(
            n(),
            forecast_mu * noise_dispersion,
            (1 - forecast_mu) * noise_dispersion
          )
        ) |>
        select(-forecast_mu)
    } else {
      trajectories_out |>
        mutate(forecast = rpois(n = n(), lambda = forecast)) ## Want poisson dispersion
    }
  }

  trajectories_out |>
    mutate(resp_season_week = resp_season_week + 1) |>
    select(id, resp_season_week, forecast)
}


# Theoretical ceiling for max_matches ==========================================
#
# Upper bound on how many distinct historical trajectories fit_process_copycat()
# could possibly draw from, given the trajectory-matching settings. This is an
# UPPER BOUND, not an exact count: fit_process_copycat()'s own minimum-overlap
# filter (n() >= 4 in copycat_fxn()) can shrink the realized candidate pool
# further once actual week-by-week matching happens. Used to bound the
# "Max Historical Matches" UI control.

copycat_max_possible_matches <- function(df,
                                          seasonality,
                                          resp_week_range = 0,
                                          share_groups = TRUE) {

  if (seasonality == 'D' | seasonality == 'E') {
    df <- df |>
      mutate(resp_season_year = MMWRweek(date)$MMWRyear)
  } else {
    df <- df |>
      mutate(year = MMWRweek(date)$MMWRyear,
             week = MMWRweek(date)$MMWRweek) |>
      mutate(resp_season_year = ifelse(week >= 40, year, year - 1)) |>
      select(-year, -week)
  }

  most_recent_year <- max(df$resp_season_year)

  ## Mirrors get_full_year_df()'s year_too_short rule: a season only counts as
  ## a usable historical trajectory if every target group has at least 50
  ## weeks of data that year.
  season_counts <- df |>
    filter(resp_season_year != most_recent_year) |>
    count(target_group, resp_season_year)

  eligible <- season_counts |>
    group_by(resp_season_year) |>
    filter(min(n) >= 50) |>
    ungroup()

  n_shifts <- if (resp_week_range != 0) (2 * resp_week_range + 1) else 1

  if (isTRUE(share_groups)) {
    nrow(eligible) * n_shifts
  } else {
    group_counts <- eligible |> count(target_group, name = "n_series")
    if (nrow(group_counts) == 0) {
      0
    } else {
      min(group_counts$n_series) * n_shifts
    }
  }
}
