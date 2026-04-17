# CalCopycat =========================================================
#
# Trajectory-level residual bootstrap with LOO calibration stratified by
# epidemic week. See fit_process_calcopycat() for the main entry point.
#
# NOTE: get_seasonal_spline_vals() is re-defined below because it is a nested
# (non-exported) function in copycat.R. copycat_fxn_cal() is a local variant
# of copycat_fxn() that removes Poisson observation noise — uncertainty in the
# calibrated model comes from trajectory sampling, GAM growth-rate noise, and
# LOO structural residuals instead.


# Helper: GAM spline values for one season =====================================

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
    weeks   = new_season_weeks,
    pred    = mod$fitted.values,
    pred_se = as.numeric(predict(mod, se = TRUE)$se.fit)
  ) |>
    filter(weeks %in% season_weeks)
}


# Helper: Reconstruct levels from log-growth trajectories =======================

growth_to_level_trajectories <- function(growth_trajectories, starting_value) {
  growth_trajectories |>
    arrange(id, horizon) |>
    group_by(id) |>
    mutate(
      mult_factor = cumprod(exp(growth_rate)),
      forecast    = starting_value * mult_factor
    ) |>
    ungroup() |>
    select(id, resp_season_week, horizon, forecast)
}


# Helper: Smooth residual draws =================================================

sample_smoothed_residual <- function(residuals,
                                     adjust = 1,
                                     density_n = 300) {
  residuals <- residuals[is.finite(residuals)]

  if (length(residuals) == 0) return(0)

  resid_sd <- stats::sd(residuals)

  if (length(residuals) < 4 || is.na(resid_sd) || resid_sd == 0) {
    return(ifelse(is.na(resid_sd) || resid_sd == 0, 0, stats::rnorm(1, 0, resid_sd)))
  }

  dens <- stats::density(residuals, adjust = adjust, n = density_n)
  weights <- pmax(dens$y, 0)

  if (sum(weights) <= 0 || anyNA(weights)) {
    return(stats::rnorm(1, 0, resid_sd))
  }

  sample(dens$x, size = 1, prob = weights)
}


# Copycat sampler (no Poisson noise) ==========================================
#
# Identical to copycat_fxn() in R/copycat.R except the rpois() observation
# noise step is omitted. Poisson noise is an arbitrary additive layer that
# double-counts uncertainty when LOO structural residuals are also present.

calcopycat_fxn <- function(curr_data,
                             forecast_horizon   = 5,
                             recent_weeks_touse = 12,
                             nsamps             = 1000,
                             resp_week_range    = 0,
                             db                 = traj_db,
                             return_scale       = c("level", "growth")) {

  return_scale <- match.arg(return_scale)

  most_recent_week  <- max(curr_data$resp_season_week)
  most_recent_value <- tail(curr_data$value, 1)

  cleaned_data <- curr_data |>
    select(resp_season_week, curr_weekly_change) |>
    filter(!is.na(curr_weekly_change)) |>
    tail(recent_weeks_touse)

  if (resp_week_range != 0) {
    matching_data <- cleaned_data |>
      mutate(week_change = list(c(-(1:resp_week_range), 0, (1:resp_week_range)))) |>
      unnest(week_change) |>
      mutate(resp_season_week = resp_season_week + week_change) |>
      filter(resp_season_week > 0)
  } else {
    matching_data <- cleaned_data |>
      mutate(week_change = 0) |>
      filter(resp_season_week > 0)
  }

  db |>
    inner_join(matching_data, by = "resp_season_week", relationship = "many-to-many") |>
    group_by(week_change, target_group, resp_season_year) |>
    filter(n() == nrow(cleaned_data) | n() >= 4) |>
    summarize(weight = sum((pred - curr_weekly_change)^2) / n(), .groups = "drop") |>
    ungroup() |>
    filter(!is.na(weight)) -> traj_temp

  min_allowed_weight <- 0.02

  traj_temp |>
    mutate(weight = ifelse(weight < min_allowed_weight, min_allowed_weight, weight)) |>
    arrange(desc(weight)) |>
    sample_n(size = nsamps, replace = TRUE, weight = 1 / weight^2) |>
    mutate(id = seq_along(weight)) |>
    select(id, target_group, resp_season_year, week_change) -> trajectories

  # browser()

  sampled_growth <- trajectories |>
    left_join(
      db |> nest(data = c("resp_season_week", "pred", "pred_se")),
      by = c("target_group", "resp_season_year")
    ) |>
    unnest(data) |>
    mutate(resp_season_week = resp_season_week - week_change) |>
    filter(resp_season_week %in% most_recent_week:(most_recent_week + forecast_horizon - 1)) |>
    mutate(growth_rate = rnorm(n(), pred, pred_se)) |>
    group_by(id) |>
    arrange(resp_season_week) |>
    mutate(horizon = row_number()) |>
    ungroup() |>
    mutate(resp_season_week = resp_season_week + 1) |>
    select(id, resp_season_week, horizon, growth_rate)

  if (return_scale == "growth") {
    return(sampled_growth)
  }

  growth_to_level_trajectories(sampled_growth, starting_value = most_recent_value)

}


# LOO calibration ==============================================================
#
# For each held-out season, simulate nsamps_cal trajectories from current_ref_week
# using a traj_db built on all OTHER seasons. Compare the mean forecast growth
# trajectory to the held-out realized growth trajectory and return residuals by
# horizon.
#
# Residuals carry ref_season_week so the caller can confirm the conditioning.
# Returns NULL if fewer than 3 historical seasons are available.

run_loo_calibration <- function(historic_df,
                                current_ref_week,
                                fcast_horizon,
                                recent_weeks_touse,
                                resp_week_range,
                                share_groups    = TRUE,
                                ref_week_window = 2,
                                nsamps_cal      = 100) {

  unique_seasons <- unique(historic_df$resp_season_year)
  unique_groups  <- unique(historic_df$target_group)

  if (length(unique_seasons) < 3) return(NULL)

  # Reference weeks to collect residuals from (current ± window, clipped to > 0)
  ref_weeks <- seq(current_ref_week - ref_week_window,
                   current_ref_week + ref_week_window)
  ref_weeks <- ref_weeks[ref_weeks > 0]

  bind_rows(map(unique_groups, function(curr_grp) {
    grp_df <- filter(historic_df, target_group == curr_grp)

    bind_rows(map(unique_seasons, function(held_out) {
      loo_df <- filter(grp_df, resp_season_year != held_out)
      if (n_distinct(loo_df$resp_season_year) < 2) return(NULL)

      # Build LOO trajectory database once per held-out season (reused across ref weeks)
      loo_traj_source <- if (share_groups) {
        filter(historic_df, resp_season_year != held_out)
      } else {
        loo_df
      }

      loo_traj_db <- loo_traj_source |>
        group_by(target_group, resp_season_year) |>
        arrange(resp_season_week) |>
        mutate(get_seasonal_spline_vals(resp_season_week, value)) |>
        ungroup() |>
        select(target_group, resp_season_year, resp_season_week, pred, pred_se)

      # Run LOO simulation for each reference week in the window
      bind_rows(map(ref_weeks, function(ref_wk) {

        sim_data <- grp_df |>
          filter(resp_season_year == held_out,
                 resp_season_week <= ref_wk) |>
          ungroup() |>
          mutate(value             = value + 1,
                 curr_weekly_change = log(lead(value) / value)) |>
          select(resp_season_week, value, curr_weekly_change)

        if (nrow(sim_data) < 3) return(NULL)

        actual_growth <- grp_df |>
          filter(resp_season_year == held_out,
                 resp_season_week >= ref_wk,
                 resp_season_week <= ref_wk + fcast_horizon) |>
          arrange(resp_season_week) |>
          mutate(
            observed_value = value + 1,
            growth_rate    = log(observed_value / lag(observed_value))
          ) |>
          filter(resp_season_week > ref_wk) |>
          mutate(horizon = resp_season_week - ref_wk) |>
          select(horizon, actual_growth = growth_rate)

        if (nrow(actual_growth) == 0) return(NULL)

        tryCatch({
          # Residuals are computed in log-growth space so calibration matches the
          # modeled quantity. We average forecast growth across trajectories per
          # horizon, then compare the held-out realized growth path to that mean.
          calcopycat_fxn(
            curr_data          = sim_data,
            forecast_horizon   = fcast_horizon,
            recent_weeks_touse = recent_weeks_touse,
            nsamps             = nsamps_cal,
            resp_week_range    = resp_week_range,
            db                 = loo_traj_db,
            return_scale       = "growth"
          ) |>
            group_by(horizon) |>
            summarize(forecast_growth = mean(growth_rate), .groups = "drop") |>
            left_join(actual_growth, by = "horizon") |>
            filter(!is.na(actual_growth)) |>
            mutate(residual        = actual_growth - forecast_growth,
                   held_out_year   = held_out,
                   target_group    = curr_grp,
                   ref_season_week = ref_wk) |>
            select(target_group, held_out_year, ref_season_week, horizon, residual)
        }, error = function(e) NULL)
      }))
    }))
  }))
}


# Apply calibration ============================================================
#
# For each trajectory sample, add a random draw from a smoothed growth-residual
# distribution (pooled from all LOO seasons) at the matching horizon. Residuals
# are pooled across ref_season_weeks within ±ref_week_window of current_ref_week
# to broaden the calibration sample.

apply_calibration_to_growth_trajectories <- function(growth_trajectories,
                                                     calibration_residuals,
                                                     target_group_val,
                                                     current_ref_week,
                                                     ref_week_window = 2) {

  if (is.null(calibration_residuals) || nrow(calibration_residuals) == 0) {
    return(growth_trajectories)
  }

  residuals_grp <- calibration_residuals |>
    filter(target_group == target_group_val,
           abs(ref_season_week - current_ref_week) <= ref_week_window)

  if (nrow(residuals_grp) == 0) return(growth_trajectories)

  # Center residuals per horizon so calibration changes spread while preserving
  # the model's mean growth trajectory.
  residual_pools <- residuals_grp |>
    group_by(horizon) |>
    mutate(residual = residual - mean(residual)) |>
    summarize(pool = list(residual), .groups = "drop")

  growth_trajectories |>
    left_join(residual_pools, by = "horizon") |>
    mutate(
      noise       = map_dbl(pool, ~ sample_smoothed_residual(.x)),
      growth_rate = growth_rate + noise
    ) |>
    select(-pool, -noise)

}


# Main entry point =============================================================

fit_process_calcopycat <- function(df,
                                    fcast_horizon,
                                    quantiles_needed,
                                    seasonality,
                                    recent_weeks_touse = 12,
                                    nsamps             = 1000,
                                    resp_week_range    = 0,
                                    share_groups       = TRUE,
                                    ref_week_window    = 2,
                                    nsamps_cal         = 100) {

  # Nested helper (identical to fit_process_copycat) ---------------------------
  get_full_year_df <- function(curr_year, full_df) {
    full_df |>
      filter(resp_season_year == curr_year) |>
      count(target_group) |>
      pull(n) |> min() -> weeks_in_year

    full_df |>
      filter(resp_season_year >= curr_year) |>
      group_by(target_group) |>
      mutate(resp_season_week = seq_along(value)) |>
      filter(resp_season_week <= 62) |>
      mutate(resp_season_year = curr_year) |>
      ungroup() -> df_to_return

    return(df_to_return |> mutate(year_too_short = ifelse(weeks_in_year < 50, TRUE, FALSE)))
  }

  # Seasonality / date setup (identical to fit_process_copycat) ----------------
  if (seasonality == "D" | seasonality == "E") {
    df <- df |> mutate(resp_season_year = MMWRweek(date)$MMWRyear)
  } else {
    df <- df |>
      mutate(year = MMWRweek(date)$MMWRyear,
             week = MMWRweek(date)$MMWRweek) |>
      mutate(resp_season_year = ifelse(week >= 40, year, year - 1)) |>
      select(-year, -week)
  }

  # Season expansion and split -------------------------------------------------
  unique(df$resp_season_year) |>
    map(get_full_year_df, full_df = df) |>
    bind_rows() -> temp

  most_recent_year <- max(df$resp_season_year)

  recent_df  <- temp |> filter(resp_season_year == most_recent_year)
  historic_df <- temp |> filter(resp_season_year != most_recent_year, !year_too_short)

  # Trajectory database --------------------------------------------------------
  traj_db <- historic_df |>
    group_by(target_group, resp_season_year) |>
    arrange(resp_season_week) |>
    mutate(get_seasonal_spline_vals(resp_season_week, value)) |>
    ungroup() |>
    select(target_group, resp_season_year, resp_season_week, pred, pred_se)

  # LOO calibration (stratified by current epidemic week) ----------------------
  current_ref_week <- max(recent_df$resp_season_week)

  cal_residuals <- run_loo_calibration(
    historic_df        = historic_df,
    current_ref_week   = current_ref_week,
    fcast_horizon      = fcast_horizon,
    recent_weeks_touse = recent_weeks_touse,
    resp_week_range    = resp_week_range,
    share_groups       = share_groups,
    ref_week_window    = ref_week_window,
    nsamps_cal         = nsamps_cal
  )

  # Group forecasts ------------------------------------------------------------
  groups         <- unique(recent_df$target_group)
  group_forecasts <- vector("list", length = length(groups))

  for (curr_group in groups) {
    current_data <- recent_df |>
      ungroup() |>
      filter(target_group == curr_group) |>
      mutate(value             = value + 1,
             curr_weekly_change = log(lead(value) / value)) |>
      select(resp_season_week, value, curr_weekly_change)

    starting_value <- tail(current_data$value, 1)

    # Raw growth trajectories
    growth_trajectories <- current_data |>
      calcopycat_fxn(
        db                 = if (share_groups) traj_db else filter(traj_db, target_group == curr_group),
        recent_weeks_touse = recent_weeks_touse,
        nsamps             = nsamps,
        resp_week_range    = resp_week_range,
        forecast_horizon   = fcast_horizon,
        return_scale       = "growth"
      )

    # Residual bootstrap calibration in growth space
    growth_trajectories <- apply_calibration_to_growth_trajectories(
      growth_trajectories   = growth_trajectories,
      calibration_residuals = cal_residuals,
      target_group_val      = curr_group,
      current_ref_week      = current_ref_week,
      ref_week_window       = ref_week_window
    )

    forecast_trajectories <- growth_to_level_trajectories(
      growth_trajectories,
      starting_value = starting_value
    ) |>
      mutate(
        forecast = forecast - 1,
        forecast = pmax(forecast, 0)
      )

    # Quantiles from calibrated trajectories (same pattern as fit_process_copycat)
    cleaned_forecasts_quantiles <- forecast_trajectories |>
      group_by(resp_season_week) |>
      summarize(qs = list(
        value = quantile(pmax(forecast, 0), probs = quantiles_needed)
      )) |>
      mutate(horizon = seq_along(resp_season_week)) |>
      unnest_wider(qs) |>
      gather(quantile, value, -resp_season_week, -horizon) |>
      ungroup() |>
      mutate(
        quantile       = as.numeric(gsub("[\\%,]", "", quantile)) / 100,
        target_group   = curr_group,
        output_type_id = as.numeric(quantile),
        output_type    = "quantile",
        value          = value
      ) |>
      select(horizon, target_group, output_type, output_type_id, value)

    group_forecasts[[match(curr_group, groups)]] <- cleaned_forecasts_quantiles |>
      mutate(output_type_id = as.character(output_type_id))
  }

  bind_rows(group_forecasts) |>
    arrange(target_group, horizon, output_type_id)
}
