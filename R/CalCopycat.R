# CalCopycat =========================================================
#
# Trajectory-level residual bootstrap with anchor-based LOO calibration.
# Matching and calibration operate on continuous weekly history rather than
# respiratory-season splits. Historical candidates are filtered by comparable
# MMWR epiweek and scored by how well their spline-fitted growth histories
# match the current observed growth history.


# Session cache ================================================================

.calcopycat_cache <- new.env(parent = emptyenv())

make_cache_key <- function(prefix, object) {
  serialized <- serialize(object, NULL, version = 2)
  ints <- as.integer(serialized)

  if (length(ints) == 0) {
    return(paste(prefix, "empty", sep = ":"))
  }

  idx <- seq_along(ints)
  hash_a <- sum(((ints + 1L) * (idx %% 65521L)) %% 2147483647L) %% 2147483647L
  hash_b <- sum(((ints + 7L) * ((idx * 131L) %% 65521L)) %% 2147483647L) %% 2147483647L

  paste(prefix, length(ints), hash_a, hash_b, sep = ":")
}


cache_get <- function(key) {
  if (exists(key, envir = .calcopycat_cache, inherits = FALSE)) {
    get(key, envir = .calcopycat_cache, inherits = FALSE)
  } else {
    NULL
  }
}


cache_set <- function(key, value) {
  assign(key, value, envir = .calcopycat_cache)
  value
}


# Helper: GAM spline values for one continuous history =========================

fit_full_history_growth_spline <- function(history_index, value) {
  history_index <- as.numeric(history_index)
  n_points <- length(history_index)

  if (n_points == 0) {
    return(tibble(history_index = numeric(), pred = numeric(), pred_se = numeric()))
  }

  shifted_value <- value + 1
  log_growth <- log(dplyr::lead(shifted_value) / shifted_value)
  log_growth[is.na(log_growth)] <- 0

  if (n_points < 5 || dplyr::n_distinct(history_index) < 5) {
    fallback_sd <- stats::sd(log_growth, na.rm = TRUE)
    fallback_sd <- ifelse(is.na(fallback_sd), 0, fallback_sd)

    return(tibble(
      history_index = history_index,
      pred = log_growth,
      pred_se = rep(fallback_sd, n_points)
    ))
  }

  padding <- min(5, n_points)
  padded_index <- c(
    seq(min(history_index) - padding, min(history_index) - 1),
    history_index,
    seq(max(history_index) + 1, max(history_index) + padding)
  )
  padded_value <- c(
    rep(head(value, 1), padding),
    value,
    rep(tail(value, 1), padding)
  )
  padded_growth <- log(dplyr::lead(padded_value + 1) / (padded_value + 1))
  padded_growth[is.na(padded_growth)] <- 0

  k_val <- max(4, min(length(unique(padded_index)) - 1, floor(n_points / 5)))

  mod <- mgcv::gam(
    padded_growth ~ s(padded_index, k = k_val),
    data = tibble(padded_index = padded_index, padded_growth = padded_growth)
  )

  preds <- predict(
    mod,
    newdata = tibble(padded_index = history_index),
    se.fit = TRUE
  )

  tibble(
    history_index = history_index,
    pred = as.numeric(preds$fit),
    pred_se = as.numeric(preds$se.fit)
  )
}


# Helper: Continuous history prep ==============================================

compute_run_lengths <- function(is_consecutive_from_prev) {
  n_vals <- length(is_consecutive_from_prev)

  past_run <- integer(n_vals)
  if (n_vals >= 2) {
    for (idx in 2:n_vals) {
      past_run[[idx]] <- if (isTRUE(is_consecutive_from_prev[[idx]])) {
        past_run[[idx - 1]] + 1
      } else {
        0
      }
    }
  }

  future_run <- integer(n_vals)
  if (n_vals >= 2) {
    for (idx in seq(n_vals - 1, 1)) {
      future_run[[idx]] <- if (isTRUE(is_consecutive_from_prev[[idx + 1]])) {
        future_run[[idx + 1]] + 1
      } else {
        0
      }
    }
  }

  tibble(
    past_consecutive_length = past_run,
    future_consecutive_length = future_run
  )
}


prepare_continuous_history <- function(df, forecast_horizon) {
  df |>
    mutate(
      mmwr_year = MMWRweek::MMWRweek(date)$MMWRyear,
      mmwr_week = MMWRweek::MMWRweek(date)$MMWRweek
    ) |>
    group_by(target_group) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      history_index = row_number(),
      is_consecutive_from_prev = row_number() == 1 |
        as.numeric(date - lag(date)) == 7,
      curr_weekly_change = log(dplyr::lead(value + 1) / (value + 1))
    ) |>
    group_modify(~ {
      raw_values <- .x$value
      run_lengths <- compute_run_lengths(.x$is_consecutive_from_prev)
      spline_vals <- fit_full_history_growth_spline(.x$history_index, .x$value)

      .x |>
        bind_cols(run_lengths) |>
        left_join(spline_vals, by = "history_index") |>
        mutate(
          recent_level = purrr::map_dbl(
            row_number(),
            ~ mean(raw_values[pmax(1, .x - 3):.x], na.rm = TRUE)
          ),
          is_anchor_candidate = past_consecutive_length >= 1 &
            future_consecutive_length >= forecast_horizon
        )
    }) |>
    ungroup()
}


# Helper: Epiweek utilities ====================================================

compute_epiweek_distance <- function(week_a, week_b, cycle_weeks = 53) {
  week_diff <- abs(week_a - week_b)
  pmin(week_diff, cycle_weeks - week_diff)
}


advance_epiweek <- function(start_week, steps, cycle_weeks = 53) {
  ((start_week - 1 + steps) %% cycle_weeks) + 1
}


# Helper: Reconstruct levels from log-growth trajectories ======================

growth_to_level_trajectories <- function(growth_trajectories, starting_value) {
  growth_trajectories |>
    arrange(id, horizon) |>
    group_by(id) |>
    mutate(
      mult_factor = cumprod(exp(growth_rate)),
      forecast = starting_value * mult_factor
    ) |>
    ungroup() |>
    select(id, resp_season_week, horizon, forecast)
}


# Helper: Level bucket utilities ===============================================

build_level_breaks <- function(values, n_buckets = 3) {
  values <- values[is.finite(values)]

  if (length(values) == 0) return(c(-Inf, Inf))

  probs <- seq(0, 1, length.out = n_buckets + 1)
  cuts <- unique(as.numeric(stats::quantile(values, probs = probs, na.rm = TRUE, type = 8)))

  if (length(cuts) <= 1) return(c(-Inf, Inf))

  c(-Inf, cuts[2:(length(cuts) - 1)], Inf)
}


assign_level_bucket <- function(values, breaks) {
  cut(values, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)
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


# Helper: Current and candidate windows ========================================

get_recent_match_window <- function(curr_data, recent_weeks_touse) {
  anchor_pos <- nrow(curr_data)

  if (anchor_pos < 2) {
    return(tibble(history_index = numeric(), curr_weekly_change = numeric()))
  }

  available_len <- min(curr_data$past_consecutive_length[[anchor_pos]], recent_weeks_touse)

  if (available_len < 1) {
    return(tibble(history_index = numeric(), curr_weekly_change = numeric()))
  }

  curr_data |>
    slice((anchor_pos - available_len):(anchor_pos - 1)) |>
    select(history_index, curr_weekly_change) |>
    filter(!is.na(curr_weekly_change))
}

build_local_anchor_spline_lookup <- function(history_df,
                                             recent_weeks_touse,
                                             forecast_horizon,
                                             local_buffer = 2,
                                             current_mmwr_week = NULL,
                                             resp_week_range = 0) {
  cache_key <- make_cache_key("anchor_lookup", list(
    history = history_df |>
      select(date, target_group, value, history_index, is_anchor_candidate),
    recent_weeks_touse = recent_weeks_touse,
    forecast_horizon = forecast_horizon,
    local_buffer = local_buffer,
    current_mmwr_week = current_mmwr_week,
    resp_week_range = resp_week_range
  ))

  cached_lookup <- cache_get(cache_key)
  if (!is.null(cached_lookup)) return(cached_lookup)

  history_by_group <- split(history_df, history_df$target_group)

  lookup <- purrr::imap_dfr(history_by_group, function(group_history, group_name) {
    min_idx <- min(group_history$history_index)
    max_idx <- max(group_history$history_index)

    candidate_anchors <- group_history |>
      filter(is_anchor_candidate)

    if (!is.null(current_mmwr_week) && resp_week_range > 0) {
      filtered_anchors <- candidate_anchors |>
        filter(compute_epiweek_distance(mmwr_week, current_mmwr_week) <= resp_week_range)

      if (nrow(filtered_anchors) > 0) {
        candidate_anchors <- filtered_anchors
      }
    }

    purrr::map_dfr(candidate_anchors$history_index, function(anchor_idx) {
      support_start <- max(min_idx, anchor_idx - recent_weeks_touse - local_buffer)
      support_end <- min(max_idx, anchor_idx + forecast_horizon + local_buffer)

      support_df <- group_history |>
        filter(history_index >= support_start, history_index <= support_end) |>
        arrange(history_index)

      support_start_date <- min(support_df$date, na.rm = TRUE)
      support_end_date <- max(support_df$date, na.rm = TRUE)
      anchor_date <- group_history |>
        filter(history_index == anchor_idx) |>
        pull(date) |>
        first()
      anchor_mmwr_week <- group_history |>
        filter(history_index == anchor_idx) |>
        pull(mmwr_week) |>
        first()

      local_spline <- fit_full_history_growth_spline(support_df$history_index, support_df$value)

      local_spline |>
        filter(
          history_index >= anchor_idx - recent_weeks_touse,
          history_index < anchor_idx + forecast_horizon
        ) |>
        mutate(
          target_group = group_name,
          anchor_history_index = anchor_idx,
          anchor_date = anchor_date,
          anchor_mmwr_week = anchor_mmwr_week,
          support_start_date = support_start_date,
          support_end_date = support_end_date
        )
    })
  })

  cache_set(cache_key, lookup)
}


filter_anchor_lookup_for_answer_window <- function(anchor_lookup,
                                                   answer_start_date,
                                                   answer_end_date) {
  if (is.null(anchor_lookup) || nrow(anchor_lookup) == 0) {
    return(anchor_lookup)
  }

  anchor_lookup |>
    filter(
      support_end_date < answer_start_date |
        support_start_date > answer_end_date
    )
}


score_candidate_anchors <- function(curr_growth,
                                    current_mmwr_week,
                                    candidate_history,
                                    anchor_lookup,
                                    resp_week_range,
                                    exclude_target_group = NULL,
                                    exclude_anchor_index = NULL) {
  match_length <- length(curr_growth)

  if (match_length == 0) {
    return(tibble())
  }

  candidate_anchors <- candidate_history |>
    filter(
      is_anchor_candidate,
      past_consecutive_length >= match_length
    )

  if (!is.null(exclude_target_group) && !is.null(exclude_anchor_index)) {
    candidate_anchors <- candidate_anchors |>
      filter(!(target_group == exclude_target_group & history_index == exclude_anchor_index))
  }

  if (resp_week_range > 0) {
    filtered_anchors <- candidate_anchors |>
      filter(compute_epiweek_distance(mmwr_week, current_mmwr_week) <= resp_week_range)

    if (nrow(filtered_anchors) > 0) {
      candidate_anchors <- filtered_anchors
    }
  }

  available_anchors <- anchor_lookup |>
    distinct(target_group, anchor_history_index)

  candidate_anchors <- candidate_anchors |>
    semi_join(
      available_anchors,
      by = c("target_group", "history_index" = "anchor_history_index")
    )

  lookup_by_group <- split(anchor_lookup, anchor_lookup$target_group)

  candidate_anchors |>
    transmute(
      target_group,
      anchor_history_index = history_index,
      anchor_mmwr_week = mmwr_week,
      recent_level,
      weight = purrr::map2_dbl(
        target_group,
        anchor_history_index,
        function(group_name, anchor_idx) {
          group_lookup <- lookup_by_group[[group_name]]
          pred_window <- group_lookup |>
            filter(
              anchor_history_index == anchor_idx,
              history_index >= anchor_idx - match_length,
              history_index < anchor_idx
            ) |>
            arrange(history_index) |>
            pull(pred)

          if (length(pred_window) != match_length || anyNA(pred_window)) {
            return(NA_real_)
          }

          mean((pred_window - curr_growth)^2)
        }
      )
    ) |>
    filter(is.finite(weight))
}


sample_anchor_growth_paths <- function(trajectories,
                                       anchor_lookup,
                                       forecast_horizon,
                                       current_mmwr_week) {
  purrr::map2_dfr(
    trajectories$target_group,
    trajectories$anchor_history_index,
    function(group_name, anchor_idx) {
      current_id <- trajectories |>
        filter(target_group == group_name, anchor_history_index == anchor_idx) |>
        slice(1) |>
        pull(id)

      anchor_lookup |>
        filter(
          target_group == group_name,
          anchor_history_index == anchor_idx,
          history_index >= anchor_idx,
          history_index < anchor_idx + forecast_horizon
        ) |>
        arrange(history_index) |>
        mutate(
          id = current_id,
          growth_rate = rnorm(
            n(),
            mean = pred,
            sd = ifelse(is.na(pred_se), 0, pmax(pred_se, 0))
          ),
          horizon = row_number(),
          resp_season_week = advance_epiweek(current_mmwr_week, horizon)
        ) |>
        select(id, resp_season_week, horizon, growth_rate)
    }
  )
}


# Copycat sampler (no Poisson noise) ==========================================

calcopycat_fxn <- function(curr_data,
                           forecast_horizon = 5,
                           recent_weeks_touse = 12,
                           nsamps = 1000,
                           resp_week_range = 0,
                           db = traj_db,
                           db_history = NULL,
                           db_local_lookup = NULL,
                           exclude_target_group = NULL,
                           exclude_anchor_index = NULL,
                           local_buffer = 2,
                           return_scale = c("level", "growth")) {

  return_scale <- match.arg(return_scale)
  if (is.null(db_history)) db_history <- db

  curr_data <- curr_data |>
    arrange(history_index)

  most_recent_value <- tail(curr_data$value, 1)
  current_mmwr_week <- tail(curr_data$mmwr_week, 1)

  cleaned_data <- get_recent_match_window(curr_data, recent_weeks_touse)

  if (nrow(cleaned_data) == 0) {
    stop("CalCopycat requires at least one consecutive observed growth rate to match on.")
  }

  if (is.null(db_local_lookup)) {
    db_local_lookup <- build_local_anchor_spline_lookup(
      history_df = db_history,
      recent_weeks_touse = recent_weeks_touse,
      forecast_horizon = forecast_horizon,
      local_buffer = local_buffer,
      current_mmwr_week = current_mmwr_week,
      resp_week_range = resp_week_range
    )
  }

  traj_temp <- score_candidate_anchors(
    curr_growth = cleaned_data$curr_weekly_change,
    current_mmwr_week = current_mmwr_week,
    candidate_history = db_history,
    anchor_lookup = db_local_lookup,
    resp_week_range = resp_week_range,
    exclude_target_group = exclude_target_group,
    exclude_anchor_index = exclude_anchor_index
  )

  if (nrow(traj_temp) == 0) {
    stop("No eligible historical anchors found for CalCopycat matching.")
  }

  min_allowed_weight <- 0.02

  trajectories <- traj_temp |>
    mutate(weight = ifelse(weight < min_allowed_weight, min_allowed_weight, weight)) |>
    arrange(desc(weight)) |>
    sample_n(size = nsamps, replace = TRUE, weight = 1 / weight^2) |>
    mutate(id = seq_along(weight)) |>
    select(id, target_group, anchor_history_index, anchor_mmwr_week)

  sampled_growth <- trajectories |>
    group_by(id) |>
    group_split() |>
    purrr::map_dfr(function(traj_row) {
      sample_anchor_growth_paths(
        trajectories = traj_row,
        anchor_lookup = db_local_lookup,
        forecast_horizon = forecast_horizon,
        current_mmwr_week = current_mmwr_week
      )
    })

  if (return_scale == "growth") {
    return(sampled_growth)
  }

  growth_to_level_trajectories(sampled_growth, starting_value = most_recent_value)
}


# LOO calibration ==============================================================

run_anchor_loo_calibration <- function(historic_df,
                                       anchor_lookup = NULL,
                                       current_ref_week,
                                       fcast_horizon,
                                       recent_weeks_touse,
                                       resp_week_range,
                                       share_groups = TRUE,
                                       ref_week_window = 2,
                                       nsamps_cal = 100,
                                       local_buffer = 2) {
  cache_key <- make_cache_key("loo_calibration", list(
    history = historic_df |>
      select(date, target_group, value, mmwr_week, history_index, recent_level, is_anchor_candidate),
    current_ref_week = current_ref_week,
    fcast_horizon = fcast_horizon,
    recent_weeks_touse = recent_weeks_touse,
    resp_week_range = resp_week_range,
    share_groups = share_groups,
    ref_week_window = ref_week_window,
    nsamps_cal = nsamps_cal,
    local_buffer = local_buffer
  ))

  cached_residuals <- cache_get(cache_key)
  if (!is.null(cached_residuals)) return(cached_residuals)

  loo_lookup <- if (is.null(anchor_lookup)) {
    build_local_anchor_spline_lookup(
      history_df = historic_df,
      recent_weeks_touse = recent_weeks_touse,
      forecast_horizon = fcast_horizon,
      local_buffer = local_buffer,
      current_mmwr_week = current_ref_week,
      resp_week_range = resp_week_range + ref_week_window
    )
  } else {
    anchor_lookup
  }

  unique_groups <- unique(historic_df$target_group)

  calibration_residuals <- bind_rows(purrr::map(unique_groups, function(curr_grp) {
    grp_df <- filter(historic_df, target_group == curr_grp)

    eval_anchors <- grp_df |>
      filter(
        is_anchor_candidate,
        compute_epiweek_distance(mmwr_week, current_ref_week) <= ref_week_window
      )

    if (nrow(eval_anchors) == 0) return(NULL)

    bind_rows(purrr::map(eval_anchors$history_index, function(anchor_idx) {
      anchor_row <- eval_anchors |>
        filter(history_index == anchor_idx) |>
        slice(1)

      anchor_date <- anchor_row$date[[1]]

      actuals <- grp_df |>
        filter(
          date > anchor_date
        ) |>
        arrange(date) |>
        slice_head(n = fcast_horizon) |>
        mutate(horizon = row_number()) |>
        select(horizon, actual = value)

      if (nrow(actuals) == 0) return(NULL)

      answer_end_date <- grp_df |>
        filter(date > anchor_date) |>
        arrange(date) |>
        slice_head(n = fcast_horizon) |>
        summarize(max_date = max(date, na.rm = TRUE)) |>
        pull(max_date)

      if (length(answer_end_date) != 1 || is.na(answer_end_date)) return(NULL)

      filtered_lookup <- filter_anchor_lookup_for_answer_window(
        anchor_lookup = if (share_groups) loo_lookup else filter(loo_lookup, target_group == curr_grp),
        answer_start_date = anchor_date + 1,
        answer_end_date = answer_end_date
      )

      if (nrow(filtered_lookup) == 0) return(NULL)

      sim_data <- historic_df |>
        filter(target_group == curr_grp) |>
        filter(date <= anchor_date) |>
        arrange(history_index) |>
        mutate(value = value + 1) |>
        select(
          target_group,
          history_index,
          mmwr_week,
          value,
          curr_weekly_change,
          past_consecutive_length
        )

      if (nrow(sim_data) < 2) return(NULL)

      tryCatch({
        calcopycat_fxn(
          curr_data = sim_data,
          forecast_horizon = fcast_horizon,
          recent_weeks_touse = recent_weeks_touse,
          nsamps = nsamps_cal,
          resp_week_range = resp_week_range,
          db_history = if (share_groups) historic_df else grp_df,
          db_local_lookup = filtered_lookup,
          exclude_target_group = curr_grp,
          exclude_anchor_index = anchor_idx,
          local_buffer = local_buffer,
          return_scale = "level"
        ) |>
          mutate(
            forecast = pmax(forecast - 1, 0)
          ) |>
          group_by(horizon) |>
          summarize(forecast = mean(forecast), .groups = "drop") |>
          left_join(actuals, by = "horizon") |>
          filter(!is.na(actual)) |>
          mutate(
            residual = actual - forecast,
            held_out_year = NA_integer_,
            target_group = curr_grp,
            ref_season_week = anchor_row$mmwr_week[[1]],
            recent_level = anchor_row$recent_level[[1]]
          ) |>
          select(target_group, held_out_year, ref_season_week, horizon, residual, recent_level)
      }, error = function(e) NULL)
    }))
  }))

  cache_set(cache_key, calibration_residuals)
}


# Apply calibration ============================================================

apply_calibration_to_growth_trajectories <- function(growth_trajectories,
                                                     calibration_residuals,
                                                     target_group_val,
                                                     current_ref_week,
                                                     ref_week_window = 2) {

  if (is.null(calibration_residuals) || nrow(calibration_residuals) == 0) {
    return(growth_trajectories)
  }

  residuals_grp <- calibration_residuals |>
    filter(
      target_group == target_group_val,
      compute_epiweek_distance(ref_season_week, current_ref_week) <= ref_week_window
    )

  if (nrow(residuals_grp) == 0) return(growth_trajectories)

  residual_pools <- residuals_grp |>
    group_by(horizon) |>
    mutate(residual = residual - mean(residual)) |>
    summarize(pool = list(residual), .groups = "drop")

  growth_trajectories |>
    left_join(residual_pools, by = "horizon") |>
    mutate(
      noise = purrr::map_dbl(pool, ~ sample_smoothed_residual(.x)),
      growth_rate = growth_rate + noise
    ) |>
    select(-pool, -noise)
}


apply_calibration_to_level_trajectories <- function(level_trajectories,
                                                    calibration_residuals,
                                                    target_group_val,
                                                    current_ref_week,
                                                    current_recent_level,
                                                    ref_week_window = 2,
                                                    level_bucket_count = 3,
                                                    min_bucket_n = 20) {

  if (is.null(calibration_residuals) || nrow(calibration_residuals) == 0) {
    return(level_trajectories)
  }

  residuals_grp <- calibration_residuals |>
    filter(
      target_group == target_group_val,
      compute_epiweek_distance(ref_season_week, current_ref_week) <= ref_week_window
    )

  if (nrow(residuals_grp) == 0) return(level_trajectories)

  level_breaks <- build_level_breaks(residuals_grp$recent_level, n_buckets = level_bucket_count)

  residuals_grp <- residuals_grp |>
    mutate(level_bucket = assign_level_bucket(recent_level, level_breaks))

  current_bucket <- assign_level_bucket(current_recent_level, level_breaks)

  residuals_bucket <- residuals_grp |>
    filter(level_bucket == current_bucket)

  bucket_counts <- residuals_bucket |>
    count(horizon, name = "n_bucket")

  fallback_needed <- nrow(residuals_bucket) == 0 ||
    any(bucket_counts$n_bucket < min_bucket_n) ||
    n_distinct(residuals_bucket$horizon) < n_distinct(level_trajectories$horizon)

  residuals_use <- if (fallback_needed) residuals_grp else residuals_bucket

  residual_pools <- residuals_use |>
    group_by(horizon) |>
    mutate(residual = residual - mean(residual)) |>
    summarize(pool = list(residual), .groups = "drop")

  level_trajectories |>
    left_join(residual_pools, by = "horizon") |>
    mutate(
      noise = purrr::map_dbl(pool, ~ sample_smoothed_residual(.x)),
      forecast = pmax(forecast + noise, 0)
    ) |>
    select(-pool, -noise)
}


# Main entry point =============================================================

fit_process_calcopycat <- function(df,
                                   fcast_horizon,
                                   quantiles_needed,
                                   recent_weeks_touse = 12,
                                   nsamps = 1000,
                                   resp_week_range = 0,
                                   share_groups = TRUE,
                                   ref_week_window = 2,
                                   nsamps_cal = 100,
                                   level_bucket_count = 3,
                                   min_bucket_n = 20,
                                   local_buffer = 2) {

  history_df <- prepare_continuous_history(
    df = df,
    forecast_horizon = fcast_horizon
  )

  current_ref_week <- history_df |>
    arrange(date) |>
    tail(1) |>
    pull(mmwr_week)

  anchor_lookup <- build_local_anchor_spline_lookup(
    history_df = history_df,
    recent_weeks_touse = recent_weeks_touse,
    forecast_horizon = fcast_horizon,
    local_buffer = local_buffer,
    current_mmwr_week = current_ref_week,
    resp_week_range = resp_week_range
  )

  loo_anchor_lookup <- build_local_anchor_spline_lookup(
    history_df = history_df,
    recent_weeks_touse = recent_weeks_touse,
    forecast_horizon = fcast_horizon,
    local_buffer = local_buffer,
    current_mmwr_week = current_ref_week,
    resp_week_range = resp_week_range + ref_week_window
  )

  cal_residuals <- run_anchor_loo_calibration(
    historic_df = history_df,
    anchor_lookup = loo_anchor_lookup,
    current_ref_week = current_ref_week,
    fcast_horizon = fcast_horizon,
    recent_weeks_touse = recent_weeks_touse,
    resp_week_range = resp_week_range,
    share_groups = share_groups,
    ref_week_window = ref_week_window,
    nsamps_cal = nsamps_cal,
    local_buffer = local_buffer
  )

  groups <- unique(history_df$target_group)
  group_forecasts <- vector("list", length = length(groups))

  for (curr_group in groups) {
    current_data <- history_df |>
      filter(target_group == curr_group) |>
      arrange(history_index) |>
      mutate(value = value + 1)

    current_recent_level <- mean(
      tail(current_data$value - 1, min(4, nrow(current_data))),
      na.rm = TRUE
    )

    current_group_ref_week <- tail(current_data$mmwr_week, 1)

    forecast_trajectories <- calcopycat_fxn(
      curr_data = current_data,
      db_history = if (share_groups) history_df else filter(history_df, target_group == curr_group),
      db_local_lookup = if (share_groups) anchor_lookup else filter(anchor_lookup, target_group == curr_group),
      recent_weeks_touse = recent_weeks_touse,
      nsamps = nsamps,
      resp_week_range = resp_week_range,
      forecast_horizon = fcast_horizon,
      local_buffer = local_buffer,
      return_scale = "level"
    ) |>
      mutate(
        forecast = forecast - 1,
        forecast = pmax(forecast, 0)
      )

    forecast_trajectories <- apply_calibration_to_level_trajectories(
      level_trajectories = forecast_trajectories,
      calibration_residuals = cal_residuals,
      target_group_val = curr_group,
      current_ref_week = current_group_ref_week,
      current_recent_level = current_recent_level,
      ref_week_window = ref_week_window,
      level_bucket_count = level_bucket_count,
      min_bucket_n = min_bucket_n
    )

    cleaned_forecasts_quantiles <- forecast_trajectories |>
      group_by(resp_season_week) |>
      summarize(qs = list(
        value = quantile(pmax(forecast, 0), probs = quantiles_needed)
      )) |>
      mutate(horizon = seq_along(resp_season_week)) |>
      tidyr::unnest_wider(qs) |>
      tidyr::gather(quantile, value, -resp_season_week, -horizon) |>
      ungroup() |>
      mutate(
        quantile = as.numeric(gsub("[\\%,]", "", quantile)) / 100,
        target_group = curr_group,
        output_type_id = as.numeric(quantile),
        output_type = "quantile",
        value = value
      ) |>
      select(horizon, target_group, output_type, output_type_id, value)

    group_forecasts[[match(curr_group, groups)]] <- cleaned_forecasts_quantiles |>
      mutate(output_type_id = as.character(output_type_id))
  }

  bind_rows(group_forecasts) |>
    arrange(target_group, horizon, output_type_id)
}
