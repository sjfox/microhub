inla.setOption(inla.mode = "classic")

# Wrangle data for INLA model ==================================================

wrangle_inla_population <- function(
  dataframe,
  forecast_date,
  data_to_drop,
  forecast_horizons,
  pop_table # new argument
) {
  forecast_date <- as.Date(forecast_date)

  # Preprocess the data
  data_preprocessed <- dataframe |>
    rename(
      epiweek = week,
      count = value
    ) |>
    filter(year > 2021)

  # Add population column
  data_with_population <- add_population_column(
    pop_table,
    data_preprocessed
  )

  # Determine configuration based on selected data to drop option
  config <- switch(
    data_to_drop,
    "0 weeks" = list(days_before = 0, weeks_ahead = 4),
    "1 week" = list(days_before = 4, weeks_ahead = 5),
    "2 week" = list(days_before = 11, weeks_ahead = 6),
    stop("Invalid data_to_drop option")
  )

  days_before <- config$days_before
  base_weeks_ahead <- config$weeks_ahead

  # Adjust weeks_ahead based on forecast horizons
  weeks_ahead <- base_weeks_ahead + (as.numeric(forecast_horizons) - 4)

  # Final data wrangling
  wrangled_data <- data_with_population |>
    mutate(date = as.Date(date)) |>
    filter(date < ymd(forecast_date) - days_before)

  fit_df <- prep_fit_data_population(wrangled_data, weeks_ahead)

  return(fit_df)
}

wrangle_inla_no_population <- function(
  dataframe,
  forecast_date,
  data_to_drop,
  forecast_horizons
) {
  forecast_date <- as.Date(forecast_date)

  # Preprocess the data
  data_preprocessed <- dataframe |>
    rename(
      epiweek = week,
      count = value
    ) |>
    filter(year > 2021)

  # Determine configuration based on selected data to drop option
  config <- switch(
    data_to_drop,
    "0 weeks" = list(days_before = 0, weeks_ahead = 4),
    "1 week" = list(days_before = 4, weeks_ahead = 5),
    "2 week" = list(days_before = 11, weeks_ahead = 6),
    stop("Invalid data_to_drop option")
  )

  days_before <- config$days_before
  base_weeks_ahead <- config$weeks_ahead

  # Adjust weeks_ahead based on forecast horizons
  weeks_ahead <- base_weeks_ahead + (as.numeric(forecast_horizons) - 4)

  # Final data wrangling
  wrangled_data <- data_preprocessed |>
    mutate(date = as.Date(date)) |>
    filter(date < ymd(forecast_date) - days_before)

  fit_df <- prep_fit_data_no_population(wrangled_data, weeks_ahead)

  return(fit_df)
}
# Fit and process INLA =========================================================

fit_process_inla_offset_aggregate <- function(
  fit_df,
  forecast_date,
  ar_order,
  rw_order,
  seasonal_smoothness,
  forecast_uncertainty_parameter,
  q = c(0.025, 0.25, 0.5, 0.75, 0.975),
  joint = TRUE
) {
  forecast_date <- as.Date(forecast_date)

  fit_df <- fit_df |>
    filter(target_group != "Overall")

  # Fit the current model
  fit <- fit_current_model1(
    fit_df,
    forecast_date,
    ar_order,
    rw_order,
    seasonal_smoothness,
    forecast_uncertainty_parameter,
    q,
    joint
  )

  # Sample national-level predictions
  nat_samps <- sample_national(fit_df, fit, forecast_date)

  # Sample count predictions
  pred_samples <- sample_count_predictions(fit_df, fit)

  # Summarize quantiles
  forecast_quantiles <- summarize_quantiles_aggregate(
    pred_samples,
    nat_samps,
    forecast_date,
    q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  )

  return(forecast_quantiles)
}

fit_process_inla_no_offset_aggregate <- function(
  fit_df,
  forecast_date,
  ar_order,
  rw_order,
  seasonal_smoothness,
  forecast_uncertainty_parameter,
  q = c(0.025, 0.25, 0.5, 0.75, 0.975),
  joint = TRUE
) {
  forecast_date <- as.Date(forecast_date)

  fit_df <- fit_df |>
    filter(target_group != "Overall")

  # Fit the current model
  fit <- fit_current_model1_no_offset(
    fit_df,
    forecast_date,
    ar_order,
    rw_order,
    seasonal_smoothness,
    forecast_uncertainty_parameter,
    q,
    joint
  )

  # Sample national-level predictions
  nat_samps <- sample_national_no_offset(fit_df, fit, forecast_date)

  # Sample count predictions
  pred_samples <- sample_count_predictions_no_offset(fit_df, fit)

  # Summarize quantiles
  forecast_quantiles <- summarize_quantiles_aggregate(
    pred_samples,
    nat_samps,
    forecast_date,
    q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  )

  return(forecast_quantiles)
}

fit_process_inla_offset_single_target <- function(
  fit_df,
  forecast_date,
  ar_order,
  rw_order,
  seasonal_smoothness,
  forecast_uncertainty_parameter,
  q = c(0.025, 0.25, 0.5, 0.75, 0.975),
  joint = TRUE
) {
  forecast_date <- as.Date(forecast_date)

  # Fit the current model
  fit <- fit_current_model1(
    fit_df,
    forecast_date,
    ar_order,
    rw_order,
    seasonal_smoothness,
    forecast_uncertainty_parameter,
    q,
    joint
  )

  # Sample count predictions
  pred_samples <- sample_count_predictions(fit_df, fit)

  # Summarize quantiles
  forecast_quantiles <- summarize_quantiles_single_target(
    pred_samples,
    nat_samps,
    forecast_date,
    q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  )

  return(forecast_quantiles)
}

fit_process_inla_no_offset_single_target <- function(
  fit_df,
  forecast_date,
  ar_order,
  rw_order,
  seasonal_smoothness,
  forecast_uncertainty_parameter,
  q = c(0.025, 0.25, 0.5, 0.75, 0.975),
  joint = TRUE
) {
  forecast_date <- as.Date(forecast_date)

  # Fit the current model
  fit <- fit_current_model1_no_offset(
    fit_df,
    forecast_date,
    ar_order,
    rw_order,
    seasonal_smoothness,
    forecast_uncertainty_parameter,
    q,
    joint
  )

  # Sample count predictions
  pred_samples <- sample_count_predictions_no_offset(fit_df, fit)

  # Summarize quantiles
  forecast_quantiles <- summarize_quantiles_single_target(
    pred_samples,
    nat_samps,
    forecast_date,
    q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  )

  return(forecast_quantiles)
}


# INLA helper functions ========================================================

add_population_column <- function(pop_table, data_frame) {
  # Add a year column based on date
  data_frame <- data_frame |>
    mutate(year = year(date))

  # If `year` is in population table, do year-based join
  if ("year" %in% colnames(pop_table)) {
    min_year <- min(pop_table$year, na.rm = TRUE)
    max_year <- max(pop_table$year, na.rm = TRUE)

    data_frame <- data_frame |>
      mutate(year_capped = case_when(
        year < min_year ~ min_year,
        year > max_year ~ max_year,
        TRUE ~ year
      )) |>
      left_join(
        pop_table,
        by = c("year_capped" = "year", "target_group")
      ) |>
      select(-year_capped)
  } else {
    # No year in population table → join by age group only
    data_frame <- data_frame |>
      left_join(
        pop_table,
        by = "target_group"
      )
  }

  return(data_frame)
}

########################## this is the version for population data
prep_fit_data_population <- function(input_data, weeks_ahead = weeks_ahead) {
  input_data$date <- as.Date(input_data$date)

  ret <- input_data |>
    group_by(date) |>
    mutate(t = cur_group_id(), .after = date) |> # add a time counter starting from 1 for earliest week
    ungroup() |>
    mutate(
      snum = as.numeric(fct_inorder(target_group)), # INLA needs groups as ints starting from 1, so add numeric state code
      ex_lam = population
    )

  # make a dataframe to hold group info for forecasting
  pred_df <- expand_grid(
    tibble(
      date = duration(1:weeks_ahead, "week") + max(ret$date),
      t = 1:weeks_ahead + max(ret$t),
      epiweek = epiweek(date)
    ),
    distinct(ret, target_group, snum, population) # makes pairs of new times X each state
  ) |>
    left_join(
      distinct(ret, target_group, epiweek, ex_lam),
      by = join_by(epiweek, target_group)
    ) # go and find `ex_lam` values for each state and epiweek

  bind_rows(ret, pred_df) |> # add to data for counts to be NAs
    arrange(t)
}

################################## this is the version without population data
prep_fit_data_no_population <- function(input_data, weeks_ahead = weeks_ahead) {
  input_data$date <- as.Date(input_data$date)

  ret <- input_data |>
    group_by(date) |>
    mutate(t = cur_group_id(), .after = date) |> # add a time counter starting from 1 for earliest week
    ungroup() |>
    mutate(
      snum = as.numeric(fct_inorder(target_group)) # INLA needs groups as ints starting from 1, so add numeric state code
    )

  # make a dataframe to hold group info for forecasting
  pred_df <- expand_grid(
    tibble(
      date = duration(1:weeks_ahead, "week") + max(ret$date),
      t = 1:weeks_ahead + max(ret$t),
      epiweek = epiweek(date)
    ),
    distinct(ret, target_group, snum) # makes pairs of new times X each state
  ) |>
    left_join(
      distinct(ret, target_group, epiweek),
      by = join_by(epiweek, target_group)
    ) # go and find `ex_lam` values for each state and epiweek

  bind_rows(ret, pred_df) |> # add to data for counts to be NAs
    arrange(t)
}
######################## this is the version with the offset
fit_current_model1 <- function(
  fit_df,
  forecast_date,
  ar_order,
  rw_order,
  seasonal_smoothness,
  forecast_uncertainty_parameter,
  q = c(0.025, 0.25, 0.5, 0.75, 0.975),
  joint = TRUE
) {
  # Set hyperparameters for seasonal effect
  hyper_epwk <- switch(
    seasonal_smoothness,
    "default" = list(theta = list(prior = "pc.prec", param = c(0.5, 0.01))),
    "less" = list(theta = list(prior = "pc.prec", param = c(0.25, 0.01))),
    "more" = list(theta = list(prior = "pc.prec", param = c(1, 0.01))),
    stop("Invalid selection for seasonal_smoothness")
  )

  # Set hyperparameters for weekly effect
  # hyper_wk <- switch(weekly_effect,
  # "default" = list(theta=list(prior="pc.prec", param=c(1, 0.01))),
  # "less" = list(theta=list(prior="pc.prec", param=c(0.5, 0.01))),
  # "more" = list(theta=list(prior="pc.prec", param=c(2, 0.01))),
  # stop("Invalid selection for weekly effect"))

  hyper_wk <- switch(
    forecast_uncertainty_parameter,
    "default" = list(theta = list(prior = "pc.prec", param = c(1, 0.01))),
    "small" = list(theta = list(prior = "pc.prec", param = c(0.2, 0.01))),
    "tiny" = list(theta = list(prior = "pc.prec", param = c(0.05, 0.01))),
    stop("Invalid selection for forecast_uncertainty_parameter")
  )

  # Create the rw_mod part of the formula dynamically
  rw_mod <- switch(
    rw_order,
    "1" = "f(epiweek, model='rw1', cyclic=TRUE, hyper=hyper_epwk)",
    "2" = "f(epiweek, model='rw2', cyclic=TRUE, hyper=hyper_epwk)", ### this is default
    stop("Invalid selection for RW order")
  )

  # Create the complete formula as a string
  formula_string <- paste(
    "count ~ 1 +",
    rw_mod,
    "+ f(t, model='ar', order=",
    ar_order,
    ", group=snum, hyper=hyper_wk, control.group=list(model='exchangeable'))"
  )

  # Convert the formula string to a formula object
  mod <- as.formula(formula_string)

  pred_idx <- which(fit_df$date >= forecast_date)

  fit <- inla(
    mod,
    family = "poisson",
    data = fit_df,
    E = fit_df$ex_lam,
    quantiles = q,
    selection = if (!joint) NULL else list(Predictor = pred_idx),
    control.compute = list(
      dic = FALSE,
      mlik = FALSE,
      return.marginals.predictor = TRUE
    ),
    control.predictor = list(link = 1, compute = TRUE)
  )
  return(fit)
}

sample_count_predictions <- function(
  fit_df,
  fit,
  nsamp = 10000
) {
  samp_counts <- map2_dfr(
    fit$marginals.fitted.values,
    fit_df$ex_lam,
    \(m, ex) {
      msamp <- pmax(0, inla.rmarginal(nsamp, m)) # sampling sometimes produces very small neg. numbers
      ct_samp <- rpois(nsamp, msamp * ex)
      tibble_row(count_samp = list(ct_samp))
    }
  )

  return(bind_cols(fit_df, samp_counts))
}


sample_national <- function(fit_df, fit, forecast_date, nsamp = 10000) {
  state_info <- distinct(fit_df, target_group, ex_lam)
  nstate <- nrow(state_info)

  ret_df <- fit_df |>
    filter(date >= forecast_date) |>
    # filter(date >= temp_date) |>
    group_by(date, t, epiweek) |>
    summarise(population = sum(population), .groups = "drop")

  jsamp_fvals <- exp(inla.rjmarginal(nsamp, fit$selection)$samples)
  # ex_lam <- filter(fit_df, date >= forecast_date)$ex_lam
  ex_lam <- filter(fit_df, date >= forecast_date)$ex_lam


  tslice <- map(1:nrow(ret_df), ~ nstate * (.x - 1) + 1:nstate) # produce sequence [1:nstate, nstate+1:2nstate, ...]

  imap_dfr(tslice, \(idx, t) {
    nat_sum_per_t <- map_dbl(1:nsamp, \(samp) {
      lambda <- jsamp_fvals[idx, samp] * ex_lam
      samp <- rpois(nstate, lambda)
      sum(samp)
    })
    # qs <- quantile(nat_sum_per_t, q)
    # names(qs) <- str_c("q", names(qs))
    tibble_row(target_group = "Overall", count_samp = list(nat_sum_per_t))
  }) |>
    bind_cols(ret_df) |>
    select(date:epiweek, target_group, population, count_samp)
}

################################### this is the no offset version
fit_current_model1_no_offset <- function(
  fit_df,
  forecast_date,
  ar_order,
  rw_order,
  seasonal_smoothness,
  forecast_uncertainty_parameter,
  q = c(0.025, 0.25, 0.5, 0.75, 0.975),
  joint = TRUE
) {
  hyper_epwk <- switch(
    seasonal_smoothness,
    "default" = list(theta = list(prior = "pc.prec", param = c(0.5, 0.01))),
    "less"    = list(theta = list(prior = "pc.prec", param = c(0.25, 0.01))),
    "more"    = list(theta = list(prior = "pc.prec", param = c(1, 0.01))),
    stop("Invalid seasonal_smoothness")
  )

  hyper_wk <- switch(
    forecast_uncertainty_parameter,
    "default" = list(theta = list(prior = "pc.prec", param = c(1, 0.01))),
    "small"   = list(theta = list(prior = "pc.prec", param = c(0.2, 0.01))),
    "tiny"    = list(theta = list(prior = "pc.prec", param = c(0.05, 0.01))),
    stop("Invalid forecast_uncertainty_parameter")
  )

  rw_mod <- switch(
    rw_order,
    "1" = "f(epiweek, model='rw1', cyclic=TRUE, hyper=hyper_epwk)",
    "2" = "f(epiweek, model='rw2', cyclic=TRUE, hyper=hyper_epwk)",
    stop("Invalid RW order")
  )

  formula_string <- paste(
    "count ~ 1 +",
    rw_mod,
    "+ f(t, model='ar', order=",
    ar_order,
    ", group=snum, hyper=hyper_wk, control.group=list(model='exchangeable'))"
  )

  mod <- as.formula(formula_string)
  pred_idx <- which(fit_df$date >= forecast_date)

  fit <- inla(
    mod,
    family = "poisson",
    data = fit_df,
    quantiles = q,
    selection = if (!joint) NULL else list(Predictor = pred_idx),
    control.compute = list(
      dic = FALSE,
      mlik = FALSE,
      return.marginals.predictor = TRUE
    ),
    control.predictor = list(link = 1, compute = TRUE)
  )

  return(fit)
}

sample_count_predictions_no_offset <- function(fit_df, fit, nsamp = 10000) {
  samp_counts <- map(fit$marginals.fitted.values, \(m) {
    msamp <- pmax(0, inla.rmarginal(nsamp, m))
    rpois(nsamp, msamp)
  })

  count_df <- tibble(count_samp = samp_counts)
  return(bind_cols(fit_df, count_df))
}

sample_national_no_offset <- function(fit_df, fit, forecast_date, nsamp = 10000) {
  state_info <- distinct(fit_df, target_group)
  nstate <- nrow(state_info)

  ret_df <- fit_df |>
    filter(date >= forecast_date) |>
    group_by(date, t, epiweek) |>
    summarise(.groups = "drop")

  jsamp_fvals <- exp(inla.rjmarginal(nsamp, fit$selection)$samples)

  tslice <- map(1:nrow(ret_df), ~ nstate * (.x - 1) + 1:nstate)

  imap_dfr(tslice, \(idx, t) {
    nat_sum_per_t <- map_dbl(1:nsamp, \(samp) {
      lambda <- jsamp_fvals[idx, samp]
      sum(rpois(nstate, lambda))
    })
    tibble_row(target_group = "Overall", count_samp = list(nat_sum_per_t))
  }) |>
    bind_cols(ret_df) |>
    select(date, t, epiweek, target_group, count_samp)
}

######################################### works for aggregate

summarize_quantiles_aggregate <- function(
  pred_samples,
  nat_samps,
  forecast_date,
  q
) {
  pred_samples |>
    filter(date >= forecast_date) |>
    bind_rows(nat_samps) |>
    unnest(count_samp) |>
    group_by(date, target_group) |>
    summarize(
      qs = list(
        value = quantile(count_samp, probs = q)
      ),
      .groups = "drop"
    ) |>
    unnest_wider(qs) |>
    pivot_longer(contains("%"), names_to = "quantile") |>
    mutate(quantile = as.numeric(gsub("[\\%,]", "", quantile)) / 100) |>
    mutate(
      horizon = as.numeric(as.factor(date)) - 1,
      reference_date = as.Date(forecast_date) + 3,
      # Use lubridate syntax to add horizon to forecast_date
      target_end_date = (as.Date(forecast_date) %m+% weeks(horizon)) + 3,
      # Below gave error (non-numeric argument to binary operator)
      # target_end_date = forecast_date + horizon * 7,
      output_type_id = as.numeric(quantile),
      output_type = "quantile",
      value = round(value)
    ) |>
    arrange(target_group, horizon, quantile) |>
    dplyr::select(
      reference_date,
      horizon,
      target_end_date,
      target_group,
      output_type,
      output_type_id,
      value
    )
}

####################### works for single target
summarize_quantiles_single_target <- function(
  pred_samples,
  nat_samps,
  forecast_date,
  q
) {
  pred_samples |>
    filter(date >= forecast_date) |>
    # bind_rows(nat_samps) |>
    unnest(count_samp) |>
    group_by(date, target_group) |>
    summarize(
      qs = list(
        value = quantile(count_samp, probs = q)
      ),
      .groups = "drop"
    ) |>
    unnest_wider(qs) |>
    pivot_longer(contains("%"), names_to = "quantile") |>
    mutate(quantile = as.numeric(gsub("[\\%,]", "", quantile)) / 100) |>
    mutate(
      horizon = as.numeric(as.factor(date)) - 1,
      reference_date = as.Date(forecast_date) + 3,
      # Use lubridate syntax to add horizon to forecast_date
      target_end_date = (as.Date(forecast_date) %m+% weeks(horizon)) + 3,
      # Below gave error (non-numeric argument to binary operator)
      # target_end_date = forecast_date + horizon * 7,
      output_type_id = as.numeric(quantile),
      output_type = "quantile",
      value = round(value)
    ) |>
    arrange(target_group, horizon, quantile) |>
    dplyr::select(
      reference_date,
      horizon,
      target_end_date,
      target_group,
      output_type,
      output_type_id,
      value
    )
}
