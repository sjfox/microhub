# Setup ========================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(tibble)
library(lubridate)
library(INLA)
library(MMWRweek)

inla.setOption(inla.mode = "classic")

# Wrangle data for INLA model ==================================================

wrangle_inla <- function(
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
      count = inc_sari_hosp
    ) |>
    filter(year > 2021) |>
    filter(age_group %in% c("Pediatric", "Adult", "Overall"))

  # Add population column
  data_with_population <- add_population_column(
    "data/Population_Data.csv",
    data_preprocessed,
    2023
  )

  # Determine configuration based on selected data to drop option
  config <- switch(
    data_to_drop,
    "1 week" = list(days_before = 2, weeks_ahead = 5),
    "2 week" = list(days_before = 7, weeks_ahead = 6),
    "3 week" = list(days_before = 14, weeks_ahead = 7),
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

  fit_df <- prep_fit_data(wrangled_data, weeks_ahead)

  return(fit_df)
}

# Fit and process INLA =========================================================

fit_process_inla <- function(
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

  # Sample national-level predictions
  nat_samps <- sample_national(fit_df, fit, forecast_date)

  # Sample count predictions
  pred_samples <- sample_count_predictions(fit_df, fit)

  # Summarize quantiles
  forecast_quantiles <- summarize_quantiles(
    pred_samples,
    nat_samps,
    forecast_date,
    q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  )

  return(forecast_quantiles)
}

# INLA helper functions ========================================================

add_population_column <- function(file_path, data_frame, year) {
  # Read the population data from the specified file
  pop_data <- read.csv(file_path)

  # Define the new column names
  new_names <- c(
    "Name",
    "Code",
    "Age",
    "Code2",
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019",
    "2020",
    "2021",
    "2022",
    "2023"
  )

  # Rename the columns
  colnames(pop_data)[1:length(new_names)] <- new_names

  # Define age groups
  under_20_groups <- c(
    "Population ages 0-14, total",
    "Population ages 15-19, female",
    "Population ages 15-19, male"
  )

  over_20_groups <- c(
    "Population ages 20-24, female",
    "Population ages 20-24, male",
    "Population ages 25-29, female",
    "Population ages 25-29, male",
    "Population ages 30-34, female",
    "Population ages 30-34, male",
    "Population ages 35-39, female",
    "Population ages 35-39, male",
    "Population ages 40-44, female",
    "Population ages 40-44, male",
    "Population ages 45-49, female",
    "Population ages 45-49, male",
    "Population ages 50-54, female",
    "Population ages 50-54, male",
    "Population ages 55-59, female",
    "Population ages 55-59, male",
    "Population ages 60-64, female",
    "Population ages 60-64, male",
    "Population ages 65 and above, female",
    "Population ages 65 and above, male"
  )

  # Calculate the population sums for the specified year
  sum_under_20 <- pop_data |>
    filter(Age %in% under_20_groups) |>
    summarise(Sum = sum(as.numeric(.data[[as.character(year)]]))) |>
    pull(Sum)

  sum_over_20 <- pop_data |>
    filter(Age %in% over_20_groups) |>
    summarise(Sum = sum(as.numeric(.data[[as.character(year)]]))) |>
    pull(Sum)

  sum_total <- sum_under_20 + sum_over_20

  # Update the input data frame with the new population data
  data_frame <- data_frame |>
    mutate(population = case_when(
      age_group == "Pediatric" ~ sum_under_20,
      age_group == "Adult" ~ sum_over_20,
      age_group == "Overall" ~ sum_total,
      TRUE ~ NA_real_ # Assign NA for other cases
    ))

  return(data_frame)
}

prep_fit_data <- function(input_data, weeks_ahead = weeks_ahead) {
  input_data$date <- as.Date(input_data$date)

  ret <- input_data |>
    filter(age_group != "Overall") |>
    group_by(date) |>
    mutate(t = cur_group_id(), .after = date) |> # add a time counter starting from 1 for earliest week
    ungroup() |>
    mutate(
      snum = as.numeric(fct_inorder(age_group)), # INLA needs groups as ints starting from 1, so add numeric state code
      ex_lam = population
    )

  # make a dataframe to hold group info for forecasting
  pred_df <- expand_grid(
    tibble(
      date = duration(1:weeks_ahead, "week") + max(ret$date),
      t = 1:weeks_ahead + max(ret$t),
      epiweek = epiweek(date)
    ),
    distinct(ret, age_group, snum, population) # makes pairs of new times X each state
  ) |>
    left_join(distinct(ret, age_group, epiweek, ex_lam)) # go and find `ex_lam` values for each state and epiweek

  bind_rows(ret, pred_df) |> # add to data for counts to be NAs
    arrange(t)
}

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
  state_info <- distinct(fit_df, age_group, ex_lam)
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
    tibble_row(age_group = "Overall", count_samp = list(nat_sum_per_t))
  }) |>
    bind_cols(ret_df) |>
    select(date:epiweek, age_group, population, count_samp)
}

summarize_quantiles <- function(
  pred_samples,
  nat_samps,
  forecast_date,
  q
) {
  pred_samples |>
    filter(date >= forecast_date) |>
    bind_rows(nat_samps) |>
    unnest(count_samp) |>
    group_by(date, age_group) |>
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
      target = paste0("inc sari hosp"),
      horizon = as.numeric(as.factor(date)) - 1,
      reference_date = forecast_date,
      # Use lubridate syntax to add horizon to forecast_date
      target_end_date = as.Date(forecast_date) %m+% weeks(horizon),
      # Below gave error (non-numeric argument to binary operator)
      # target_end_date = forecast_date + horizon * 7,
      output_type_id = as.character(quantile),
      output_type = "quantile",
      value = round(value)
    ) |>
    arrange(age_group, horizon, quantile) |>
    dplyr::select(
      reference_date,
      target,
      horizon,
      target_end_date,
      age_group,
      output_type,
      output_type_id,
      value
    )
}
