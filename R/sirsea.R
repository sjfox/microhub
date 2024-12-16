# Setup ========================================================================

library(dplyr)
library(lubridate)
library(forcats)
library(splines)
library(cmdstanr)
library(posterior)

# Wrangle data for SIRsea model ================================================

wrangle_sirsea <- function(
  dataframe,
  forecast_date,
  data_to_drop,
  forecast_horizons
) {
  # Define the configuration based on data_to_drop
  config <- switch(
    data_to_drop,
    "1 week" = list(days_before = 2, weeks_ahead = 5),
    "2 week" = list(days_before = 7, weeks_ahead = 6),
    "3 week" = list(days_before = 14, weeks_ahead = 7),
    stop("Invalid data_to_drop option")
  )

  weeks_ahead <- config$weeks_ahead

  # Wrangle data
  wrangled_data <- dataframe |>
    filter(
      age_group != "Unknown"
    ) |> # Remove rows with "Unknown" age group
    filter(
      date < as.Date(forecast_date) - days(config$days_before)
    ) |> # Remove recent data
    rename(epiweek = week)

  # Subset data for mechanistic model
  subset_data <- wrangled_data |>
    filter(
      date > "2021-09-01",
      age_group %in% c("Pediatric", "Adult")
    ) |> # Filter relevant data
    mutate(
      age_group = fct_relevel(age_group, "Pediatric")
    ) |> # Reorder factor levels
    filter(epiweek != 53) |> # Exclude epiweek 53 for consistency
    arrange(age_group, date)

  if (!"inc_sari_hosp" %in% colnames(subset_data)) {
    stop("The column 'inc_sari_hosp' is not present in the dataframe.")
  }

  if (nrow(subset_data) %% 2 != 0) {
    stop("The number of rows in 'subset_data' must be even to construct 'ymat'.")
  }

  Tmax <- length(unique(subset_data$date)) + weeks_ahead
  spline_df <- round(
    Tmax / 7
  ) # the number of knots to place evenly (approx every 7 weeks)
  B <- t(bs(1:Tmax, spline_df, Boundary.knots = c(-2, Tmax + 3)))

  ymat <- matrix(subset_data$inc_sari_hosp, nrow = 2, byrow = TRUE)

  # Prepare data for Stan
  stan_dat <- list(
    `T` = length(unique(subset_data$date)),
    H = weeks_ahead,
    G = 2,
    N = c(1e6, 1e6), # let's just say a million people in each group at risk
    df = spline_df,
    B = B,
    y = ymat,
    # i_init=c(0.005, 0.005),
    alpha = c(0.9, 0.7), # pediatrics recover more quickly
    sd_phi_df = 3,
    sd_phi_scale = 5
  )

  return(list(subset_data = subset_data, stan_dat = stan_dat))
}

# Fit and process SIRsea =======================================================

fit_process_sirsea <- function(
  dataframe,
  stan_dat,
  forecast_date,
  data_to_drop,
  cmdstan_path
) {
  dataframe <- dataframe |>
    mutate(date = as.Date(date))

  config <- switch(
    data_to_drop,
    "1 week" = list(days_before = 2, weeks_ahead = 5),
    "2 week" = list(days_before = 7, weeks_ahead = 6),
    "3 week" = list(days_before = 14, weeks_ahead = 7),
    stop("Invalid data_to_drop option")
  )

  weeks_ahead <- config$weeks_ahead

  # Set the CmdStan path
  set_cmdstan_path(cmdstan_path)

  # Fit the model with MCMC
  exec <- cmdstan_model("data/sirsea2.stan")

  fit <- exec$sample(
    data = stan_dat,
    chains = 8,
    parallel_chains = 8,
    iter_sampling = 1000,
    iter_warmup = 10000,
    adapt_delta = 0.9
  )

  # Summarize fit results
  (fit_summ <- fit$summary()) # want rhat all <1.01
  post <- as_draws_df(fit$draws())

  # Process posterior predictions
  last_date <- as.Date(max(dataframe$date))

  quantiles_needed <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

  post_pred <- post |>
    mutate(draw = .draw) |>
    select(contains("yhat"), draw) |>
    pivot_longer(-draw, values_to = "pred_count") |>
    mutate(
      age_group = ifelse(
        str_extract(name, "\\d+(?=,)") == "1",
        "Pediatric",
        "Adult"
      ),
      age_group = fct_relevel(age_group, "Pediatric"),
      t = as.integer(str_extract(name, "(?<=,)\\d+")),
      .keep = "unused",
      .before = pred_count
    ) |>
    arrange(draw, age_group, t) |>
    group_by(draw, age_group) |>
    # mutate(
    #   date = c(unique(dataframe$date), last_date + weeks(1:weeks_ahead))
    # ) |> # 5, 6,
    # mutate(
    #   date = c(unique(dataframe$date), as.Date(last_date + weeks(1:weeks_ahead)))
    # ) |>
    mutate(date = c(
      unique(dataframe$date),
      seq(last_date + 7, by = "1 week", length.out = weeks_ahead)
    )) |>
    ungroup()

  # Summarize predictions
  post_pred_summ <- post_pred |>
    filter(t > 1) |>
    group_by(age_group, date) |>
    summarize(
      mean = mean(pred_count),
      qs = list(value = quantile(pred_count, c(0.025, 0.25, 0.5, 0.75, 0.975))),
      .groups = "drop"
    ) |>
    unnest_wider(qs) |>
    left_join(dataframe, by = c("age_group", "date"))

  # Combine overall predictions
  pp_overall <- post_pred |>
    group_by(draw, date) |>
    summarise(pred_count = sum(pred_count), .groups = "drop") |>
    mutate(age_group = "Overall")

  post_pred_quants <- bind_rows(post_pred, pp_overall) |>
    filter(date > last_date) |>
    mutate(horizon = as.numeric(as.factor(date)) - 1) |>
    group_by(age_group, date, horizon) |>
    reframe(enframe(quantile(pred_count, quantiles_needed), "quant_level")) |>
    arrange(date, age_group)

  post_pred_quants <- format_quantiles(post_pred_quants, forecast_date)

  # Return results
  return(post_pred_quants)
}
