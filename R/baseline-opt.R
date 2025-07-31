# Wrangle data for opt baseline ================================================

wrangle_baseline_opt <- function(
  dataframe,
  forecast_date,
  data_to_drop
) {
  forecast_date <- as.Date(forecast_date)
  reference_date <- forecast_date

  config <- switch(
    data_to_drop,
    "0 weeks" = list(days_before = 0, weeks_ahead = 4),
    "1 week"  = list(days_before = 4, weeks_ahead = 5),
    "2 week"  = list(days_before = 11, weeks_ahead = 6),
    stop("Invalid data_to_drop option")
  )

  days_before <- config$days_before

  target_tbl <- dataframe |>
    filter(date < forecast_date - days(days_before))

  # ✅ Aggregate duplicates before epi_df
  target_edf <- target_tbl |>
    group_by(target_group, date) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
    transmute(
      geo_value = target_group,
      time_value = date,
      weekly_count = value
    ) |>
    as_epi_df()

  desired_max_time_value <- reference_date - 7L

  list(
    target_edf = target_edf,
    forecast_date = forecast_date,
    desired_max_time_value = desired_max_time_value,
    base_weeks_ahead = config$weeks_ahead
  )
}

# Helper: Convert simulation matrix to tidy quantiles dataframe
get_quantiles_df <- function(
  sim_matrix,
  taus = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
) {
  if (!is.matrix(sim_matrix)) {
    stop("`sim_matrix` must be a matrix with simulations in rows and horizons in columns.")
  }

  h <- ncol(sim_matrix)
  quantile_list <- lapply(1:h, function(i) {
    quantile(sim_matrix[, i], probs = taus, na.rm = TRUE)
  })

  df <- do.call(rbind, quantile_list)
  df <- as.data.frame(df)
  df$h <- 1:h

  df_long <- tidyr::pivot_longer(df, -h, names_to = "name", values_to = "value")
  df_long$quantile <- as.numeric(gsub("%", "", df_long$name)) / 100

  df_long <- df_long[, c("h", "quantile", "value")]

  return(df_long)
}

# Fit and process opt baseline =================================================

fit_process_baseline_opt <- function(
  target_edf,
  forecast_date,
  desired_max_time_value,
  base_weeks_ahead,
  forecast_horizons
) {
  forecast_date <- as.Date(forecast_date)
  reference_date <- forecast_date
  max_horizon <- base_weeks_ahead + (forecast_horizons - 4)

  # Filter if necessary
  target_edf <- target_edf |>
    filter(time_value <= desired_max_time_value)

  # Prepare output
  preds <- data.frame()

  # Loop over each geo_value (target_group)
  for (grp in unique(target_edf$geo_value)) {
    df_grp <- target_edf |> filter(geo_value == grp)

    # Fit model
    baseline_fit <- fit_simple_ts(
      y = df_grp$weekly_count,
      ts_frequency = 1,
      model = "quantile_baseline",
      transformation = "sqrt",
      transform_offset = 1,
      d = 0,
      D = 0,
      symmetrize = TRUE,
      window_size = 8
    )

    # Predict
    sim_matrix <- predict(
      baseline_fit,
      nsim = 10000,
      horizon = max_horizon,
      origin = "obs",
      force_nonneg = TRUE
    )

    # Compute quantiles
    quantiles_df <- get_quantiles_df(sim_matrix) |>
      mutate(
        geo_value = grp,
        value = ifelse(quantile == 0.5, tail(df_grp$weekly_count, 1), value)
      )

    preds <- bind_rows(preds, quantiles_df)
  }

  # Final formatting
  preds_formatted <- preds |>
    mutate(
      horizon = h - 2,
      reference_date = reference_date,
      target_end_date = reference_date + horizon * 7L,
      output_type = "quantile",
      target_group = geo_value,
      output_type_id = quantile
    ) |>
    select(
      reference_date,
      horizon,
      target_end_date,
      target_group,
      output_type,
      output_type_id,
      value
    ) |>
    filter(horizon >= 0) |>
    arrange(target_group, horizon, output_type_id)

  return(preds_formatted)
}
