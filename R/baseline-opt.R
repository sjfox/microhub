
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
    df,
    weeks_ahead,
    quantiles_needed
) {

  preds <- map_dfr(unique(df$target_group), \(grp) {

    df_grp <- df |> filter(target_group == grp)

    baseline_fit <- fit_simple_ts(
      y = df_grp$value,
      ts_frequency = 1,
      model = "quantile_baseline",
      transformation = "sqrt",
      transform_offset = 1,
      d = 0,
      D = 0,
      symmetrize = TRUE,
      window_size = 8 # use just 8 most recent weeks
    )

    sim_matrix <- predict(
      baseline_fit,
      nsim = 10000,
      horizon = weeks_ahead,
      origin = "obs", # predict forward from the most recent value
      force_nonneg = TRUE
    )

    get_quantiles_df(sim_matrix) |>
      mutate(
        target_group = grp,
        value = ifelse(quantile == 0.5, tail(df_grp$value, 1), value)
      )
  })

  # Final formatting
  preds_formatted <- preds |>
    mutate(
      horizon = h - 2,
      reference_date = forecast_date + 3,
      target_end_date = forecast_date + 3 + horizon * 7L,
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
