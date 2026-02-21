# Wrangle data for seasonal baseline ===========================================

wrangle_baseline_seasonal <- function(
  dataframe,
  forecast_date,
  data_to_drop = NULL
) {
  # browser()
  forecast_year <- year(forecast_date)
  forecast_week <- isoweek(forecast_date)

  data <- dataframe |>
    mutate(
      date = ymd(date),
      year = year(date),
      week = isoweek(date),
      count = ifelse(value == 0, 0.5, value),
      season_week = week
    ) %>%
    filter(year < forecast_year | (year == forecast_year & week < forecast_week))

  config <- switch(
    data_to_drop,
    "0 weeks" = list(days_before = 0, weeks_ahead = 4),
    "1 week"  = list(days_before = 4, weeks_ahead = 5),
    "2 week"  = list(days_before = 11, weeks_ahead = 6),
    stop("Invalid data_to_drop option")
  )

  days_before <- config$days_before

  # if (!is.null(data_to_drop)) {
  #   data <- anti_join(data, data_to_drop, by = c("target_group", "date"))
  # }

  data <- data |>
    filter(date < forecast_date - days(days_before))

  return(data)
}

# Fit and process opt baseline =================================================

fit_process_baseline_seasonal <- function(
  clean_data,
  fcast_horizon,
  quantiles_needed,
  seasonality,
  n_sim = 10000
) {
  df <- clean_data

  weeks_in_mmwr_year <- function(y) {
    MMWRweek::MMWRweek(as.Date(sprintf("%d-12-28", y)))[["MMWRweek"]]
  }

  get_season_week <- function(x, seasonality) {
    mmwr_df <- MMWRweek::MMWRweek(x)
    mmwr_year <- mmwr_df[["MMWRyear"]]
    mmwr_week <- mmwr_df[["MMWRweek"]]

    if (seasonality == "NH") {
      season_year <- ifelse(mmwr_week >= 40, mmwr_year, mmwr_year - 1)
      start_year_weeks <- vapply(season_year, weeks_in_mmwr_year, numeric(1))
      ifelse(
        mmwr_week >= 40,
        mmwr_week - 39,
        (start_year_weeks - 39) + mmwr_week
      )
    } else {
      mmwr_week
    }
  }

  fit_one_group <- function(df_group) {
    history_weeks <- get_season_week(df_group$date, seasonality)
    train_df <- df_group |>
      mutate(
        count = value + 1,
        season_week = history_weeks
      )

    if (nrow(train_df) < 10 || dplyr::n_distinct(train_df$season_week) < 5) {
      return(NULL)
    }

    k_basis <- min(20, dplyr::n_distinct(train_df$season_week) - 1)
    if (k_basis < 4) return(NULL)

    s <- mgcv::s
    model <- mgcv::gam(
      log(count) ~ s(season_week, bs = "cc", k = k_basis),
      data = train_df,
      method = "REML"
    )

    last_date <- max(train_df$date)
    last_week <- train_df |>
      arrange(date) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::pull(season_week)

    last_mmwr <- MMWRweek::MMWRweek(last_date)
    last_mmwr_week <- last_mmwr[["MMWRweek"]]
    last_mmwr_year <- last_mmwr[["MMWRyear"]]

    season_cycle <- if (seasonality == "NH") {
      season_year <- ifelse(last_mmwr_week >= 40, last_mmwr_year, last_mmwr_year - 1)
      weeks_in_mmwr_year(season_year)
    } else {
      weeks_in_mmwr_year(last_mmwr_year)
    }

    future_weeks <- ((last_week + seq_len(fcast_horizon) - 1) %% season_cycle) + 1
    eta <- as.numeric(predict(model, newdata = tibble(season_week = future_weeks)))
    sigma <- sqrt(model$sig2)

    sim <- matrix(
      rnorm(n_sim * length(eta), mean = rep(eta, each = n_sim), sd = sigma),
      nrow = n_sim,
      ncol = length(eta)
    ) |>
      exp() |>
      {\(x) pmax(x - 1, 0)}()

    qs <- purrr::map_dfr(seq_len(ncol(sim)), function(h) {
      tibble(
        horizon = h,
        output_type_id = as.character(quantiles_needed),
        value = as.numeric(quantile(sim[, h], probs = quantiles_needed, na.rm = TRUE))
      )
    })

    qs |>
      mutate(
        target_group = df_group$target_group[1],
        output_type = "quantile"
      ) |>
      select(horizon, target_group, output_type, output_type_id, value)
  }

  df |>
    mutate(date = as.Date(date)) |>
    group_split(target_group) |>
    purrr::map_dfr(fit_one_group) |>
    mutate(output_type_id = as.character(output_type_id)) |>
    dplyr::select(
      horizon,
      target_group,
      output_type,
      output_type_id,
      value
    )
}
