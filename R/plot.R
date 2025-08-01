# Prepare data for plotting ====================================================

prepare_historic_data <- function(
  data,
  cleaned_forecasts_quantiles,
  forecast_date
) {
  theme_set(theme_cowplot())

  start_date <- as.Date(paste0(year(forecast_date), "-01", "-01"))

  data <- data |>
    rename(
      epiweek = week,
      count = value
    )

  # Filter current season data starting from the specified date
  curr_season_data <- data |>
    filter(date >= as.Date(start_date))

  # Filter and reshape forecast data
  forecast_df <- cleaned_forecasts_quantiles |>
    filter(output_type_id %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    spread(key = output_type_id, value = value) |>
    mutate(
      target_end_date = as.Date(target_end_date), # Ensure dates are formatted
      year = year(target_end_date),
      epiweek = sapply(target_end_date, function(date) MMWRweek(date)$MMWRweek)
    )

  # Create historic data for current season data
  historic_season_data <- curr_season_data |>
    select(year, epiweek, target_group, date) |>
    rename(date_graph = date)
  historic_season_data$year <- historic_season_data$year - 1

  historic_forecast_data <- forecast_df |>
    select(year, epiweek, target_group, target_end_date) |>
    rename(date_graph = target_end_date)
  historic_forecast_data$year <- historic_forecast_data$year - 1

  # Ensure that both data sets have the necessary columns for merging
  if (!all(c("year", "epiweek", "target_group") %in% names(data))) {
    stop("Main data does not have all necessary columns (year, epiweek, target_group)")
  }
  if (!all(c("year", "epiweek", "target_group") %in% names(historic_season_data))) {
    stop("Historic season data does not have all necessary columns (year, epiweek, target_group)")
  }
  if (!all(c("year", "epiweek", "target_group") %in% names(historic_forecast_data))) {
    stop("Historic forecast data does not have all necessary columns (year, epiweek, target_group)")
  }

  # Merge with main data to get matching records from the previous year
  historic_season_truth <- merge(
    data,
    historic_season_data,
    by = c("year", "epiweek", "target_group"),
    all = FALSE
  )

  historic_forecast_truth <- merge(
    data,
    historic_forecast_data,
    by = c("year", "epiweek", "target_group"),
    all = FALSE
  )

  # Combine the two sets of historical data
  historic_data <- rbind(historic_season_truth, historic_forecast_truth)

  # Return all three data frames in a list
  return(list(
    historic_data = historic_data,
    curr_season_data = curr_season_data,
    forecast_df = forecast_df
  ))
}

# Actual plotting function =====================================================

plot_state_forecast_try <- function(
  forecast_date,
  curr_location_name,
  curr_season_data,
  forecast_df,
  historic_data,
  data_to_drop
) { # Add historic_data as a parameter

  fit_date <- as.Date(forecast_date) + 3

  curr_df <- curr_season_data |>
    filter(target_group == curr_location_name) |>
    arrange(epiweek) |>
    filter(date < fit_date)

  forecast_df <- filter(forecast_df, target_group == curr_location_name) |>
    arrange(epiweek) # Ensure data is sorted by epiweek

  historic_df <- historic_data |>
    filter(target_group == curr_location_name)

  max_count <- max(
    c(curr_df$count, forecast_df$`0.75`, historic_df$count),
    na.rm = TRUE
  )

  config <- switch(
    data_to_drop,
    "0 weeks" = list(data_pts = 0),
    "1 week" = list(data_pts = 1),
    "2 week" = list(data_pts = 2),
    stop("Invalid data_to_drop option")
  )

  if (config$data_pts == 0) {
    ggplot() +
      geom_ribbon(
        data = forecast_df,
        aes(epiweek, ymin = `0.025`, ymax = `0.975`),
        alpha = .2
      ) +
      geom_ribbon(
        data = forecast_df,
        aes(epiweek, ymin = `0.25`, ymax = `0.75`),
        alpha = .2
      ) +
      geom_line(
        data = forecast_df,
        aes(epiweek, `0.5`)
      ) +
      geom_point(
        data = curr_df,
        aes(epiweek, count)
      ) +
      geom_point(
        data = historic_df,
        aes(epiweek, count),
        color = "red"
      ) + # Add the historic data points in red
      labs(
        title = paste(curr_location_name, "Cases"),
        x = NULL,
        y = "Admits"
      ) +
      background_grid(major = "xy", minor = "y") +
      # coord_cartesian(ylim = c(0, max(c(curr_df$count, forecast_df$`0.75`, na.rm = TRUE), na.rm = TRUE))) +
      coord_cartesian(ylim = c(0, max_count)) +
      theme_minimal() +
      theme(
        title = element_text(face = "bold"),
        axis.title.y = element_text(face = "plain", vjust = 2.5)
      )
  } else {
    last_point <- tail(curr_df, config$data_pts)

    ggplot() +
      geom_ribbon(
        data = forecast_df,
        aes(epiweek, ymin = `0.025`, ymax = `0.975`),
        alpha = .2
      ) +
      geom_ribbon(
        data = forecast_df,
        aes(epiweek, ymin = `0.25`, ymax = `0.75`),
        alpha = .2
      ) +
      geom_line(
        data = forecast_df,
        aes(epiweek, `0.5`)
      ) +
      geom_point(
        data = curr_df,
        aes(epiweek, count)
      ) +
      geom_point(
        data = historic_df,
        aes(epiweek, count),
        color = "red"
      ) + # Add the historic data points in red
      geom_point(
        data = last_point,
        aes(epiweek, count),
        color = "blue",
        size = 2
      ) +
      labs(
        title = paste(curr_location_name, "Cases"),
        x = NULL,
        y = "Admits"
      ) +
      background_grid(major = "xy", minor = "y") +
      # coord_cartesian(ylim = c(0, max(c(curr_df$count, forecast_df$`0.75`, na.rm = TRUE), na.rm = TRUE))) +
      coord_cartesian(ylim = c(0, max_count)) +
      theme_minimal() +
      theme(
        title = element_text(face = "bold"),
        axis.title.y = element_text(face = "plain", vjust = 2.5)
      )
  }
}

# Simple plot ==================================================================

# This plot needs to be used starting in December until we fix Actual plotting function

plot_state_forecast <- function(
  curr_location_name,
  curr_season_data,
  forecast_df
) {
  curr_df <- curr_season_data |>
    filter(target_group == curr_location_name)

  forecast_df <- filter(forecast_df, target_group == curr_location_name)

  ggplot(forecast_df, aes(target_end_date, `0.5`)) +
    geom_ribbon(
      aes(ymin = `0.025`, ymax = `0.975`),
      alpha = .2
    ) +
    geom_ribbon(
      aes(ymin = `0.25`, ymax = `0.75`),
      alpha = .2
    ) +
    geom_line() +
    geom_point(
      data = curr_df,
      aes(date, count)
    ) +
    labs(title = curr_location_name, x = NULL, y = "Admits") +
    background_grid(major = "xy", minor = "y") +
    coord_cartesian(ylim = c(0, max(c(curr_df$count, forecast_df$`0.75`))))
}
