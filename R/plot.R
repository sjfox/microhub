# Prepare data for plotting ====================================================

# prepare_historic_data <- function(
#   data,
#   cleaned_forecasts_quantiles,
#   forecast_date
# ) {
#   browser()
#   theme_set(theme_cowplot())
#   start_date <- as.Date(paste0(year(forecast_date), "-01", "-01"))
#
#   data <- data |>
#     rename(
#       epiweek = week,
#       count = value
#     )
#
#   # Filter current season data starting from the specified date
#   curr_season_data <- data |>
#     filter(date >= as.Date(start_date))
#
#   # Filter and reshape forecast data
#   forecast_df <- cleaned_forecasts_quantiles |>
#     filter(output_type == 'quantile') |>
#     filter(round(output_type_id, digits = 3) %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
#     spread(key = output_type_id, value = value) |>
#     mutate(
#       target_end_date = as.Date(target_end_date), # Ensure dates are formatted
#       year = year(target_end_date),
#       epiweek = sapply(target_end_date, function(date) MMWRweek(date)$MMWRweek)
#     )
#
#   # Create historic data for current season data
#   historic_season_data <- curr_season_data |>
#     select(year, epiweek, target_group, date) |>
#     rename(date_graph = date)
#   historic_season_data$year <- historic_season_data$year - 1
#
#   historic_forecast_data <- forecast_df |>
#     select(year, epiweek, target_group, target_end_date) |>
#     rename(date_graph = target_end_date)
#   historic_forecast_data$year <- historic_forecast_data$year - 1
#
#   # Ensure that both data sets have the necessary columns for merging
#   if (!all(c("year", "epiweek", "target_group") %in% names(data))) {
#     stop("Main data does not have all necessary columns (year, epiweek, target_group)")
#   }
#   if (!all(c("year", "epiweek", "target_group") %in% names(historic_season_data))) {
#     stop("Historic season data does not have all necessary columns (year, epiweek, target_group)")
#   }
#   if (!all(c("year", "epiweek", "target_group") %in% names(historic_forecast_data))) {
#     stop("Historic forecast data does not have all necessary columns (year, epiweek, target_group)")
#   }
#
#   # Merge with main data to get matching records from the previous year
#   historic_season_truth <- merge(
#     data,
#     historic_season_data,
#     by = c("year", "epiweek", "target_group"),
#     all = FALSE
#   )
#
#   historic_forecast_truth <- merge(
#     data,
#     historic_forecast_data,
#     by = c("year", "epiweek", "target_group"),
#     all = FALSE
#   )
#
#   # Combine the two sets of historical data
#   historic_data <- rbind(historic_season_truth, historic_forecast_truth)
#
#   # Return all three data frames in a list
#   return(list(
#     historic_data = historic_data,
#     curr_season_data = curr_season_data,
#     forecast_df = forecast_df
#   ))
# }

# Actual plotting function =====================================================

# TODO (Bren): maybe we want to add the "nowcast" predictions rather than just
# the four "forecast" predictions?
plot_forecasts <- function(
  target_name,
  forecast_df,
  data_df,
  seasonality
) { # Add historic_data as a parameter
  # browser()

  ## Select correct target group
  forecast_df <- forecast_df |>
    filter(target_group == target_name)
  data_df <- data_df |>
    filter(target_group == target_name)

  ## Get the data ready for plotting
  if(seasonality == 'SH' | seasonality == 'T'){
    data_df |>
      mutate(resp_season_year = MMWRweek(date)$MMWRyear,
             resp_season_week = MMWRweek(date)$MMWRweek) -> data_df
  } else{
    ## Still need to double check this one works
    df |>
      mutate(year = MMWRweek(date)$MMWRyear,
             week = MMWRweek(date)$MMWRweek) |>
      mutate(resp_season_year = ifelse(week >= 40, year, year-1)) |>
      select(-year, -week) -> df
  }

  most_recent_year <- max(data_df$resp_season_year)

  data_df |>
    filter(resp_season_year == most_recent_year) -> recent_df

  ## Only want past season data up to the current season and forecast horizon
  ## Calculate how many forecasts and add it to the max week of the recent_df
  max_past_year_resp_week <- (forecast_df |> pull(horizon) |> unique() |> length()) +
    max(recent_df$resp_season_week)

  first_date <- recent_df |>
    filter(resp_season_week == 1) |>
    pull(date)
  data_df |>
    filter(resp_season_year == most_recent_year-1,
           resp_season_week <= max_past_year_resp_week) |>
    mutate(date = first_date + weeks(resp_season_week-1)) -> past_df

  ## Get the forecasts ready for plotting
  forecast_df |>
    filter(output_type == 'quantile') |>
    filter(output_type_id %in% c('0.025', '0.25', '0.5', '0.75', '0.975')) |>
    spread(key = output_type_id, value = value) -> processed_forecast_df

  ## Now make the plot
  max_yvalue <- max(
    c(recent_df$value, processed_forecast_df$`0.75`, past_df$value),
    na.rm = TRUE
  )



  if (!any(recent_df$dropped_week)) {
    ggplot() +
      geom_ribbon(
        data = processed_forecast_df,
        aes(target_end_date, ymin = `0.025`, ymax = `0.975`),
        alpha = .2
      ) +
      geom_ribbon(
        data = processed_forecast_df,
        aes(target_end_date, ymin = `0.25`, ymax = `0.75`),
        alpha = .2
      ) +
      geom_line(
        data = processed_forecast_df,
        aes(target_end_date, `0.5`)
      ) +
      geom_point(
        data = past_df,
        aes(date, value),
        color = "red",
        alpha = .5
      ) + # Add the historic data points in red
      geom_point(
        data = recent_df,
        aes(date, value)
      ) -> base_plot

  } else {
    dropped_points <- recent_df |>
      filter(dropped_week)
    ggplot() +
      geom_ribbon(
        data = processed_forecast_df,
        aes(target_end_date, ymin = `0.025`, ymax = `0.975`),
        alpha = .2
      ) +
      geom_ribbon(
        data = processed_forecast_df,
        aes(target_end_date, ymin = `0.25`, ymax = `0.75`),
        alpha = .2
      ) +
      geom_line(
        data = processed_forecast_df,
        aes(target_end_date, `0.5`)
      ) +
      geom_point(
        data = past_df,
        aes(date, value),
        color = "red",
        alpha = .5
      ) + # Add the historic data points in red
      geom_point(
        data = recent_df |>
          filter(!dropped_week),
        aes(date, value)
      ) +
      geom_point(
        data = dropped_points,
        aes(date, value),
        color = "blue",
        alpha = .5,
        size = 2
      ) -> base_plot
  }

  base_plot +
    labs(
      title = target_name,
      x = NULL,
      y = "Value"
    ) +
    background_grid(major = "xy", minor = "y") +
    # coord_cartesian(ylim = c(0, max(c(curr_df$count, forecast_df$`0.75`, na.rm = TRUE), na.rm = TRUE))) +
    coord_cartesian(ylim = c(0, max_yvalue)) +
    theme_minimal() +
    theme(
      title = element_text(face = "bold"),
      axis.title.y = element_text(face = "plain", vjust = 2.5)
    )
}

# # Simple plot ==================================================================
#
# # This plot needs to be used starting in December until we fix Actual plotting function
#
# plot_state_forecast <- function(
#   curr_location_name,
#   curr_season_data,
#   forecast_df
# ) {
#   curr_df <- curr_season_data |>
#     filter(target_group == curr_location_name)
#
#   forecast_df <- filter(forecast_df, target_group == curr_location_name)
#
#   ggplot(forecast_df, aes(target_end_date, `0.5`)) +
#     geom_ribbon(
#       aes(ymin = `0.025`, ymax = `0.975`),
#       alpha = .2
#     ) +
#     geom_ribbon(
#       aes(ymin = `0.25`, ymax = `0.75`),
#       alpha = .2
#     ) +
#     geom_line() +
#     geom_point(
#       data = curr_df,
#       aes(date, count)
#     ) +
#     labs(title = curr_location_name, x = NULL, y = "Admits") +
#     background_grid(major = "xy", minor = "y") +
#     coord_cartesian(ylim = c(0, max(c(curr_df$count, forecast_df$`0.75`))))
# }
