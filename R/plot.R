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
  if (seasonality == 'D' | seasonality == 'E') {
    data_df |>
      mutate(resp_season_year = MMWRweek(date)$MMWRyear,
             resp_season_week = MMWRweek(date)$MMWRweek) -> data_df
  } else{
    data_df |>
      mutate(year = MMWRweek(date)$MMWRyear,
             week = MMWRweek(date)$MMWRweek) |>
      mutate(resp_season_year = ifelse(week >= 40, year, year-1),
             resp_season_week = ifelse(week >= 40, week - 39, (MMWRweek(as.Date(sprintf("%d-12-28", resp_season_year)))[["MMWRweek"]] - 39) + week)) |>
      select(-year, -week) -> data_df
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

plot_uploaded_time_series <- function(
  raw_data,
  forecast_date,
  data_to_drop
) {
  req(raw_data, forecast_date, data_to_drop)

  plot_df <- raw_data |>
    mutate(
      date = as.Date(date),
      point_status = case_when(
        date > forecast_date ~ "After forecast date",
        TRUE ~ "Included"
      )
    )

  dates_to_remove <- plot_df |>
    filter(date <= forecast_date) |>
    distinct(date) |>
    arrange(desc(date)) |>
    slice_head(n = get_weeks_to_drop(data_to_drop)) |>
    pull(date)

  plot_df <- plot_df |>
    mutate(
      point_status = case_when(
        date %in% dates_to_remove ~ "Dropped from fit",
        TRUE ~ point_status
      ),
      point_status = factor(
        point_status,
        levels = c("Included", "Dropped from fit", "After forecast date")
      )
    )

  ggplot(plot_df, aes(x = date, y = value)) +
    geom_line(color = "#5D6D7E", linewidth = 0.45) +
    geom_vline(
      xintercept = as.Date(forecast_date),
      color = "#002454",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    geom_point(aes(color = point_status), size = 2.2, alpha = 0.9) +
    facet_wrap(~ target_group, ncol = 1, scales = "free_y") +
    scale_color_manual(
      values = c(
        "Included" = "#002454",
        "Dropped from fit" = "#D94841",
        "After forecast date" = "#9AA5B1"
      ),
      drop = FALSE
    ) +
    labs(
      x = NULL,
      y = "Value",
      color = NULL
    ) +
    background_grid(major = "xy", minor = "y") +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "top",
      axis.title.y = element_text(face = "plain", vjust = 2.5)
    )
}

prep_respiratory_season_data <- function(
  data_df,
  seasonality
) {
  season_df <- if (seasonality == "D" | seasonality == "E") {
    data_df |>
      mutate(resp_season_year = MMWRweek(date)$MMWRyear)
  } else {
    data_df |>
      mutate(
        year = MMWRweek(date)$MMWRyear,
        week = MMWRweek(date)$MMWRweek
      ) |>
      mutate(resp_season_year = ifelse(week >= 40, year, year - 1)) |>
      select(-year, -week)
  }

  season_df |>
    group_by(target_group, resp_season_year) |>
    arrange(date, .by_group = TRUE) |>
    mutate(resp_season_week = row_number()) |>
    ungroup()
}

get_respiratory_season_position <- function(
  date_value,
  seasonality
) {
  date_value <- as.Date(date_value)

  if (seasonality == "D" | seasonality == "E") {
    tibble(
      resp_season_year = MMWRweek(date_value)$MMWRyear,
      resp_season_week = MMWRweek(date_value)$MMWRweek
    )
  } else {
    mmwr_year <- MMWRweek(date_value)$MMWRyear
    mmwr_week <- MMWRweek(date_value)$MMWRweek
    resp_season_year <- ifelse(mmwr_week >= 40, mmwr_year, mmwr_year - 1)
    final_week_prev_year <- MMWRweek(as.Date(sprintf("%d-12-28", resp_season_year)))[["MMWRweek"]]

    tibble(
      resp_season_year = resp_season_year,
      resp_season_week = ifelse(
        mmwr_week >= 40,
        mmwr_week - 39,
        (final_week_prev_year - 39) + mmwr_week
      )
    )
  }
}

plot_uploaded_resp_season_series <- function(
  raw_data,
  forecast_date,
  seasonality
) {
  req(raw_data, forecast_date, seasonality)

  forecast_position <- get_respiratory_season_position(
    date_value = forecast_date,
    seasonality = seasonality
  )

  plot_df <- raw_data |>
    mutate(date = as.Date(date)) |>
    filter(date <= forecast_date) |>
    prep_respiratory_season_data(seasonality = seasonality) |>
    mutate(resp_season_year = as.factor(resp_season_year))

  ggplot(
    plot_df,
    aes(
      x = resp_season_week,
      y = value,
      color = resp_season_year,
      group = resp_season_year
    )
  ) +
    geom_line(linewidth = 0.7, alpha = 0.85) +
    geom_point(size = 1.6, alpha = 0.9) +
    geom_vline(
      xintercept = forecast_position$resp_season_week,
      color = "#002454",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    facet_wrap(~ target_group, ncol = 3, scales = "free_y") +
    labs(
      x = "Respiratory Season Week",
      y = "Value",
      color = "Season Year"
    ) +
    background_grid(major = "xy", minor = "y") +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "top",
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
