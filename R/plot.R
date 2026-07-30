# Actual plotting function =====================================================

# TODO (Bren): maybe we want to add the "nowcast" predictions rather than just
# the four "forecast" predictions?
plot_forecasts <- function(
  forecast_df,
  data_df,
  seasonality,
  target_name = NULL,
  recent_points = 8L
) {
  if (!is.null(target_name)) {
    forecast_df <- forecast_df |>
      filter(target_group == target_name)
    data_df <- data_df |>
      filter(target_group == target_name)
  }

  data_df <- data_df |>
    mutate(date = as.Date(date))

  if (seasonality == "D" | seasonality == "E") {
    data_df <- data_df |>
      mutate(
        resp_season_year = MMWRweek(date)$MMWRyear,
        resp_season_week = MMWRweek(date)$MMWRweek
      )
  } else {
    data_df <- data_df |>
      mutate(
        year = MMWRweek(date)$MMWRyear,
        week = MMWRweek(date)$MMWRweek
      ) |>
      mutate(
        resp_season_year = ifelse(week >= 40, year, year - 1),
        resp_season_week = ifelse(
          week >= 40,
          week - 39,
          (MMWRweek(as.Date(sprintf("%d-12-28", resp_season_year)))[["MMWRweek"]] - 39) + week
        )
      ) |>
      select(-year, -week)
  }

  recent_df <- data_df |>
    arrange(date) |>
    group_by(target_group) |>
    slice_tail(n = recent_points) |>
    ungroup()

  processed_forecast_df <- forecast_df |>
    mutate(
      target_end_date = as.Date(target_end_date),
      forecast_position = purrr::map(target_end_date, get_respiratory_season_position, seasonality = seasonality),
      forecast_resp_season_week = purrr::map_int(forecast_position, "resp_season_week")
    ) |>
    select(-forecast_position) |>
    filter(output_type == "quantile") |>
    filter(output_type_id %in% c("0.025", "0.25", "0.5", "0.75", "0.975")) |>
    spread(key = output_type_id, value = value)

  comparison_windows <- recent_df |>
    summarize(
      display_start = min(date, na.rm = TRUE),
      display_end_observed = max(date, na.rm = TRUE),
      .by = target_group
    ) |>
    left_join(
      processed_forecast_df |>
        summarize(
          display_end_forecast = max(target_end_date, na.rm = TRUE),
          .by = target_group
        ),
      by = "target_group"
    ) |>
    mutate(
      display_end = dplyr::if_else(
        is.na(display_end_forecast),
        display_end_observed,
        pmax(display_end_observed, display_end_forecast)
      )
    )

  past_df <- data_df |>
    inner_join(comparison_windows, by = "target_group") |>
    mutate(date_aligned = date + lubridate::weeks(52)) |>
    filter(
      date_aligned >= display_start,
      date_aligned <= display_end
    ) |>
    mutate(date = date_aligned) |>
    select(-date_aligned)

  dropped_points <- recent_df |>
    filter(dropped_week)
  included_recent_df <- recent_df |>
    filter(!dropped_week)

  n_target_groups <- n_distinct(c(
    processed_forecast_df$target_group,
    recent_df$target_group
  ))
  facet_cols <- choose_forecast_facet_cols(n_target_groups)

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
    ) +
    geom_point(
      data = included_recent_df,
      aes(date, value)
    ) +
    geom_point(
      data = dropped_points,
      aes(date, value),
      color = "blue",
      alpha = .5,
      size = 2
    ) +
    facet_wrap(~ target_group, ncol = facet_cols, scales = "free_y") +
    labs(
      title = target_name,
      x = NULL,
      y = "Value"
    ) +
    background_grid(major = "xy", minor = "y") +
    theme_minimal() +
    theme(
      title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      axis.title.y = element_text(face = "plain", vjust = 2.5)
    )
}

choose_forecast_facet_cols <- function(n_target_groups) {
  if (n_target_groups <= 1) {
    return(1L)
  }

  if (n_target_groups <= 4) {
    return(2L)
  }

  if (n_target_groups <= 9) {
    return(3L)
  }

  if (n_target_groups <= 16) {
    return(4L)
  }

  ceiling(sqrt(n_target_groups))
}

plot_uploaded_time_series <- function(
  raw_data,
  forecast_date,
  data_to_drop,
  target_group = NULL
) {
  shiny::req(raw_data, forecast_date, data_to_drop)

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

  if (!is.null(target_group)) {
    plot_df <- plot_df |>
      filter(.data$target_group == !!target_group)
  }

  base_plot <- ggplot(plot_df, aes(x = date, y = value)) +
    geom_line(color = "#5D6D7E", linewidth = 0.45) +
    geom_vline(
      xintercept = as.Date(forecast_date),
      color = "#002454",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    geom_point(aes(color = point_status), size = 2.2, alpha = 0.9) +
    scale_color_manual(
      values = c(
        "Included" = "#002454",
        "Dropped from fit" = "#D94841",
        "After forecast date" = "#9AA5B1"
      ),
      drop = FALSE
    ) +
    labs(
      title = target_group,
      x = NULL,
      y = "Value",
      color = NULL
    ) +
    background_grid(major = "xy", minor = "y") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      strip.text = element_text(face = "bold", size = 15),
      legend.position = "top",
      legend.text = element_text(size = 13),
      axis.text = element_text(size = 13),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(face = "plain", size = 15, vjust = 2.5)
    )

  if (is.null(target_group)) {
    base_plot + facet_wrap(~ target_group, ncol = 1, scales = "free_y")
  } else {
    base_plot
  }
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
  seasonality,
  data_to_drop = "0 weeks",
  target_group = NULL
) {
  shiny::req(raw_data, forecast_date, seasonality, data_to_drop)

  forecast_position <- get_respiratory_season_position(
    date_value = forecast_date,
    seasonality = seasonality
  )

  raw_data <- raw_data |>
    mutate(date = as.Date(date))

  dates_to_remove <- raw_data |>
    filter(date <= forecast_date) |>
    distinct(date) |>
    arrange(desc(date)) |>
    slice_head(n = get_weeks_to_drop(data_to_drop)) |>
    pull(date)

  plot_df <- raw_data |>
    mutate(date = as.Date(date)) |>
    filter(date <= forecast_date) |>
    prep_respiratory_season_data(seasonality = seasonality)

  if (!is.null(target_group)) {
    plot_df <- plot_df |>
      filter(.data$target_group == !!target_group)
  }

  current_season_year <- max(plot_df$resp_season_year, na.rm = TRUE)

  plot_df <- plot_df |>
    mutate(
      season_status = if_else(
        resp_season_year == current_season_year,
        "Current season",
        "Historical seasons"
      ),
      dropped_week = date %in% dates_to_remove
    )

  historical_df <- plot_df |>
    filter(season_status == "Historical seasons")

  current_df <- plot_df |>
    filter(season_status == "Current season")

  current_included_df <- current_df |>
    filter(!dropped_week)

  dropped_df <- current_df |>
    filter(dropped_week)

  base_plot <- ggplot(
    plot_df,
    aes(
      x = resp_season_week,
      y = value,
      group = resp_season_year
    )
  ) +
    geom_line(
      data = historical_df,
      color = "#B8C0CC",
      linewidth = 0.55,
      alpha = 0.55
    ) +
    geom_point(
      data = historical_df,
      color = "#B8C0CC",
      size = 1.1,
      alpha = 0.45
    ) +
    geom_line(
      data = current_df,
      color = "#002454",
      linewidth = 1,
      alpha = 0.95
    ) +
    geom_point(
      data = current_included_df,
      color = "#002454",
      size = 1.9,
      alpha = 0.95
    ) +
    geom_point(
      data = dropped_df,
      color = "#D94841",
      size = 2.3,
      alpha = 0.95
    ) +
    geom_vline(
      xintercept = forecast_position$resp_season_week,
      color = "#002454",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    labs(
      title = target_group,
      x = "Respiratory Season Week",
      y = "Value"
    ) +
    background_grid(major = "xy", minor = "y") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      strip.text = element_text(face = "bold", size = 15),
      legend.position = "top",
      legend.text = element_text(size = 13),
      axis.text = element_text(size = 13),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(face = "plain", size = 15, vjust = 2.5)
    )

  if (is.null(target_group)) {
    base_plot + facet_wrap(~ target_group, ncol = 3, scales = "free_y")
  } else {
    base_plot
  }
}

write_model_plots_pdf <- function(plot_specs, file, width = 8, height = 8) {
  if (length(plot_specs) == 0) {
    stop("No model plots are available to export.")
  }

  pdf(file = file, width = width, height = height, onefile = TRUE)
  on.exit(dev.off(), add = TRUE)

  for (plot_spec in plot_specs) {
    print(plot_spec$plot)
  }

  invisible(file)
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
