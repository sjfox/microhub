# Retrospective forecasting helpers ==========================================

retrospective_model_choices <- c(
  "Regular Baseline" = "baseline_regular",
  "Seasonal Baseline" = "baseline_seasonal",
  "Opt Baseline" = "baseline_opt",
  "INFLAenza" = "inla",
  "Copycat" = "copycat",
  "CalCopycat" = "calcopycat",
  "newGBQR" = "newgbqr",
  "FourCAT" = "fourcat"
)

retrospective_development_model_choices <- c("calcopycat", "fourcat")

retrospective_default_model_choices <- retrospective_model_choices[
  !(retrospective_model_choices %in% retrospective_development_model_choices)
]

retrospective_default_settings <- function(has_population = FALSE) {
  list(
    inla = list(
      forecast_uncertainty = "default",
      use_offset = isTRUE(has_population)
    ),
    copycat = list(
      recent_weeks_touse = 100,
      resp_week_range = 2,
      share_groups = TRUE
    ),
    calcopycat = list(
      recent_weeks_touse = 12,
      resp_week_range = 2,
      share_groups = TRUE,
      ref_week_window = 1,
      nsamps_cal = 100
    ),
    newgbqr = list(
      model_type = "global"
    ),
    fourcat = list(
      seeds = c(41L, 42L, 43L)
    )
  )
}

available_retrospective_reference_dates <- function(data) {
  data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::distinct(date) |>
    dplyr::arrange(date) |>
    dplyr::pull(date) |>
    {\(x) x[-1]}()
}

retrospective_reference_range <- function(data, start_date, end_date) {
  dates <- available_retrospective_reference_dates(data)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  dates[dates >= start_date & dates <= end_date]
}

format_retrospective_forecasts <- function(forecast_df, model_name, reference_date) {
  reference_date <- as.Date(reference_date)

  forecast_df |>
    dplyr::mutate(
      model = model_name,
      reference_date = reference_date,
      horizon = as.integer(horizon) - 1L,
      target_end_date = reference_date + lubridate::weeks(horizon),
      output_type_id = as.character(output_type_id)
    ) |>
    dplyr::select(
      model,
      reference_date,
      horizon,
      target_end_date,
      target_group,
      output_type,
      output_type_id,
      value
    )
}

is_retrospective_baseline_model <- function(model_name) {
  grepl("Baseline", model_name, ignore.case = TRUE)
}

build_retrospective_nonbaseline_ensemble <- function(weekly_results) {
  weekly_output <- dplyr::bind_rows(weekly_results)

  if (nrow(weekly_output) == 0) {
    return(NULL)
  }

  nonbaseline_forecasts <- weekly_output |>
    dplyr::filter(
      !is_retrospective_baseline_model(model),
      model != "Ensemble"
    )

  if (dplyr::n_distinct(nonbaseline_forecasts$model) < 2) {
    return(NULL)
  }

  nonbaseline_forecasts |>
    dplyr::summarize(
      value = round(stats::median(value, na.rm = TRUE), 0),
      .by = c(
        reference_date,
        horizon,
        target_end_date,
        target_group,
        output_type,
        output_type_id
      )
    ) |>
    dplyr::mutate(model = "Ensemble", .before = 1) |>
    dplyr::select(
      model,
      reference_date,
      horizon,
      target_end_date,
      target_group,
      output_type,
      output_type_id,
      value
    )
}

retrospective_ensemble_plot_data <- function(forecasts,
                                             actual_data,
                                             forecast_stride = 3L) {
  empty <- list(
    actual = tibble::tibble(
      date = as.Date(character()),
      target_group = character(),
      value = numeric()
    ),
    forecast = tibble::tibble(
      reference_date = as.Date(character()),
      target_group = character(),
      target_end_date = as.Date(character()),
      q0.025 = numeric(),
      q0.25 = numeric(),
      q0.5 = numeric(),
      q0.75 = numeric(),
      q0.975 = numeric()
    ),
    model = NA_character_,
    is_ensemble = FALSE
  )

  if (is.null(forecasts) || nrow(forecasts) == 0) {
    return(empty)
  }

  quantile_forecasts <- forecasts |>
    dplyr::mutate(
      reference_date = as.Date(reference_date),
      target_end_date = as.Date(target_end_date),
      output_type_id = as.character(output_type_id)
    ) |>
    dplyr::filter(
      output_type == "quantile",
      output_type_id %in% c("0.025", "0.25", "0.5", "0.75", "0.975")
    )

  if (nrow(quantile_forecasts) == 0) {
    return(empty)
  }

  available_models <- quantile_forecasts |>
    dplyr::distinct(model) |>
    dplyr::arrange(model) |>
    dplyr::pull(model)
  plot_model <- if ("Ensemble" %in% available_models) {
    "Ensemble"
  } else {
    nonbaseline_models <- setdiff(
      available_models,
      c("Regular Baseline", "Seasonal Baseline", "Opt Baseline")
    )
    if (length(nonbaseline_models) > 0) nonbaseline_models[[1]] else available_models[[1]]
  }

  plot_forecasts <- quantile_forecasts |>
    dplyr::filter(model == plot_model)

  reference_dates <- plot_forecasts |>
    dplyr::distinct(reference_date) |>
    dplyr::arrange(reference_date) |>
    dplyr::pull(reference_date)

  stride <- max(1L, as.integer(forecast_stride)[[1]])
  kept_reference_dates <- reference_dates[seq(1L, length(reference_dates), by = stride)]
  if (!utils::tail(reference_dates, 1) %in% kept_reference_dates) {
    kept_reference_dates <- c(kept_reference_dates, utils::tail(reference_dates, 1))
  }

  forecast_plot <- plot_forecasts |>
    dplyr::filter(reference_date %in% kept_reference_dates) |>
    dplyr::select(
      reference_date,
      target_group,
      target_end_date,
      output_type_id,
      value
    ) |>
    tidyr::pivot_wider(
      names_from = output_type_id,
      values_from = value,
      names_prefix = "q"
    ) |>
    dplyr::arrange(target_group, reference_date, target_end_date)

  missing_quantile_cols <- setdiff(
    c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975"),
    names(forecast_plot)
  )
  forecast_plot[missing_quantile_cols] <- NA_real_

  target_groups <- forecast_plot |>
    dplyr::distinct(target_group) |>
    dplyr::pull(target_group)
  target_dates <- forecast_plot |>
    dplyr::distinct(target_end_date) |>
    dplyr::pull(target_end_date)

  actual_plot <- actual_data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(
      target_group %in% target_groups,
      date >= min(target_dates, na.rm = TRUE),
      date <= max(target_dates, na.rm = TRUE)
    ) |>
    dplyr::select(date, target_group, value) |>
    dplyr::arrange(target_group, date)

  list(
    actual = actual_plot,
    forecast = forecast_plot |>
      dplyr::select(
        reference_date,
        target_group,
        target_end_date,
        q0.025,
        q0.25,
        q0.5,
        q0.75,
        q0.975
      ),
    model = plot_model,
    is_ensemble = identical(plot_model, "Ensemble")
  )
}

plot_retrospective_ensemble_forecasts <- function(forecasts,
                                                  actual_data,
                                                  forecast_stride = 3L) {
  plot_data <- retrospective_ensemble_plot_data(
    forecasts = forecasts,
    actual_data = actual_data,
    forecast_stride = forecast_stride
  )

  shiny::req(nrow(plot_data$forecast) > 0)

  has_95 <- any(!is.na(plot_data$forecast$`q0.025`)) &&
    any(!is.na(plot_data$forecast$`q0.975`))
  has_50 <- any(!is.na(plot_data$forecast$`q0.25`)) &&
    any(!is.na(plot_data$forecast$`q0.75`))
  has_median <- any(!is.na(plot_data$forecast$`q0.5`))

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = plot_data$actual,
      ggplot2::aes(date, value),
      color = "#1F2937",
      linewidth = 0.65
    ) +
    ggplot2::geom_point(
      data = plot_data$actual,
      ggplot2::aes(date, value),
      color = "#1F2937",
      size = 1.2,
      alpha = 0.8
    )

  if (has_95) {
    p <- p +
      ggplot2::geom_ribbon(
        data = plot_data$forecast,
        ggplot2::aes(
          target_end_date,
          ymin = `q0.025`,
          ymax = `q0.975`,
          group = interaction(reference_date, target_group)
        ),
        fill = "#4EA3C8",
        alpha = 0.13
      )
  }

  if (has_50) {
    p <- p +
      ggplot2::geom_ribbon(
        data = plot_data$forecast,
        ggplot2::aes(
          target_end_date,
          ymin = `q0.25`,
          ymax = `q0.75`,
          group = interaction(reference_date, target_group)
        ),
        fill = "#1B7FA7",
        alpha = 0.22
      )
  }

  if (has_median) {
    p <- p +
      ggplot2::geom_line(
        data = plot_data$forecast,
        ggplot2::aes(
          target_end_date,
          `q0.5`,
          group = interaction(reference_date, target_group)
        ),
        color = "#0B6E99",
        linewidth = 0.8,
        alpha = 0.8
      )
  }

  p +
    ggplot2::facet_wrap(~target_group, scales = "free_y") +
    ggplot2::labs(
      x = NULL,
      y = "Observed value",
      caption = paste0(
        "Black line: observed target data. Blue lines and ribbons: ",
        plot_data$model,
        " forecasts, showing every ",
        forecast_stride,
        "rd forecast origin and always including the final origin."
      )
    ) +
    cowplot::background_grid(major = "xy", minor = "y") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0, color = "#5E6C80")
    )
}

retrospective_score_metric <- function(score_tbl) {
  if (any(!is.na(score_tbl$mean_relative_wis))) {
    return(list(
      column = "mean_relative_wis",
      label = "Relative WIS",
      uses_relative = TRUE
    ))
  }

  list(
    column = "mean_wis",
    label = "WIS",
    uses_relative = FALSE
  )
}

retrospective_wrap_labels <- function(labels, width = 14L) {
  vapply(
    labels,
    function(label) paste(strwrap(label, width = width), collapse = "\n"),
    character(1)
  )
}

plot_retrospective_target_group_scores <- function(score_tbl,
                                                   reference_model = "Regular Baseline") {
  metric <- retrospective_score_metric(score_tbl)
  plot_tbl <- score_tbl |>
    dplyr::filter(model != reference_model) |>
    dplyr::mutate(
      score_value = .data[[metric$column]],
      score_label = ifelse(is.na(score_value), "", sprintf("%.2f", score_value))
    )

  if (nrow(plot_tbl) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No non-baseline model scores available.") +
        ggplot2::theme_void(base_size = 15)
    )
  }

  order_category <- if ("Overall" %in% plot_tbl$target_group) {
    "Overall"
  } else {
    plot_tbl |>
      dplyr::distinct(target_group) |>
      dplyr::arrange(target_group) |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(target_group)
  }

  model_levels <- plot_tbl |>
    dplyr::filter(target_group == order_category) |>
    dplyr::arrange(score_value, model) |>
    dplyr::pull(model) |>
    unique()
  model_levels <- c(
    model_levels,
    sort(setdiff(unique(as.character(plot_tbl$model)), model_levels))
  )

  plot_tbl <- plot_tbl |>
    dplyr::mutate(model = factor(model, levels = rev(model_levels)))

  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = target_group, y = model, fill = score_value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = score_label), size = 4.6) +
    ggplot2::scale_x_discrete(labels = retrospective_wrap_labels) +
    ggplot2::labs(
      title = "Retrospective performance by target group",
      subtitle = if (metric$uses_relative) {
        "Cells show relative WIS by model and target group. Lower is better; values below 1 improve on Regular Baseline."
      } else {
        "Cells show WIS by model and target group. Lower is better."
      },
      x = "Target group",
      y = NULL,
      fill = metric$label
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 13),
      axis.text.y = ggplot2::element_text(size = 13),
      axis.title.x = ggplot2::element_text(size = 15, margin = ggplot2::margin(t = 10)),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 13),
      plot.title = ggplot2::element_text(size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 14),
      panel.grid = ggplot2::element_blank()
    )

  if (metric$uses_relative) {
    p +
      ggplot2::scale_fill_gradient2(
        low = "#2E8B57",
        mid = "#F7F7F7",
        high = "#C0392B",
        midpoint = 1,
        na.value = "#E5E7EB"
      )
  } else {
    p +
      ggplot2::scale_fill_gradient(
        low = "#E8F3EC",
        high = "#C0392B",
        na.value = "#E5E7EB"
      )
  }
}

plot_retrospective_forecast_date_scores <- function(score_tbl,
                                                    reference_model = "Regular Baseline") {
  metric <- retrospective_score_metric(score_tbl)
  plot_tbl <- score_tbl |>
    dplyr::filter(model != reference_model) |>
    dplyr::mutate(
      reference_date = as.Date(reference_date),
      score_value = .data[[metric$column]],
      is_baseline = is_retrospective_baseline_model(model)
    )

  if (nrow(plot_tbl) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No non-baseline model scores available.") +
        ggplot2::theme_void(base_size = 15)
    )
  }

  baseline_models <- plot_tbl |>
    dplyr::filter(is_baseline) |>
    dplyr::distinct(model) |>
    dplyr::arrange(model) |>
    dplyr::pull(model)
  nonbaseline_models <- plot_tbl |>
    dplyr::filter(!is_baseline) |>
    dplyr::distinct(model) |>
    dplyr::arrange(model) |>
    dplyr::pull(model)

  baseline_colors <- if (length(baseline_models) > 0) {
    stats::setNames(
      grDevices::grey.colors(length(baseline_models), start = 0.35, end = 0.7),
      baseline_models
    )
  } else {
    character()
  }
  model_colors <- c(
    baseline_colors,
    stats::setNames(scales::hue_pal()(length(nonbaseline_models)), nonbaseline_models)
  )

  p <- ggplot2::ggplot(
    plot_tbl,
    ggplot2::aes(x = reference_date, y = score_value, color = model, group = model)
  ) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.95) +
    ggplot2::geom_point(size = 2.8, alpha = 0.95) +
    ggplot2::scale_color_manual(values = model_colors) +
    ggplot2::labs(
      title = "Retrospective performance by forecast date",
      x = "Forecast date",
      y = metric$label,
      color = "Model"
    ) +
    cowplot::background_grid(major = "xy", minor = "y") +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 13),
      axis.text.x = ggplot2::element_text(size = 13),
      axis.text.y = ggplot2::element_text(size = 13),
      axis.title.x = ggplot2::element_text(size = 15, margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(size = 15, margin = ggplot2::margin(r = 10)),
      plot.title = ggplot2::element_text(size = 18, face = "bold")
    )

  if (metric$uses_relative) {
    p + ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "#5E6C80")
  } else {
    p
  }
}

as_scoringutils_scores <- function(score_rows, metrics) {
  class(score_rows) <- c("scores", class(score_rows))
  attr(score_rows, "metrics") <- metrics
  score_rows
}

add_direct_baseline_relative_skill <- function(score_rows,
                                               metric,
                                               output_col,
                                               reference_model) {
  score_rows[[output_col]] <- NA_real_

  if (nrow(score_rows) == 0 || !reference_model %in% score_rows$model) {
    return(score_rows)
  }

  baseline_scores <- score_rows |>
    dplyr::filter(model == reference_model) |>
    dplyr::select(
      reference_date,
      horizon,
      target_end_date,
      target_group,
      baseline_metric = dplyr::all_of(metric)
    )

  score_rows |>
    dplyr::select(-dplyr::all_of(output_col)) |>
    dplyr::left_join(
      baseline_scores,
      by = c("reference_date", "horizon", "target_end_date", "target_group")
    ) |>
    dplyr::mutate(
      !!output_col := dplyr::if_else(
        baseline_metric > 0,
        .data[[metric]] / baseline_metric,
        NA_real_
      )
    ) |>
    dplyr::select(-baseline_metric)
}

add_scoringutils_relative_skill <- function(score_rows,
                                            group_vars,
                                            metric,
                                            output_col,
                                            reference_model) {
  score_rows[[output_col]] <- NA_real_

  if (nrow(score_rows) == 0 ||
      !reference_model %in% score_rows$model) {
    return(score_rows)
  }

  if (length(setdiff(unique(score_rows$model), reference_model)) < 2) {
    return(add_direct_baseline_relative_skill(
      score_rows = score_rows,
      metric = metric,
      output_col = output_col,
      reference_model = reference_model
    ))
  }

  scored <- tryCatch(
    {
      by_arg <- if (length(group_vars) == 0) NULL else group_vars
      skill_input <- score_rows |>
        dplyr::select(
          dplyr::any_of(c(
            "model",
            group_vars,
            "reference_date",
            "horizon",
            "target_end_date",
            "target_group",
            metric
          ))
        )

      suppressWarnings(
        scoringutils::add_relative_skill(
          scores = as_scoringutils_scores(skill_input, metric),
          compare = "model",
          by = by_arg,
          metric = metric,
          baseline = reference_model
        )
      )
    },
    error = function(e) score_rows
  )

  scaled_col <- paste0(metric, "_scaled_relative_skill")
  if (scaled_col %in% names(scored)) {
    skill_lookup <- scored |>
      dplyr::select(dplyr::any_of(c("model", group_vars, scaled_col))) |>
      dplyr::distinct()

    score_rows <- score_rows |>
      dplyr::select(-dplyr::all_of(output_col)) |>
      dplyr::left_join(skill_lookup, by = c("model", group_vars)) |>
      dplyr::rename(!!output_col := dplyr::all_of(scaled_col))
  }

  score_rows
}

summarize_retrospective_score_rows <- function(score_rows,
                                               group_vars = character(),
                                               reference_model = "Regular Baseline") {
  if (nrow(score_rows) == 0) {
    return(tibble::tibble())
  }

  mean_or_na <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }

    mean(x, na.rm = TRUE)
  }

  score_rows <- score_rows |>
    add_scoringutils_relative_skill(
      group_vars = group_vars,
      metric = "wis",
      output_col = "relative_wis",
      reference_model = reference_model
    ) |>
    add_scoringutils_relative_skill(
      group_vars = group_vars,
      metric = "log_wis",
      output_col = "relative_log_wis",
      reference_model = reference_model
    )

  score_rows |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "model")))) |>
    dplyr::summarize(
      mean_wis = mean_or_na(wis),
      mean_relative_wis = mean_or_na(relative_wis),
      mean_log_wis = mean_or_na(log_wis),
      mean_relative_log_wis = mean_or_na(relative_log_wis),
      coverage_50 = mean_or_na(covered_50),
      coverage_95 = mean_or_na(covered_95),
      n_forecast_targets = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_vars)), mean_wis, model)
}

score_retrospective_forecasts <- function(formatted_forecasts,
                                          actual_data,
                                          reference_model = "Regular Baseline") {
  empty_score_rows <- tibble::tibble(
    model = character(),
    reference_date = as.Date(character()),
    horizon = integer(),
    target_end_date = as.Date(character()),
    target_group = character(),
    actual = numeric(),
    wis = numeric(),
    relative_wis = numeric(),
    log_wis = numeric(),
    relative_log_wis = numeric(),
    weighted_interval_score_50 = numeric(),
    covered_50 = logical(),
    weighted_interval_score_95 = numeric(),
    covered_95 = logical()
  )

  if (is.null(formatted_forecasts) || nrow(formatted_forecasts) == 0) {
    return(list(
      rows = empty_score_rows,
      overall = tibble::tibble(),
      by_target_group = tibble::tibble(),
      by_forecast_date = tibble::tibble()
    ))
  }

  actual_tbl <- actual_data |>
    dplyr::transmute(
      target_end_date = as.Date(date),
      target_group,
      actual = value
    )

  quantile_tbl <- formatted_forecasts |>
    dplyr::mutate(
      reference_date = as.Date(reference_date),
      target_end_date = as.Date(target_end_date),
      output_type_id = as.character(output_type_id),
      quantile = as.numeric(output_type_id)
    ) |>
    dplyr::filter(output_type == "quantile", !is.na(quantile)) |>
    dplyr::left_join(actual_tbl, by = c("target_end_date", "target_group")) |>
    dplyr::filter(!is.na(actual))

  if (nrow(quantile_tbl) == 0) {
    return(list(
      rows = empty_score_rows,
      overall = tibble::tibble(),
      by_target_group = tibble::tibble(),
      by_forecast_date = tibble::tibble()
    ))
  }

  wis_tbl <- quantile_tbl |>
    dplyr::group_by(model, reference_date, horizon, target_end_date, target_group, actual) |>
    dplyr::summarize(
      wis = scoringutils::wis(
        observed = dplyr::first(actual),
        predicted = value,
        quantile_level = quantile,
        na.rm = TRUE
      ),
      log_wis = scoringutils::wis(
        observed = log1p(dplyr::first(actual)),
        predicted = log1p(value),
        quantile_level = quantile,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  interval_tbl <- quantile_tbl |>
    dplyr::filter(quantile %in% c(0.025, 0.25, 0.75, 0.975)) |>
    dplyr::select(
      model,
      reference_date,
      horizon,
      target_end_date,
      target_group,
      output_type_id,
      value,
      actual
    ) |>
    tidyr::pivot_wider(names_from = output_type_id, values_from = value, names_prefix = "q")

  missing_interval_cols <- setdiff(
    c("q0.025", "q0.25", "q0.75", "q0.975"),
    names(interval_tbl)
  )
  interval_tbl[missing_interval_cols] <- NA_real_

  if (nrow(interval_tbl) > 0) {
    interval_tbl <- interval_tbl |>
      dplyr::mutate(
        weighted_interval_score_50 = NA_real_,
        covered_50 = scoringutils::interval_coverage(
          observed = actual,
          predicted = cbind(q0.25, q0.75),
          quantile_level = c(0.25, 0.75),
          interval_range = 50
        ),
        weighted_interval_score_95 = NA_real_,
        covered_95 = scoringutils::interval_coverage(
          observed = actual,
          predicted = cbind(q0.025, q0.975),
          quantile_level = c(0.025, 0.975),
          interval_range = 95
        )
      )
  } else {
    interval_tbl <- interval_tbl |>
      dplyr::mutate(
        weighted_interval_score_50 = numeric(),
        covered_50 = logical(),
        weighted_interval_score_95 = numeric(),
        covered_95 = logical()
      )
  }

  score_rows <- wis_tbl |>
    dplyr::left_join(
      interval_tbl |>
        dplyr::select(
          model,
          reference_date,
          horizon,
          target_end_date,
          target_group,
          weighted_interval_score_50,
          covered_50,
          weighted_interval_score_95,
          covered_95
        ),
      by = c("model", "reference_date", "horizon", "target_end_date", "target_group")
    )

  score_rows <- score_rows |>
    dplyr::mutate(
      relative_wis = NA_real_,
      relative_log_wis = NA_real_
    ) |>
    dplyr::select(
      model,
      reference_date,
      horizon,
      target_end_date,
      target_group,
      actual,
      wis,
      relative_wis,
      log_wis,
      relative_log_wis,
      weighted_interval_score_50,
      covered_50,
      weighted_interval_score_95,
      covered_95
    )

  list(
    rows = score_rows,
    overall = summarize_retrospective_score_rows(score_rows, reference_model = reference_model),
    by_target_group = summarize_retrospective_score_rows(score_rows, "target_group", reference_model = reference_model),
    by_forecast_date = summarize_retrospective_score_rows(score_rows, "reference_date", reference_model = reference_model)
  )
}

retrospective_model_runners <- function(settings) {
  list(
    baseline_regular = list(
      label = "Regular Baseline",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_baseline_flat(
          df = train_data,
          weeks_ahead = horizon,
          quantiles_needed = quantiles_needed
        )
      }
    ),
    baseline_seasonal = list(
      label = "Seasonal Baseline",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_baseline_seasonal(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          seasonality = seasonality
        )
      }
    ),
    baseline_opt = list(
      label = "Opt Baseline",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_baseline_flat(
          df = train_data,
          weeks_ahead = horizon,
          quantiles_needed = quantiles_needed,
          window_size = 8
        )
      }
    ),
    inla = list(
      label = "INFLAenza",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_inla(
          df = train_data,
          weeks_ahead = horizon,
          quantiles_needed = quantiles_needed,
          forecast_uncertainty = settings$inla$forecast_uncertainty,
          use_offset = settings$inla$use_offset
        )
      }
    ),
    copycat = list(
      label = "Copycat",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_copycat(
          df = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          recent_weeks_touse = settings$copycat$recent_weeks_touse,
          resp_week_range = settings$copycat$resp_week_range,
          seasonality = seasonality,
          share_groups = settings$copycat$share_groups
        )
      }
    ),
    calcopycat = list(
      label = "CalCopycat",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_calcopycat(
          df = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          seasonality = seasonality,
          recent_weeks_touse = settings$calcopycat$recent_weeks_touse,
          resp_week_range = settings$calcopycat$resp_week_range,
          share_groups = settings$calcopycat$share_groups,
          ref_week_window = settings$calcopycat$ref_week_window,
          nsamps_cal = settings$calcopycat$nsamps_cal
        )
      }
    ),
    newgbqr = list(
      label = "newGBQR",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_newgbqr(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          num_bags = 50,
          bag_frac_samples = 0.7,
          nrounds = 100,
          seasonality = seasonality,
          model_type = settings$newgbqr$model_type
        )
      }
    ),
    fourcat = list(
      label = "FourCAT",
      run = function(train_data, horizon, quantiles_needed, seasonality) {
        fit_process_fourcat(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          zone = seasonality,
          seeds = settings$fourcat$seeds
        )
      }
    )
  )
}

write_retrospective_zip <- function(output_dir) {
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  zip_path <- paste0(output_dir, ".zip")
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dir.create(dirname(zip_path), recursive = TRUE, showWarnings = FALSE)
  setwd(output_dir)
  utils::zip(zipfile = zip_path, files = list.files(".", recursive = TRUE), flags = "-q")
  zip_path
}

run_retrospective_forecasts <- function(data,
                                        reference_dates,
                                        models,
                                        horizon,
                                        seasonality,
                                        quantiles_needed,
                                        output_dir,
                                        runners = NULL,
                                        progress_callback = NULL) {
  data <- data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::arrange(date)
  reference_dates <- sort(as.Date(reference_dates))
  horizon <- as.integer(horizon)

  if (length(reference_dates) == 0) {
    stop("Select at least one retrospective reference week.")
  }
  if (length(models) == 0) {
    stop("Select at least one model.")
  }
  if (length(horizon) != 1 || is.na(horizon) || horizon < 1) {
    stop("Forecast horizon must be a single positive integer.")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  settings <- retrospective_default_settings(has_population = "population" %in% names(data))
  if (is.null(runners)) {
    runners <- retrospective_model_runners(settings)
  }

  unknown_models <- setdiff(models, names(runners))
  if (length(unknown_models) > 0) {
    stop("Unknown retrospective model(s): ", paste(unknown_models, collapse = ", "))
  }

  success_rows <- list()
  failure_rows <- list()
  file_rows <- list()
  forecast_rows <- list()

  for (reference_date_index in seq_along(reference_dates)) {
    reference_date <- reference_dates[[reference_date_index]]
    if (is.function(progress_callback)) {
      progress_callback(reference_date_index, length(reference_dates), reference_date)
    }

    train_data <- data |>
      dplyr::filter(date < reference_date)
    weekly_results <- list()

    for (model_id in models) {
      runner <- runners[[model_id]]
      model_label <- runner$label

      result <- tryCatch(
        {
          raw_forecast <- runner$run(
            train_data = train_data,
            horizon = horizon,
            quantiles_needed = quantiles_needed,
            seasonality = seasonality
          )

          formatted <- format_retrospective_forecasts(
            forecast_df = raw_forecast,
            model_name = model_label,
            reference_date = reference_date
          )

          list(ok = TRUE, data = formatted, message = "")
        },
        error = function(e) {
          list(ok = FALSE, data = NULL, message = conditionMessage(e))
        }
      )

      if (isTRUE(result$ok)) {
        weekly_results[[model_id]] <- result$data
        success_rows[[length(success_rows) + 1]] <- tibble::tibble(
          reference_date = reference_date,
          model = model_label,
          rows = nrow(result$data)
        )
      } else {
        failure_rows[[length(failure_rows) + 1]] <- tibble::tibble(
          reference_date = reference_date,
          model = model_label,
          message = result$message
        )
      }
    }

    ensemble_result <- build_retrospective_nonbaseline_ensemble(weekly_results)
    if (!is.null(ensemble_result) && nrow(ensemble_result) > 0) {
      weekly_results[["ensemble"]] <- ensemble_result
      success_rows[[length(success_rows) + 1]] <- tibble::tibble(
        reference_date = reference_date,
        model = "Ensemble",
        rows = nrow(ensemble_result)
      )
    }

    weekly_output <- dplyr::bind_rows(weekly_results)
    if (nrow(weekly_output) > 0) {
      forecast_rows[[length(forecast_rows) + 1]] <- weekly_output

      csv_path <- file.path(
        output_dir,
        paste0("retrospective_", format(reference_date, "%Y-%m-%d"), ".csv")
      )
      readr::write_csv(weekly_output, csv_path)
      file_rows[[length(file_rows) + 1]] <- tibble::tibble(
        reference_date = reference_date,
        file = csv_path,
        rows = nrow(weekly_output)
      )
    }
  }

  failures <- dplyr::bind_rows(failure_rows)
  successes <- dplyr::bind_rows(success_rows)
  files <- dplyr::bind_rows(file_rows)
  forecasts <- dplyr::bind_rows(forecast_rows)
  if (ncol(successes) == 0) {
    successes <- tibble::tibble(
      reference_date = as.Date(character()),
      model = character(),
      rows = integer()
    )
  }
  if (ncol(failures) == 0) {
    failures <- tibble::tibble(
      reference_date = as.Date(character()),
      model = character(),
      message = character()
    )
  }
  if (ncol(files) == 0) {
    files <- tibble::tibble(
      reference_date = as.Date(character()),
      file = character(),
      rows = integer()
    )
  }
  if (ncol(forecasts) == 0) {
    forecasts <- tibble::tibble(
      model = character(),
      reference_date = as.Date(character()),
      horizon = integer(),
      target_end_date = as.Date(character()),
      target_group = character(),
      output_type = character(),
      output_type_id = character(),
      value = numeric()
    )
  }

  if (nrow(failures) > 0) {
    readr::write_csv(failures, file.path(output_dir, "retrospective_failures.csv"))
  }

  scores <- score_retrospective_forecasts(forecasts, data)

  if (nrow(scores$rows) > 0) {
    readr::write_csv(scores$rows, file.path(output_dir, "retrospective_score_rows.csv"))
  }
  if (nrow(scores$overall) > 0) {
    readr::write_csv(scores$overall, file.path(output_dir, "retrospective_score_overall.csv"))
  }
  if (nrow(scores$by_target_group) > 0) {
    readr::write_csv(scores$by_target_group, file.path(output_dir, "retrospective_score_by_target_group.csv"))
  }
  if (nrow(scores$by_forecast_date) > 0) {
    readr::write_csv(scores$by_forecast_date, file.path(output_dir, "retrospective_score_by_forecast_date.csv"))
  }

  zip_path <- write_retrospective_zip(output_dir)

  list(
    output_dir = output_dir,
    zip_path = zip_path,
    files = files,
    forecasts = forecasts,
    scores = scores,
    successes = successes,
    failures = failures
  )
}
