# STArima model ===============================================================

starima_require_fable <- function() {
  required <- c("tsibble", "fable", "fabletools", "feasts")
  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) > 0) {
    stop(
      "STArima requires these packages: ",
      paste(missing, collapse = ", "),
      ". Install them with R/install-packages.R.",
      call. = FALSE
    )
  }

  suppressPackageStartupMessages({
    library(tsibble)
    library(fable)
    library(fabletools)
    library(feasts)
  })

  invisible(TRUE)
}

starima_as_tsibble <- function(data) {
  data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::arrange(target_group, date) |>
    dplyr::mutate(value = as.numeric(value)) |>
    tsibble::as_tsibble(
      index = date,
      key = target_group
    )
}

starima_compute_lambdas <- function(train_tsibble, lambda_data = NULL) {
  lambda_tsibble <- if (is.null(lambda_data) || nrow(lambda_data) == 0) {
    train_tsibble
  } else {
    starima_as_tsibble(lambda_data)
  }

  lambda_table <- lambda_tsibble |>
    fabletools::features(value, features = guerrero)

  stats::setNames(lambda_table$lambda_guerrero, lambda_table$target_group)
}

starima_simulate_one_group <- function(group_train,
                                       lambda,
                                       h,
                                       n_sims,
                                       min_stl_weeks = 104L,
                                       is_proportion = FALSE) {
  group_sd <- stats::sd(group_train$value, na.rm = TRUE)
  if (!is.finite(group_sd) || group_sd == 0) {
    return(tibble::tibble(
      target_group = group_train$target_group[[1]],
      date = rep(max(group_train$date) + seq_len(h) * 7L, each = n_sims),
      .sim = rep(group_train$value[[1]], h * n_sims)
    ))
  }

  # `group_train$value` has already been put on the logit scale upstream (in
  # fit_process_starima()) when is_proportion is TRUE, so the spec below fits
  # directly on it -- Box-Cox is skipped entirely rather than layered on top,
  # since Box-Cox has no notion of an upper bound and a proportion series
  # hovering near 1 could otherwise still be simulated above it.
  spec <- if (isTRUE(is_proportion)) {
    if (nrow(group_train) >= min_stl_weeks) {
      decomposition_model(
        STL(value ~ season(period = 52)),
        ARIMA(season_adjust)
      )
    } else {
      ARIMA(value)
    }
  } else if (nrow(group_train) >= min_stl_weeks) {
    decomposition_model(
      STL(box_cox(value, lambda) ~ season(period = 52)),
      ARIMA(season_adjust)
    )
  } else {
    ARIMA(box_cox(value, lambda))
  }

  group_fit <- group_train |>
    fabletools::model(m = spec)

  group_fit |>
    fabletools::generate(h = h, times = n_sims, bootstrap = TRUE) |>
    tibble::as_tibble()
}

starima_quantiles_from_sims <- function(sims, quantiles_needed, last_train_date, is_proportion = FALSE) {
  sims <- tibble::as_tibble(sims)

  if (isTRUE(is_proportion)) {
    # Invert the logit transform applied upstream; plogis() is already
    # bounded to (0, 1).
    sims <- dplyr::mutate(sims, .sim = plogis(.sim))
  }

  sims |>
    dplyr::group_by(target_group, date) |>
    dplyr::reframe(
      output_type_id = round(quantiles_needed, 3),
      value = stats::quantile(.sim, probs = quantiles_needed, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      horizon = as.integer(as.numeric(
        difftime(as.Date(date), as.Date(last_train_date), units = "days")
      ) / 7),
      output_type = "quantile",
      output_type_id = as.character(output_type_id),
      value = if (isTRUE(is_proportion)) pmin(pmax(value, 0), 1) else pmax(value, 0)
    ) |>
    dplyr::select(
      horizon,
      target_group,
      output_type,
      output_type_id,
      value
    )
}

fit_process_starima <- function(clean_data,
                                fcast_horizon,
                                quantiles_needed,
                                n_sim = 1000L,
                                min_stl_weeks = 104L,
                                base_seed = 12345L,
                                origin_date = NULL,
                                lambda_data = NULL,
                                data_type = "count") {
  starima_require_fable()

  is_proportion <- identical(data_type, "proportion")
  starima_logit_eps <- 1e-4

  clean_data <- clean_data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(!is.na(date), !is.na(value)) |>
    dplyr::arrange(target_group, date)

  if (is_proportion) {
    clean_data <- clean_data |>
      dplyr::mutate(value = qlogis(pmin(pmax(value, starima_logit_eps), 1 - starima_logit_eps)))
  }

  seed_date <- if (!is.null(origin_date)) {
    as.Date(origin_date)
  } else {
    max(clean_data$date, na.rm = TRUE)
  }
  if (!is.null(base_seed) && !is.na(seed_date)) {
    set.seed(base_seed + as.integer(seed_date))
  }

  train_tsibble <- starima_as_tsibble(clean_data)
  # Box-Cox lambda estimation is irrelevant in proportion mode since Box-Cox
  # itself is bypassed in starima_simulate_one_group() below.
  lambdas <- if (is_proportion) NULL else starima_compute_lambdas(train_tsibble, lambda_data = lambda_data)
  last_train_date <- max(clean_data$date, na.rm = TRUE)

  sims <- purrr::map(unique(train_tsibble$target_group), function(grp) {
    group_train <- train_tsibble |>
      dplyr::filter(target_group == grp)

      group_lambda <- if (is_proportion) NA_real_ else lambdas[[grp]]
      starima_simulate_one_group(
        group_train = group_train,
        lambda = group_lambda,
        h = fcast_horizon,
        n_sims = n_sim,
        min_stl_weeks = min_stl_weeks,
        is_proportion = is_proportion
      )
    }) |>
    dplyr::bind_rows()

  if (any(is.na(sims$.sim))) {
    warning("STArima generated NA simulation values; quantiles will ignore them.")
  }

  starima_quantiles_from_sims(
    sims = sims,
    quantiles_needed = quantiles_needed,
    last_train_date = last_train_date,
    is_proportion = is_proportion
  )
}
