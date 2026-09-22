pargbqr_lgb_params <- function(seed, learning_rate, num_leaves,
                               min_data_in_leaf, feature_fraction) {
  list(
    objective = "quantile",
    alpha = 0.5,
    verbosity = -1,
    seed = seed,
    learning_rate = learning_rate,
    num_leaves = num_leaves,
    min_data_in_leaf = min_data_in_leaf,
    feature_fraction = feature_fraction
  )
}

pargbqr_cycle_week <- function(season_week, period = 53L) {
  week <- as.integer(season_week)
  period <- as.integer(period)
  ((week - 1L) %% period) + 1L
}

pargbqr_target_week <- function(season_week, horizon, period = 53L) {
  period <- as.integer(period)
  ((pargbqr_cycle_week(season_week, period) + as.integer(horizon) - 1L) %% period) + 1L
}

pargbqr_circular_week_distance <- function(week, reference_week, period = 53L) {
  period <- as.integer(period)
  abs(((week - reference_week + period / 2) %% period) - period / 2)
}

pargbqr_centered_residual_quantiles <- function(residuals, q_levels) {
  residuals <- residuals[is.finite(residuals)]
  if (length(residuals) == 0) {
    residuals <- 0
  }

  qs <- stats::quantile(residuals, probs = q_levels, na.rm = TRUE, names = FALSE, type = 8)
  qs - stats::quantile(residuals, probs = 0.5, na.rm = TRUE, names = FALSE, type = 8)
}

pargbqr_residual_quantile_offsets <- function(df_train,
                                              residuals,
                                              df_test,
                                              q_levels,
                                              season_week_window = 4L,
                                              min_residuals = 8L) {
  calibration <- tibble::tibble(
    horizon = as.integer(df_train$horizon),
    target_week = pargbqr_target_week(df_train$season_week, df_train$horizon),
    residual = as.numeric(residuals)
  ) |>
    dplyr::filter(
      is.finite(horizon),
      is.finite(target_week),
      is.finite(residual)
    )

  test_horizon <- as.integer(df_test$horizon)
  test_target_week <- pargbqr_target_week(df_test$season_week, df_test$horizon)

  offset_values <- vapply(seq_along(test_horizon), function(i) {
    same_horizon <- calibration$horizon == test_horizon[i]
    near_week <- pargbqr_circular_week_distance(
      calibration$target_week,
      test_target_week[i]
    ) <= season_week_window

    local_pool <- calibration$residual[same_horizon & near_week]
    horizon_pool <- calibration$residual[same_horizon]
    season_pool <- calibration$residual[near_week]

    if (length(local_pool) >= min_residuals) {
      pool <- local_pool
    } else if (length(horizon_pool) > 0) {
      pool <- horizon_pool
    } else if (length(season_pool) > 0) {
      pool <- season_pool
    } else {
      pool <- calibration$residual
    }

    pargbqr_centered_residual_quantiles(pool, q_levels)
  }, numeric(length(q_levels)))

  if (length(q_levels) == 1) {
    offsets <- matrix(offset_values, ncol = 1)
  } else {
    offsets <- t(offset_values)
  }

  offsets
}

pargbqr_delta_quantiles <- function(delta_hat, residual_offsets) {
  sweep(residual_offsets, 1, delta_hat, `+`)
}

run_pargbqr_multi_group <- function(split_data_list,
                                    feat_names,
                                    ref_date,
                                    num_bags,
                                    q_levels,
                                    bag_frac_samples,
                                    nrounds,
                                    learning_rate,
                                    num_leaves,
                                    min_data_in_leaf,
                                    feature_fraction,
                                    residual_week_window = 4L,
                                    min_residuals = 8L,
                                    progress_callback = NULL,
                                    lgb_dataset_fn = lightgbm::lgb.Dataset,
                                    lgb_train_fn = lightgbm::lgb.train,
                                    predict_fn = stats::predict) {
  rng_seed <- as.numeric(as.POSIXct(ref_date))
  set.seed(rng_seed)
  lgb_seeds <- matrix(
    floor(runif(num_bags * length(split_data_list), min = 1, max = 1e8)),
    nrow = num_bags,
    ncol = length(split_data_list)
  )

  all_preds <- list()
  completed_bags <- 0L
  total_bags <- length(split_data_list) * num_bags

  for (i in seq_along(split_data_list)) {
    group_data <- split_data_list[[i]]
    loc <- group_data$location
    tg <- group_data$target_group

    x_train_mat <- as.matrix(group_data$x_train)
    x_test_mat <- as.matrix(group_data$x_test)
    y_train <- group_data$y_train
    df_train <- group_data$df_train
    train_seasons <- unique(df_train$season)

    test_preds_by_bag <- array(NA_real_, dim = c(nrow(group_data$x_test), num_bags, length(q_levels)))

    for (b in seq_len(num_bags)) {
      bag_n <- max(1L, floor(length(train_seasons) * bag_frac_samples))
      bag_n <- min(bag_n, length(train_seasons))
      bag_seasons <- sample(train_seasons, size = bag_n, replace = FALSE)
      bag_obs_inds <- df_train$season %in% bag_seasons

      dtrain <- lgb_dataset_fn(
        data = x_train_mat[bag_obs_inds, , drop = FALSE],
        label = y_train[bag_obs_inds]
      )
      model <- lgb_train_fn(
        params = pargbqr_lgb_params(
          seed = lgb_seeds[b, i],
          learning_rate = learning_rate,
          num_leaves = num_leaves,
          min_data_in_leaf = min_data_in_leaf,
          feature_fraction = feature_fraction
        ),
        data = dtrain,
        nrounds = nrounds
      )

      # Calibrate residuals out-of-bag: predicting on the same rows a bag
      # was just trained on understates residual spread (in-sample residuals
      # from a fitted model are optimistically small). Season-block bagging
      # already holds out whole seasons per bag, so reuse those held-out rows
      # for honest calibration. Fall back to the in-bag rows only in the
      # degenerate case where a bag left no season out (e.g. bag_frac_samples = 1).
      oob_obs_inds <- !bag_obs_inds
      if (!any(oob_obs_inds)) {
        oob_obs_inds <- bag_obs_inds
      }

      oob_delta_hat <- predict_fn(model, newdata = x_train_mat[oob_obs_inds, , drop = FALSE])
      residual_offsets <- pargbqr_residual_quantile_offsets(
        df_train = df_train[oob_obs_inds, , drop = FALSE],
        residuals = y_train[oob_obs_inds] - oob_delta_hat,
        df_test = group_data$df_test,
        q_levels = q_levels,
        season_week_window = residual_week_window,
        min_residuals = min_residuals
      )
      test_delta_hat <- predict_fn(model, newdata = x_test_mat)

      test_preds_by_bag[, b, ] <- pargbqr_delta_quantiles(
        delta_hat = test_delta_hat,
        residual_offsets = residual_offsets
      )

      completed_bags <- completed_bags + 1L
      if (is.function(progress_callback)) {
        progress_callback(
          completed_bags,
          total_bags,
          paste0("Testing ", tg, " bag ", b, " of ", num_bags)
        )
      }
    }

    all_preds[[paste(loc, tg, sep = "___")]] <- test_preds_by_bag
  }

  all_preds
}

run_pargbqr_global <- function(split_data_list,
                               feat_names,
                               ref_date,
                               num_bags,
                               q_levels,
                               bag_frac_samples,
                               nrounds,
                               learning_rate,
                               num_leaves,
                               min_data_in_leaf,
                               feature_fraction,
                               residual_week_window = 4L,
                               min_residuals = 8L,
                               progress_callback = NULL,
                               lgb_dataset_fn = lightgbm::lgb.Dataset,
                               lgb_train_fn = lightgbm::lgb.train,
                               predict_fn = stats::predict) {
  all_tgs <- unique(sapply(split_data_list, function(g) as.character(g$target_group)))
  ohe_names <- make.unique(paste0("tg_", make.names(all_tgs)))
  full_feats <- c(feat_names, ohe_names)

  add_ohe <- function(x_df, tg) {
    for (i in seq_along(all_tgs)) {
      x_df[[ohe_names[i]]] <- as.numeric(tg == all_tgs[i])
    }
    x_df[, full_feats, drop = FALSE]
  }

  all_x_train <- dplyr::bind_rows(lapply(split_data_list, function(g) {
    add_ohe(as.data.frame(g$x_train), as.character(g$target_group))
  }))
  all_y_train <- unlist(lapply(split_data_list, function(g) g$y_train))
  all_df_train <- dplyr::bind_rows(lapply(split_data_list, function(g) g$df_train))
  all_x_train_mat <- as.matrix(all_x_train)

  group_keys <- names(split_data_list)
  test_x_mat_by_grp <- lapply(split_data_list, function(g) {
    as.matrix(add_ohe(as.data.frame(g$x_test), as.character(g$target_group)))
  })

  rng_seed <- as.numeric(as.POSIXct(ref_date))
  set.seed(rng_seed)
  lgb_seeds <- floor(runif(num_bags, min = 1, max = 1e8))

  all_preds <- lapply(split_data_list, function(g) {
    array(NA_real_, dim = c(nrow(g$x_test), num_bags, length(q_levels)))
  })
  names(all_preds) <- group_keys

  train_seasons <- unique(all_df_train$season)

  for (b in seq_len(num_bags)) {
    bag_n <- max(1L, min(floor(length(train_seasons) * bag_frac_samples), length(train_seasons)))
    bag_seasons <- sample(train_seasons, size = bag_n, replace = FALSE)
    bag_idx <- all_df_train$season %in% bag_seasons

    dtrain <- lgb_dataset_fn(
      data = all_x_train_mat[bag_idx, , drop = FALSE],
      label = all_y_train[bag_idx]
    )
    model <- lgb_train_fn(
      params = pargbqr_lgb_params(
        seed = lgb_seeds[b],
        learning_rate = learning_rate,
        num_leaves = num_leaves,
        min_data_in_leaf = min_data_in_leaf,
        feature_fraction = feature_fraction
      ),
      data = dtrain,
      nrounds = nrounds
    )

    # Calibrate residuals out-of-bag (see run_pargbqr_multi_group for why):
    # predict on the seasons this bag excluded rather than the ones it was
    # trained on, falling back to in-bag rows only if a bag held nothing out.
    oob_idx <- !bag_idx
    if (!any(oob_idx)) {
      oob_idx <- bag_idx
    }

    oob_delta_hat <- predict_fn(model, newdata = all_x_train_mat[oob_idx, , drop = FALSE])
    residual_offsets_by_group <- lapply(split_data_list, function(group_data) {
      pargbqr_residual_quantile_offsets(
        df_train = all_df_train[oob_idx, , drop = FALSE],
        residuals = all_y_train[oob_idx] - oob_delta_hat,
        df_test = group_data$df_test,
        q_levels = q_levels,
        season_week_window = residual_week_window,
        min_residuals = min_residuals
      )
    })

    for (i in seq_along(split_data_list)) {
      test_delta_hat <- predict_fn(model, newdata = test_x_mat_by_grp[[i]])
      all_preds[[group_keys[i]]][, b, ] <- pargbqr_delta_quantiles(
        delta_hat = test_delta_hat,
        residual_offsets = residual_offsets_by_group[[i]]
      )
    }

    if (is.function(progress_callback)) {
      progress_callback(
        b,
        num_bags,
        paste0("Testing bag ", b, " of ", num_bags)
      )
    }
  }

  all_preds
}

fit_process_pargbqr <- function(clean_data,
                                fcast_horizon = NULL,
                                quantiles_needed = NULL,
                                seasonality = NULL,
                                forecast_horizon = NULL,
                                q_levels = NULL,
                                country = "Paraguay",
                                peak_week_method = c("empirical", "zone", "fixed"),
                                peak_week = NULL,
                                num_bags = 50,
                                bag_frac_samples = 0.7,
                                nrounds = 100,
                                model_type = "individual",
                                rate_per = 100000,
                                min_train_rows = 12,
                                learning_rate = 0.05,
                                num_leaves = 11,
                                min_data_in_leaf = 8,
                                feature_fraction = 0.8,
                                residual_week_window = 4L,
                                min_residuals = 8L,
                                progress_callback = NULL,
                                data_type = "count") {
  if (is.null(fcast_horizon)) {
    fcast_horizon <- forecast_horizon
  }
  if (is.null(quantiles_needed)) {
    quantiles_needed <- q_levels
  }

  if (is.null(fcast_horizon) || is.null(quantiles_needed)) {
    stop("Provide forecast horizon and quantiles via fcast_horizon/quantiles_needed (or forecast_horizon/q_levels).")
  }

  if (!all(c("date", "target_group", "value") %in% names(clean_data))) {
    stop("clean_data must include: date, target_group, value")
  }

  forecast_horizon <- as.integer(fcast_horizon)
  if (length(forecast_horizon) != 1 || is.na(forecast_horizon) || forecast_horizon < 1) {
    stop("fcast_horizon must be a single positive integer.")
  }

  q_levels <- sort(unique(as.numeric(quantiles_needed)))
  q_levels <- q_levels[!is.na(q_levels)]
  if (length(q_levels) == 0) {
    stop("quantiles_needed must contain at least one numeric quantile.")
  }

  num_bags <- as.integer(num_bags)[1]
  if (is.na(num_bags)) {
    num_bags <- 50L
  }
  num_bags <- min(max(num_bags, 10L), 100L)

  gbqr_df <- wrangle_newgbqr_for_app(
    clean_data = clean_data,
    seasonality = seasonality,
    country = country,
    rate_per = rate_per,
    data_type = data_type
  )

  resolved_peak_week <- resolve_newgbqr_peak_week(
    df = gbqr_df,
    peak_week_method = peak_week_method,
    peak_week = peak_week,
    seasonality = seasonality
  )

  forecast_date <- max(as.Date(gbqr_df$wk_end_date)) + lubridate::weeks(1)
  feat_out <- preprocess_and_prepare_newgbqr_features(
    df = gbqr_df,
    forecast_date = forecast_date,
    data_to_drop = NULL,
    forecast_horizons = seq_len(forecast_horizon),
    peak_week = resolved_peak_week
  )

  filtered_targets <- filter_newgbqr_targets_for_training(
    target_df = feat_out$target_long,
    ref_date = forecast_date,
    drop_missing_targets = FALSE
  )

  split_data <- split_newgbqr_train_test(
    df_with_pred_targets = feat_out$target_long,
    feat_names = feat_out$feature_names,
    ref_date = forecast_date,
    filtered_targets = filtered_targets
  )

  if (length(split_data) == 0) {
    return(newgbqr_empty_forecast())
  }

  train_rows <- vapply(split_data, function(x) nrow(x$df_train), integer(1))
  use_global <- identical(model_type, "global") || any(train_rows < min_train_rows)
  global_train_rows <- sum(train_rows)

  if (use_global && global_train_rows < min_train_rows) {
    return(newgbqr_empty_forecast())
  }

  lgb_fn <- if (use_global) {
    run_pargbqr_global
  } else {
    run_pargbqr_multi_group
  }

  test_preds_by_group <- lgb_fn(
    split_data_list = split_data,
    feat_names = feat_out$feature_names,
    ref_date = forecast_date,
    num_bags = num_bags,
    q_levels = q_levels,
    bag_frac_samples = bag_frac_samples,
    nrounds = nrounds,
    learning_rate = learning_rate,
    num_leaves = num_leaves,
    min_data_in_leaf = min_data_in_leaf,
    feature_fraction = feature_fraction,
    residual_week_window = residual_week_window,
    min_residuals = min_residuals,
    progress_callback = progress_callback
  )

  process_and_combine_newgbqr_forecasts(
    test_preds_by_group = test_preds_by_group,
    split_data = split_data,
    q_labels = as.character(q_levels),
    rate_per = rate_per,
    data_type = data_type
  ) |>
    dplyr::mutate(
      output_type = "quantile",
      output_type_id = as.character(output_type_id)
    ) |>
    dplyr::arrange(target_group, horizon, output_type_id) |>
    dplyr::select(
      horizon,
      target_group,
      output_type,
      output_type_id,
      value
    )
}
