######### newGBQR helper functions

newgbqr_cycle_week <- function(season_week) {
  pmin(as.integer(season_week), 52L)
}

newgbqr_circular_delta_peak <- function(season_week, peak_week, period = 52) {
  week <- newgbqr_cycle_week(season_week)
  peak <- as.numeric(peak_week)[1]
  ((week - peak + period / 2) %% period) - period / 2
}

prepare_newgbqr_features_and_targets <- function(df, forecast_horizons, peak_week) {
  forecast_horizons <- sort(unique(as.integer(forecast_horizons)))
  if (length(forecast_horizons) == 0 || any(is.na(forecast_horizons)) || any(forecast_horizons < 1)) {
    stop("forecast_horizons must be positive integers like 1:4")
  }

  peak_week <- as.numeric(peak_week)[1]
  if (is.na(peak_week)) {
    stop("peak_week must be numeric.")
  }

  df <- df |>
    dplyr::mutate(
      location = as.factor(location),
      cycle_week = newgbqr_cycle_week(season_week),
      delta_peak = newgbqr_circular_delta_peak(season_week, peak_week),
      season_week_sin = sin(2 * pi * cycle_week / 52),
      season_week_cos = cos(2 * pi * cycle_week / 52)
    )

  has_log_pop <- "log_pop" %in% names(df) && !all(is.na(df$log_pop))
  feat_names <- c(
    "inc_4rt_cs",
    "season_week",
    "delta_peak",
    "season_week_sin",
    "season_week_cos",
    if (has_log_pop) "log_pop"
  )

  add_taylor_features <- function(df, degree, window_sizes, var = "inc_4rt_cs", feat_names) {
    for (w in window_sizes) {
      df <- df |>
        dplyr::group_by(location, target_group) |>
        dplyr::arrange(wk_end_date, .by_group = TRUE) |>
        dplyr::mutate(
          taylor_list = slider::slide(
            .x = .data[[var]],
            .f = ~ {
              if (length(.x) < degree + 1 || all(is.na(.x))) return(rep(NA_real_, degree + 1))
              X <- outer(0:(length(.x) - 1), 0:degree, `^`)
              tryCatch(qr.solve(X, .x), error = function(e) rep(NA_real_, degree + 1))
            },
            .before = w - 1,
            .complete = TRUE
          )
        ) |>
        dplyr::ungroup()

      coef_cols <- paste0(var, "_taylor_d", degree, "_c", 0:degree, "_w", w, "t_sNone")
      coef_mat <- do.call(rbind, lapply(df$taylor_list, function(x) {
        if (length(x) == degree + 1) x else rep(NA_real_, degree + 1)
      }))

      df <- df |> dplyr::select(-taylor_list)
      df[coef_cols] <- tibble::as_tibble(coef_mat, .name_repair = "minimal")
      feat_names <- c(feat_names, coef_cols)
    }

    list(df = df, feat_names = feat_names)
  }

  add_rollmean_features <- function(df, window_sizes, var = "inc_4rt_cs", feat_names) {
    for (w in window_sizes) {
      name <- paste0(var, "_rollmean_w", w)

      df <- df |>
        dplyr::group_by(location, target_group) |>
        dplyr::arrange(wk_end_date, .by_group = TRUE) |>
        dplyr::mutate(!!name := slider::slide_dbl(.data[[var]], mean, .before = w - 1, .complete = TRUE)) |>
        dplyr::ungroup()

      feat_names <- c(feat_names, name)
    }

    list(df = df, feat_names = feat_names)
  }

  add_rollsd_features <- function(df, window_sizes, var = "inc_4rt_cs", feat_names) {
    for (w in window_sizes) {
      name <- paste0(var, "_rollsd_w", w)

      df <- df |>
        dplyr::group_by(location, target_group) |>
        dplyr::arrange(wk_end_date, .by_group = TRUE) |>
        dplyr::mutate(
          !!name := slider::slide_dbl(
            .data[[var]],
            ~ if (all(is.na(.x))) NA_real_ else stats::sd(.x, na.rm = TRUE),
            .before = w - 1,
            .complete = TRUE
          )
        ) |>
        dplyr::ungroup()

      feat_names <- c(feat_names, name)
    }

    list(df = df, feat_names = feat_names)
  }

  add_abs_change_features <- function(df, lags, var = "inc_4rt_cs", feat_names) {
    for (l in lags) {
      name <- paste0(var, "_abs_change", l)

      df <- df |>
        dplyr::group_by(location, target_group) |>
        dplyr::arrange(wk_end_date, .by_group = TRUE) |>
        dplyr::mutate(!!name := abs(.data[[var]] - dplyr::lag(.data[[var]], n = l))) |>
        dplyr::ungroup()

      feat_names <- c(feat_names, name)
    }

    list(df = df, feat_names = feat_names)
  }

  add_lag_features <- function(df, lag_vars, lags, feat_names) {
    for (v in lag_vars) {
      for (l in lags) {
        name <- paste0(v, "_lag", l)

        df <- df |>
          dplyr::group_by(location, target_group) |>
          dplyr::arrange(wk_end_date, .by_group = TRUE) |>
          dplyr::mutate(!!name := dplyr::lag(.data[[v]], n = l)) |>
          dplyr::ungroup()

        feat_names <- c(feat_names, name)
      }
    }

    list(df = df, feat_names = feat_names)
  }

  res <- add_taylor_features(df, degree = 2, window_sizes = c(4, 6), feat_names = feat_names)
  df <- res$df
  feat_names <- res$feat_names

  res <- add_taylor_features(df, degree = 1, window_sizes = c(3, 5), feat_names = feat_names)
  df <- res$df
  feat_names <- res$feat_names

  res <- add_rollmean_features(df, window_sizes = c(2, 4, 8), feat_names = feat_names)
  df <- res$df
  feat_names <- res$feat_names

  res <- add_rollsd_features(df, window_sizes = 4, feat_names = feat_names)
  df <- res$df
  feat_names <- res$feat_names

  res <- add_abs_change_features(df, lags = 1, feat_names = feat_names)
  df <- res$df
  feat_names <- res$feat_names

  intermediate_feats <- setdiff(
    feat_names,
    c(
      "inc_4rt_cs",
      "season_week",
      "delta_peak",
      "season_week_sin",
      "season_week_cos",
      "log_pop",
      "inc_4rt_cs_rollsd_w4",
      "inc_4rt_cs_abs_change1"
    )
  )

  res <- add_lag_features(
    df,
    lag_vars = c("inc_4rt_cs", intermediate_feats),
    lags = c(1, 2),
    feat_names = feat_names
  )

  df <- res$df
  feat_names <- res$feat_names

  df_targets_long <- purrr::map_dfr(forecast_horizons, function(h) {
    df |>
      dplyr::group_by(location, target_group) |>
      dplyr::arrange(wk_end_date, .by_group = TRUE) |>
      dplyr::mutate(
        horizon = h,
        inc_4rt_cs_target = dplyr::lead(inc_4rt_cs, h),
        delta_target = inc_4rt_cs_target - inc_4rt_cs
      ) |>
      dplyr::ungroup()
  })

  df_targets_long <- df_targets_long |>
    dplyr::mutate(
      horizon_sq = horizon^2,
      horizon_x_level = horizon * inc_4rt_cs,
      horizon_x_vol_sd4 = horizon * inc_4rt_cs_rollsd_w4
    )

  feat_names <- c(
    feat_names,
    "horizon",
    "horizon_sq",
    "horizon_x_level",
    "horizon_x_vol_sd4"
  )

  list(
    df_with_features = df,
    target_long = df_targets_long,
    feature_names = feat_names
  )
}

preprocess_and_prepare_newgbqr_features <- function(df, forecast_date = NULL,
                                                    data_to_drop = NULL,
                                                    forecast_horizons = 1:4,
                                                    peak_week) {
  forecast_horizons <- sort(unique(as.integer(forecast_horizons)))
  if (length(forecast_horizons) == 0 || any(is.na(forecast_horizons)) || any(forecast_horizons < 1)) {
    stop("forecast_horizons must be positive integers like 1:4")
  }

  if (is.null(data_to_drop) || is.null(forecast_date)) {
    df_filtered <- df
  } else {
    config <- switch(
      data_to_drop,
      "0 weeks" = list(days_before = 2, weeks_ahead = 4),
      "1 week" = list(days_before = 2, weeks_ahead = 5),
      "2 week" = list(days_before = 7, weeks_ahead = 6),
      stop("Invalid data_to_drop option")
    )

    drop_start <- as.Date(forecast_date) - lubridate::days(config$days_before)
    drop_end <- as.Date(forecast_date) + lubridate::weeks(config$weeks_ahead)

    df_filtered <- df |>
      dplyr::filter(!(wk_end_date >= drop_start & wk_end_date <= drop_end))
  }

  prepare_newgbqr_features_and_targets(
    df = df_filtered,
    forecast_horizons = forecast_horizons,
    peak_week = peak_week
  )
}

filter_newgbqr_targets_for_training <- function(
    target_df,
    ref_date,
    drop_missing_targets = TRUE
) {
  df <- target_df

  if (drop_missing_targets) {
    df <- df |> dplyr::filter(!is.na(inc_4rt_cs_target))
  }

  df |>
    dplyr::select(-dplyr::any_of("epiweek")) |>
    dplyr::filter(wk_end_date < as.Date(ref_date))
}

split_newgbqr_train_test <- function(
    df_with_pred_targets,
    feat_names,
    ref_date,
    filtered_targets = NULL
) {
  df_for_train <- if (!is.null(filtered_targets)) filtered_targets else df_with_pred_targets

  group_keys <- df_with_pred_targets |>
    dplyr::distinct(location, target_group) |>
    dplyr::mutate(group_key = paste(location, target_group, sep = "___"))

  group_keys <- split(group_keys, group_keys$group_key)

  split_results <- purrr::imap(group_keys, function(g, group_key) {
    loc <- g$location
    tg <- g$target_group

    df_all <- df_with_pred_targets |>
      dplyr::filter(
        location == loc,
        target_group == tg,
        wk_end_date < as.Date(ref_date)
      )

    if (nrow(df_all) == 0) return(NULL)

    df_test <- df_all |>
      dplyr::filter(wk_end_date == max(wk_end_date, na.rm = TRUE))

    if (nrow(df_test) == 0) return(NULL)

    x_test <- df_test |> dplyr::select(dplyr::all_of(feat_names))

    df_train <- df_for_train |>
      dplyr::filter(
        location == loc,
        target_group == tg,
        !is.na(delta_target)
      )

    if (nrow(df_train) == 0) return(NULL)

    x_train <- df_train |> dplyr::select(dplyr::all_of(feat_names))
    y_train <- df_train$delta_target

    list(
      location = loc,
      target_group = tg,
      df_train = df_train,
      x_train = x_train,
      y_train = y_train,
      df_test = df_test,
      x_test = x_test
    )
  })

  purrr::compact(split_results)
}

newgbqr_lgb_params <- function(q_level, seed, learning_rate, num_leaves,
                               min_data_in_leaf, feature_fraction) {
  list(
    objective = "quantile",
    alpha = q_level,
    verbosity = -1,
    seed = seed,
    learning_rate = learning_rate,
    num_leaves = num_leaves,
    min_data_in_leaf = min_data_in_leaf,
    feature_fraction = feature_fraction
  )
}

run_quantile_lgb_bagging_newgbqr_multi_group <- function(split_data_list, feat_names, ref_date,
                                                         num_bags,
                                                         q_levels,
                                                         bag_frac_samples,
                                                         nrounds,
                                                         learning_rate,
                                                         num_leaves,
                                                         min_data_in_leaf,
                                                         feature_fraction) {
  rng_seed <- as.numeric(as.POSIXct(ref_date))
  set.seed(rng_seed)

  lgb_seeds <- matrix(
    floor(runif(num_bags * length(q_levels), min = 1, max = 1e8)),
    nrow = num_bags,
    ncol = length(q_levels)
  )

  all_preds <- list()
  all_importance <- list()

  for (i in seq_along(split_data_list)) {
    group_data <- split_data_list[[i]]
    loc <- group_data$location
    tg <- group_data$target_group

    x_train <- group_data$x_train
    y_train <- group_data$y_train
    x_test <- group_data$x_test
    df_train <- group_data$df_train

    test_preds_by_bag <- array(NA_real_, dim = c(nrow(x_test), num_bags, length(q_levels)))
    feature_importance_df <- matrix(0, nrow = length(feat_names), ncol = num_bags * length(q_levels))
    rownames(feature_importance_df) <- feat_names

    train_seasons <- unique(df_train$season)

    for (b in seq_len(num_bags)) {
      bag_n <- max(1L, floor(length(train_seasons) * bag_frac_samples))
      bag_n <- min(bag_n, length(train_seasons))
      bag_seasons <- sample(train_seasons, size = bag_n, replace = FALSE)
      bag_obs_inds <- df_train$season %in% bag_seasons

      for (q_ind in seq_along(q_levels)) {
        q_level <- q_levels[q_ind]
        col_index <- (b - 1) * length(q_levels) + q_ind

        dtrain <- lightgbm::lgb.Dataset(data = as.matrix(x_train[bag_obs_inds, ]), label = y_train[bag_obs_inds])
        model <- lightgbm::lgb.train(
          params = newgbqr_lgb_params(
            q_level = q_level,
            seed = lgb_seeds[b, q_ind],
            learning_rate = learning_rate,
            num_leaves = num_leaves,
            min_data_in_leaf = min_data_in_leaf,
            feature_fraction = feature_fraction
          ),
          data = dtrain,
          nrounds = nrounds
        )

        test_preds_by_bag[, b, q_ind] <- predict(model, newdata = as.matrix(x_test))

        importance <- lightgbm::lgb.importance(model)
        matched <- match(feat_names, importance$Feature)
        feature_importance_df[, col_index] <- ifelse(!is.na(matched), importance$Gain[matched], 0)
      }
    }

    all_preds[[paste(loc, tg, sep = "___")]] <- test_preds_by_bag
    all_importance[[paste(loc, tg, sep = "___")]] <- feature_importance_df
  }

  list(
    test_preds_by_group = all_preds,
    feature_importance_by_group = all_importance,
    lgb_seeds = lgb_seeds
  )
}

run_quantile_lgb_bagging_newgbqr_global <- function(split_data_list, feat_names, ref_date,
                                                    num_bags, q_levels, bag_frac_samples,
                                                    nrounds, learning_rate, num_leaves,
                                                    min_data_in_leaf, feature_fraction) {
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

  group_keys <- names(split_data_list)
  test_x_by_grp <- lapply(split_data_list, function(g) {
    add_ohe(as.data.frame(g$x_test), as.character(g$target_group))
  })

  rng_seed <- as.numeric(as.POSIXct(ref_date))
  set.seed(rng_seed)
  lgb_seeds <- matrix(
    floor(runif(num_bags * length(q_levels), min = 1, max = 1e8)),
    nrow = num_bags,
    ncol = length(q_levels)
  )

  all_preds <- lapply(split_data_list, function(g) {
    array(NA_real_, dim = c(nrow(g$x_test), num_bags, length(q_levels)))
  })
  names(all_preds) <- group_keys

  train_seasons <- unique(all_df_train$season)

  for (b in seq_len(num_bags)) {
    bag_n <- max(1L, min(floor(length(train_seasons) * bag_frac_samples), length(train_seasons)))
    bag_seasons <- sample(train_seasons, size = bag_n, replace = FALSE)
    bag_idx <- all_df_train$season %in% bag_seasons

    for (q_ind in seq_along(q_levels)) {
      dtrain <- lightgbm::lgb.Dataset(
        data = as.matrix(all_x_train[bag_idx, ]),
        label = all_y_train[bag_idx]
      )
      model <- lightgbm::lgb.train(
        params = newgbqr_lgb_params(
          q_level = q_levels[q_ind],
          seed = lgb_seeds[b, q_ind],
          learning_rate = learning_rate,
          num_leaves = num_leaves,
          min_data_in_leaf = min_data_in_leaf,
          feature_fraction = feature_fraction
        ),
        data = dtrain,
        nrounds = nrounds
      )

      for (i in seq_along(split_data_list)) {
        all_preds[[group_keys[i]]][, b, q_ind] <-
          predict(model, newdata = as.matrix(test_x_by_grp[[i]]))
      }
    }
  }

  list(
    test_preds_by_group = all_preds,
    feature_importance_by_group = list(),
    lgb_seeds = lgb_seeds
  )
}

process_and_combine_newgbqr_forecasts <- function(
    test_preds_by_group,
    split_data,
    q_labels,
    rate_per = 100000
) {
  purrr::map_dfr(names(test_preds_by_group), function(group_key) {
    parts <- strsplit(group_key, "___")[[1]]
    loc <- parts[1]
    tg <- parts[2]

    test_preds_by_bag <- test_preds_by_group[[group_key]]
    df_test <- split_data[[group_key]]$df_test

    test_pred_qs <- apply(test_preds_by_bag, c(1, 3), stats::median, na.rm = TRUE)
    if (is.null(dim(test_pred_qs))) {
      test_pred_qs <- matrix(test_pred_qs, nrow = 1)
    }

    test_pred_qs_sorted <- t(apply(test_pred_qs, 1, sort))
    test_preds_qs_df <- as.data.frame(test_pred_qs_sorted)
    colnames(test_preds_qs_df) <- q_labels

    df_test_w_preds <- dplyr::bind_cols(df_test, test_preds_qs_df)

    preds_df <- df_test_w_preds |>
      dplyr::select(
        wk_end_date, location, target_group, population, uses_population,
        inc_4rt_cs, horizon, inc_4rt_center_factor, inc_4rt_scale_factor,
        dplyr::all_of(q_labels)
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(q_labels),
        names_to = "output_type_id",
        values_to = "delta_hat"
      )

    preds_df$output_type_id <- as.character(preds_df$output_type_id)
    preds_df$inc_4rt_cs_target_hat <- preds_df$inc_4rt_cs + preds_df$delta_hat
    preds_df$inc_4rt_target_hat <- (preds_df$inc_4rt_cs_target_hat + preds_df$inc_4rt_center_factor) *
      (preds_df$inc_4rt_scale_factor + 0.01)
    preds_df$value <- pmax(preds_df$inc_4rt_target_hat, 0)^4 - 0.01 - 0.75^4
    preds_df$value <- dplyr::if_else(
      preds_df$uses_population,
      preds_df$value * preds_df$population / rate_per,
      preds_df$value
    )
    preds_df$value <- pmax(preds_df$value, 0)
    preds_df$horizon <- as.integer(preds_df$horizon)
    preds_df$output_type <- "quantile"

    preds_df |>
      dplyr::select(horizon, target_group, output_type, output_type_id, value) |>
      dplyr::arrange(target_group, horizon, output_type_id)
  })
}
