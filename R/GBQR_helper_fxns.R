########all helper functions needed
prepare_features_and_targets <- function(df, max_horizon, xmas_week, delta_offsets) {
  library(dplyr)
  library(slider)
  library(purrr)
  library(tibble)

  # Ensure 'location' is a factor
  df <- df %>% mutate(location = as.factor(location))

  # === Step 1: Assign offset value(s) per location+target_group as list-column ===
  df <- df %>%
    rowwise() %>%
    mutate(delta_offset_vals = list(delta_offsets[[as.character(location)]])) %>%
    ungroup()

  # === Step 2: Compute delta_xmas features ===
  if (length(xmas_week) == 1) {
    df <- df %>%
      mutate(
        delta_base_week = xmas_week - map_dbl(delta_offset_vals, 1),
        delta_xmas = season_week - delta_base_week
      ) %>%
      select(-delta_offset_vals)

    feat_names <- c("inc_4rt_cs", "season_week", "log_pop", "delta_xmas")

  } else if (length(xmas_week) == 2) {
    df <- df %>%
      mutate(
        delta_base_week_1 = xmas_week[1] - map_dbl(delta_offset_vals, 1),
        delta_base_week_2 = xmas_week[2] - map_dbl(delta_offset_vals, 2),
        delta_xmas_first_peak  = season_week - delta_base_week_1,
        delta_xmas_second_peak = season_week - delta_base_week_2
      ) %>%
      select(-delta_offset_vals, -delta_base_week_1, -delta_base_week_2)

    feat_names <- c("inc_4rt_cs", "season_week", "log_pop",
                    "delta_xmas_first_peak", "delta_xmas_second_peak")
  }

  # === Helper: Add Taylor features ===
  add_taylor_features <- function(df, degree, window_sizes, var = "inc_4rt_cs", feat_names) {
    for (w in window_sizes) {
      df <- df %>%
        group_by(location, target_group) %>%
        arrange(wk_end_date, .by_group = TRUE) %>%
        mutate(
          taylor_list = slide(
            .x = .data[[var]],
            .f = ~ {
              if (length(.x) < degree + 1 || all(is.na(.x))) return(rep(NA_real_, degree + 1))
              X <- outer(0:(length(.x) - 1), 0:degree, `^`)
              tryCatch(qr.solve(X, .x), error = function(e) rep(NA_real_, degree + 1))
            },
            .before = w - 1,
            .complete = TRUE
          )
        ) %>%
        ungroup()

      coef_cols <- paste0(var, "_taylor_d", degree, "_c", 0:degree, "_w", w, "t_sNone")
      coef_mat <- do.call(rbind, lapply(df$taylor_list, function(x) if (length(x) == degree + 1) x else rep(NA_real_, degree + 1)))
      df <- df %>% select(-taylor_list)
      df[coef_cols] <- as_tibble(coef_mat)
      feat_names <- c(feat_names, coef_cols)
    }
    list(df = df, feat_names = feat_names)
  }

  # === Helper: Add rolling mean features ===
  add_rollmean_features <- function(df, window_sizes, var = "inc_4rt_cs", feat_names) {
    for (w in window_sizes) {
      name <- paste0(var, "_rollmean_w", w)
      df <- df %>%
        group_by(location, target_group) %>%
        arrange(wk_end_date, .by_group = TRUE) %>%
        mutate(!!name := slide_dbl(.data[[var]], mean, .before = w - 1, .complete = TRUE)) %>%
        ungroup()
      feat_names <- c(feat_names, name)
    }
    list(df = df, feat_names = feat_names)
  }

  # === Helper: Add lag features ===
  add_lag_features <- function(df, lag_vars, lags, feat_names) {
    for (v in lag_vars) {
      for (l in lags) {
        name <- paste0(v, "_lag", l)
        df <- df %>%
          group_by(location, target_group) %>%
          arrange(wk_end_date, .by_group = TRUE) %>%
          mutate(!!name := lag(.data[[v]], n = l)) %>%
          ungroup()
        feat_names <- c(feat_names, name)
      }
    }
    list(df = df, feat_names = feat_names)
  }

  # === Step 3: Apply feature engineering ===
  res <- add_taylor_features(df, degree = 2, window_sizes = c(4, 6), feat_names = feat_names)
  df <- res$df; feat_names <- res$feat_names

  res <- add_taylor_features(df, degree = 1, window_sizes = c(3, 5), feat_names = feat_names)
  df <- res$df; feat_names <- res$feat_names

  res <- add_rollmean_features(df, window_sizes = c(2, 4), feat_names = feat_names)
  df <- res$df; feat_names <- res$feat_names

  intermediate_feats <- setdiff(feat_names, c("inc_4rt_cs", "season_week", "log_pop",
                                              "delta_xmas",
                                              "delta_xmas_first_peak", "delta_xmas_second_peak"))
  res <- add_lag_features(df, lag_vars = c("inc_4rt_cs", intermediate_feats), lags = c(1, 2), feat_names = feat_names)
  df <- res$df; feat_names <- res$feat_names

  # === Step 4: Create long-format multi-horizon targets ===
  horizons <- 1:max_horizon
  df_targets_long <- map_dfr(horizons, function(h) {
    df %>%
      group_by(location, target_group) %>%
      arrange(wk_end_date, .by_group = TRUE) %>%
      mutate(
        horizon = h,
        inc_4rt_cs_target = lead(inc_4rt_cs, h),
        delta_target = inc_4rt_cs_target - inc_4rt_cs
      ) %>%
      ungroup()
  })

  feat_names <- c(feat_names, "horizon")

  return(list(
    df_with_features = df,
    target_long = df_targets_long,
    feature_names = feat_names
  ))
}

preprocess_and_prepare_features <- function(df, forecast_date,
                                            data_to_drop = "1 week",
                                            forecast_horizons = 1:4,
                                            xmas_week,
                                            delta_offsets) {
  library(dplyr)
  library(lubridate)

  # Step 1: Drop config
  config <- switch(
    data_to_drop,
    "0 weeks" = list(days_before = 2, weeks_ahead = 4),
    "1 week"  = list(days_before = 2, weeks_ahead = 5),
    "2 week"  = list(days_before = 7, weeks_ahead = 6),
    stop("Invalid data_to_drop option")
  )

  # Step 2: Filter dropped data
  drop_start <- forecast_date - days(config$days_before)
  drop_end   <- forecast_date + weeks(config$weeks_ahead)

  df_filtered <- df %>%
    filter(!(wk_end_date >= drop_start & wk_end_date <= drop_end))

  message("⛔ Dropped data between: ", format(drop_start), " to ", format(drop_end))

  # Step 3: Dynamic max_horizon
  base_weeks_ahead <- config$weeks_ahead
  max_horizon <- base_weeks_ahead + 1

  # Update forecast_horizons accordingly
  forecast_horizons <- 1:max_horizon

  message("📏 max_horizon set to: ", max_horizon)

  # Step 4: Run feature engineering
  feat_out <- prepare_features_and_targets(
    df = df_filtered,
    max_horizon = max_horizon,
    xmas_week = xmas_week,
    delta_offsets = delta_offsets
  )

  return(feat_out)
}

filter_targets_for_training <- function(
    target_df,
    ref_date,
    in_season_weeks,
    drop_missing_targets = TRUE
) {
  library(dplyr)
  library(lubridate)
  library(purrr)

  df <- target_df

  # Step 1: Drop rows with NA targets if requested
  if (drop_missing_targets) {
    df <- df %>% filter(!is.na(inc_4rt_cs_target))
  }

  # Step 2: Filter by in-season weeks per location
  df <- df %>%
    rowwise() %>%
    filter({
      loc <- as.character(location)
      week <- season_week
      week_ranges <- in_season_weeks[[loc]]
      any(sapply(week_ranges, function(rng) week >= rng[1] & week <= rng[2]))
    }) %>%
    ungroup()

  # Step 3: Drop 'epiweek' column if present
  df <- df %>% select(-any_of("epiweek"))

  # Step 4: Filter by reference date
  df <- df %>% filter(wk_end_date < as.Date(ref_date))

  cat("✅ Filtered target dataframe using in_season_weeks\n")
  cat("  • Rows:", nrow(df), "Columns:", ncol(df), "\n")

  return(df)
}
split_train_test <- function(
    df_with_pred_targets,
    feat_names,
    ref_date,
    season_week_windows
) {
  library(dplyr)
  library(purrr)

  # ✅ Create named list of group keys
  group_keys <- df_with_pred_targets %>%
    distinct(location, target_group) %>%
    mutate(group_key = paste(location, target_group, sep = "___")) %>%
    split(.$group_key)

  # ✅ Get shared season window (assumed same for all groups)
  w <- season_week_windows[[1]]

  # ✅ Apply the split logic using names
  split_results <- imap(group_keys, function(g, group_key) {
    loc <- g$location
    tg <- g$target_group

    df_group <- df_with_pred_targets %>%
      filter(location == loc, target_group == tg)

    df_filtered <- df_group %>%
      filter(
        season_week >= w[1],
        season_week <= w[2],
        wk_end_date < as.Date(ref_date)
      )

    df_test <- df_filtered %>%
      filter(wk_end_date == max(wk_end_date, na.rm = TRUE))

    x_test <- df_test %>% select(all_of(feat_names))

    df_train <- df_filtered %>% filter(!is.na(delta_target))
    x_train <- df_train %>% select(all_of(feat_names))
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

  return(split_results)
}

run_quantile_lgb_bagging_multi_group <- function(split_data_list, feat_names, ref_date,
                                                 num_bags,
                                                 q_levels,
                                                 bag_frac_samples,
                                                 nrounds) {
  library(lightgbm)
  library(dplyr)
  library(lubridate)
  library(matrixStats)
  library(tibble)

  # Set reproducible seed
  rng_seed <- as.numeric(as.POSIXct(ref_date))
  set.seed(rng_seed)

  # Generate seeds for bagging and quantiles
  lgb_seeds <- matrix(
    floor(runif(num_bags * length(q_levels), min = 1, max = 1e8)),
    nrow = num_bags,
    ncol = length(q_levels)
  )

  # Storage for test predictions and importance across all groups
  all_preds <- list()
  all_importance <- list()

  for (i in seq_along(split_data_list)) {
    group_data <- split_data_list[[i]]
    loc <- group_data$location
    tg <- group_data$target_group

    cat("📍 Fitting for", loc, "-", tg, "\n")

    x_train <- group_data$x_train
    y_train <- group_data$y_train
    x_test <- group_data$x_test
    df_train <- group_data$df_train

    test_preds_by_bag <- array(NA_real_, dim = c(nrow(x_test), num_bags, length(q_levels)))
    feature_importance_df <- matrix(0, nrow = length(feat_names), ncol = num_bags * length(q_levels))
    rownames(feature_importance_df) <- feat_names

    # Available seasons
    train_seasons <- unique(df_train$season)

    for (b in seq_len(num_bags)) {
      cat("  └ Bag", b, "\n")

      bag_seasons <- sample(train_seasons, size = floor(length(train_seasons) * bag_frac_samples), replace = FALSE)
      bag_obs_inds <- df_train$season %in% bag_seasons

      for (q_ind in seq_along(q_levels)) {
        q_level <- q_levels[q_ind]
        col_index <- (b - 1) * length(q_levels) + q_ind

        # Train model
        dtrain <- lgb.Dataset(data = as.matrix(x_train[bag_obs_inds, ]), label = y_train[bag_obs_inds])
        model <- lgb.train(
          params = list(
            objective = "quantile",
            alpha = q_level,
            verbosity = -1,
            seed = lgb_seeds[b, q_ind]
          ),
          data = dtrain,
          nrounds = nrounds
        )

        # Predict test
        test_preds_by_bag[, b, q_ind] <- predict(model, newdata = as.matrix(x_test))

        # Feature importance
        importance <- lgb.importance(model)
        matched <- match(feat_names, importance$Feature)
        feature_importance_df[, col_index] <- ifelse(!is.na(matched), importance$Gain[matched], 0)
      }
    }

    # Save for this group
    all_preds[[paste(loc, tg, sep = "___")]] <- test_preds_by_bag
    all_importance[[paste(loc, tg, sep = "___")]] <- feature_importance_df
  }

  return(list(
    test_preds_by_group = all_preds,
    feature_importance_by_group = all_importance,
    lgb_seeds = lgb_seeds
  ))
}
plot_top_feature_importance <- function(feature_importance_df, top_n = 20) {
  library(dplyr)
  library(ggplot2)

  feature_importance_df <- as.data.frame(feature_importance_df)
  feature_importance_df$feature <- rownames(feature_importance_df)

  feature_importance_df <- feature_importance_df %>%
    rowwise() %>%
    mutate(average_importance = mean(c_across(where(is.numeric)), na.rm = TRUE)) %>%
    ungroup()

  top_features <- feature_importance_df %>%
    arrange(desc(average_importance)) %>%
    slice_head(n = top_n)

  ggplot(top_features, aes(x = average_importance, y = reorder(feature, average_importance))) +
    geom_bar(stat = "identity") +
    labs(
      title = paste0("Top ", top_n, " Most Important Features"),
      x = "Average Gain",
      y = "Feature"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.y = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold")
    )
}

process_and_combine_gbqr_forecasts <- function(
    test_preds_by_group,
    split_data,
    q_labels,
    ref_date
) {
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)

  all_forecasts <- map_dfr(names(test_preds_by_group), function(group_key) {
    parts <- strsplit(group_key, "___")[[1]]
    loc <- parts[1]
    tg <- parts[2]

    test_preds_by_bag <- test_preds_by_group[[group_key]]
    df_test <- split_data[[group_key]]$df_test

    test_pred_qs <- apply(test_preds_by_bag, c(1, 3), median, na.rm = TRUE)
    test_pred_qs_sorted <- t(apply(test_pred_qs, 1, sort))
    test_pred_qs_df <- as.data.frame(test_pred_qs_sorted)
    colnames(test_pred_qs_df) <- q_labels

    df_test <- df_test %>% mutate(row_id = row_number())
    df_test_w_preds <- bind_cols(df_test, test_pred_qs_df)

    preds_df <- df_test_w_preds %>%
      select(wk_end_date, location, target_group, pop, inc_4rt_cs, horizon,
             inc_4rt_center_factor, inc_4rt_scale_factor, all_of(q_labels)) %>%
      pivot_longer(cols = all_of(q_labels), names_to = "output_type_id", values_to = "delta_hat")

    preds_df <- preds_df %>%
      mutate(
        inc_4rt_cs_target_hat = inc_4rt_cs + delta_hat,
        inc_4rt_target_hat = (inc_4rt_cs_target_hat + inc_4rt_center_factor) * (inc_4rt_scale_factor + 0.01),
        value = ((pmax(inc_4rt_target_hat, 0) ^ 4 - 0.01) * pop) / 100000,
        value = pmax(value, 0)
      )

    # ✅ Remap last 5 horizons so that most recent = 0, next = 1, ..., oldest = -1
    recent_horizons <- preds_df %>%
      distinct(horizon, wk_end_date) %>%
      arrange(desc(wk_end_date)) %>%
      slice(1:5) %>%
      arrange(wk_end_date) %>%
      mutate(new_horizon = -1:3)

    preds_df <- preds_df %>%
      inner_join(recent_horizons, by = c("horizon", "wk_end_date")) %>%
      mutate(
        horizon = new_horizon,
        reference_date = ref_date,
        target_end_date = ref_date + weeks(horizon),
        output_type = "quantile"
      ) %>%
      select(reference_date, horizon, target_end_date,
             target_group, output_type, output_type_id, value)

    return(preds_df)
  })

  return(all_forecasts)
}




