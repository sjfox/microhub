#####################################################
wrangle_function <- function(raw_data,
                             population_df,
                             forecast_date,
                             country = "Paraguay",
                             in_season_weeks = list("Paraguay" = list(c(8, 50))),
                             transform = TRUE) {
  library(dplyr)
  library(lubridate)

  df <- raw_data %>%
    mutate(date = mdy(date)) %>%
    rename(wk_end_date = date) %>%
    mutate(
      epiweek = isoweek(wk_end_date),
      year = year(wk_end_date),
      season = year(wk_end_date),
      season_week = isoweek(wk_end_date)
    ) %>%
    left_join(population_df, by = c("year", "target_group")) %>%
    rename(pop = population) %>%
    mutate(
      location = country,
      inc = value / (pop / 100000)
    ) %>%
    select(wk_end_date, location, target_group, inc, pop, season, season_week)

  if (transform) {
    if (is.null(in_season_weeks)) stop("`in_season_weeks` must be provided when `transform = TRUE`")

    df <- df %>%
      mutate(
        log_pop = log(pop),
        inc_4rt = (inc + 0.01)^0.25
      ) %>%
      rowwise() %>%
      mutate(in_season = {
        loc_weeks <- in_season_weeks[[location]]
        if (is.null(loc_weeks)) FALSE
        else any(sapply(loc_weeks, function(r) season_week >= r[1] & season_week <= r[2]))
      }) %>%
      ungroup() %>%
      group_by(location, target_group) %>%
      mutate(
        inc_4rt_scale_factor = quantile(inc_4rt[in_season], 0.95, na.rm = TRUE),
        inc_4rt_cs = inc_4rt / (inc_4rt_scale_factor + 0.01),
        inc_4rt_center_factor = mean(inc_4rt_cs[in_season], na.rm = TRUE),
        inc_4rt_cs = inc_4rt_cs - inc_4rt_center_factor
      ) %>%
      ungroup()
  }

  df
}


fit_and_process_gbqr <- function(clean_data,
                                 forecast_date,
                                 in_season_weeks,
                                 season_week_windows,
                                 forecast_horizon = 4,          # <--- NEW
                                 data_to_drop = "2 week",
                                 xmas_week = 25,
                                 delta_offsets = list("Paraguay" = 3),
                                 q_levels = c(0.025, 0.5, 0.75),
                                 num_bags = 5,
                                 bag_frac_samples = 0.7,
                                 nrounds = 5,
                                 show_feature_importance = FALSE, # <--- NEW
                                 output_dir = "output/gbqr_forecasts") {
  library(readr)
  library(dplyr)

  if (!is.numeric(forecast_horizon) || length(forecast_horizon) != 1 || is.na(forecast_horizon) || forecast_horizon < 1) {
    stop("forecast_horizon must be a single positive integer (e.g., 4).")
  }
  forecast_horizon <- as.integer(forecast_horizon)

  # Step 1: Preprocess & Feature Engineering
  feat_out <- preprocess_and_prepare_features(
    df = clean_data,
    forecast_date = forecast_date,
    data_to_drop = data_to_drop,
    forecast_horizons = 1:forecast_horizon,  # <--- key fix
    xmas_week = xmas_week,
    delta_offsets = delta_offsets
  )

  # Step 2: Filter Targets
  filtered_targets <- filter_targets_for_training(
    target_df = feat_out$target_long,
    ref_date = forecast_date,
    in_season_weeks = in_season_weeks
  )

  # Step 3: Split Train-Test
  split_data <- split_train_test(
    df_with_pred_targets = filtered_targets,
    feat_names = feat_out$feature_names,
    ref_date = forecast_date,
    season_week_windows = season_week_windows
  )

  # Step 4: Train GBQR per group
  lgb_results <- run_quantile_lgb_bagging_multi_group(
    split_data_list = split_data,
    feat_names = feat_out$feature_names,
    ref_date = forecast_date,
    num_bags = num_bags,
    q_levels = q_levels,
    bag_frac_samples = bag_frac_samples,
    nrounds = nrounds
  )

  # Optional: Plot feature importance (non-interactive by default)
  if (isTRUE(show_feature_importance)) {
    group_keys <- names(lgb_results$feature_importance_by_group)
    for (group_key in group_keys) {
      cat("🔍 Feature importance for:", group_key, "\n")
      feature_df <- lgb_results$feature_importance_by_group[[group_key]]
      p <- plot_top_feature_importance(feature_df, top_n = 20)
      print(p)
    }
  }

  # Step 5: Combine Forecasts (horizon-correct)
  all_forecasts_df <- process_and_combine_gbqr_forecasts(
    test_preds_by_group = lgb_results$test_preds_by_group,
    split_data = split_data,
    q_labels = as.character(q_levels),
    ref_date = forecast_date
  )

  # Step 6: Save Forecasts
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_file <- file.path(output_dir, paste0(format(forecast_date), "-GBQR.csv"))
  write_csv(all_forecasts_df, output_file)

  cat("✅ Final forecast saved with", nrow(all_forecasts_df), "rows at", output_file, "\n")

  all_forecasts_df
}
##################################
