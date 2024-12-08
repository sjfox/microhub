##############
#Format quantiles
#############

format_quantiles <- function(pred_quants, forecast_date) {
  #pred_quants %>%
  pred_quants <- pred_quants %>%
    filter(horizon != -1) %>%
    rename(target_end_date = date, output_type_id = quant_level) %>%
    mutate(
      target_end_date = as.Date(target_end_date),       # Convert to Date
      forecast_date = as.Date(forecast_date)           # Ensure forecast_date is Date
    ) %>%
    filter(target_end_date >= forecast_date) |>
    mutate(
      value = round(value),
      #target_end_date = target_end_date + days(data_to_drop),
      reference_date = forecast_date,
      output_type = "quantile",
      target = "inc sari hosp",
      age_group = str_to_title(age_group),
      horizon = as.numeric(as.factor(target_end_date)) - 1,
      output_type_id = format(as.numeric(sub("%", "", output_type_id)) / 100, trim = TRUE)
    ) %>%
    select(reference_date, target, horizon, target_end_date, age_group, output_type, output_type_id, value) %>%
    mutate(
      output_type_id = as.numeric(output_type_id),
      output_type_id = gsub("0+$", "", gsub("\\.$", "", sprintf("%.3f", output_type_id)))
    )
}

fit_and_process_SIRSea <- function(dataframe, stan_dat, forecast_date, data_to_drop) {
  # Load required libraries
  library(tidyverse)
  library(lubridate)
  library(cmdstanr)
  library(posterior)
  library(ggtext)
  
  dataframe <- dataframe %>%
    mutate(date = as.Date(date))
  
  config <- switch(data_to_drop,
                   "1 week" = list(days_before = 2, weeks_ahead = 5),
                   "2 week" = list(days_before = 7, weeks_ahead = 6),
                   "3 week" = list(days_before = 14, weeks_ahead = 7),
                   stop("Invalid data_to_drop option"))
  
  weeks_ahead <- config$weeks_ahead
  
  # Set the CmdStan path
  set_cmdstan_path("/Users/maya/Documents/cmdstan-2.35.0")
  # Fit the model with MCMC
  exec <- cmdstan_model("/Users/maya/Desktop/SIRSea-model-paraguay-main/sirsea2.stan") #will include file when passing over code
  
  fit <- exec$sample(
    data = stan_dat,
    chains = 8,
    parallel_chains = 8,
    iter_sampling = 1000,
    iter_warmup = 10000,
    adapt_delta = 0.9
  )
  
  # Summarize fit results
  (fit_summ <- fit$summary()) # want rhat all <1.01
  post <- as_draws_df(fit$draws())
  
  # Process posterior predictions
  last_date <- as.Date(max(dataframe$date))
  
  quantiles_needed <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  
  post_pred <- post %>%
    mutate(draw =.draw) %>%
    select(contains("yhat"), draw) %>%
    pivot_longer(-draw, values_to = "pred_count") %>%
    mutate(
      age_group = ifelse(str_extract(name, "\\d+(?=,)") == "1", "Pediatric", "Adult"),
      age_group = fct_relevel(age_group, "Pediatric"),
      t = as.integer(str_extract(name, "(?<=,)\\d+")),
      .keep = "unused", .before = pred_count
    ) %>%
    arrange(draw, age_group, t) %>%
    group_by(draw, age_group) %>%
    #mutate(date = c(unique(dataframe$date), last_date + weeks(1:weeks_ahead))) %>% #5, 6, 
    #mutate(date = c(unique(dataframe$date), as.Date(last_date + weeks(1:weeks_ahead)))) %>%
    mutate(date = c(unique(dataframe$date), seq(last_date + 7, by = "1 week", length.out = weeks_ahead))) %>%
    ungroup()
  
  # Summarize predictions
  post_pred_summ <- post_pred %>%
    filter(t > 1) %>%
    group_by(age_group, date) %>%
    summarize(
      mean = mean(pred_count),
      qs = list(value = quantile(pred_count, c(0.025, 0.25, 0.5, 0.75, 0.975))),
      .groups = "drop"
    ) %>%
    unnest_wider(qs) %>%
    left_join(dataframe, by = c("age_group", "date"))
  
  # Combine overall predictions
  pp_overall <- post_pred %>%
    group_by(draw, date) %>%
    summarise(pred_count = sum(pred_count), .groups = "drop") %>%
    mutate(age_group = "Overall")
  
  post_pred_quants <- bind_rows(post_pred, pp_overall) %>%
    filter(date > last_date) %>%
    mutate(horizon = as.numeric(as.factor(date)) - 1) %>%
    group_by(age_group, date, horizon) %>%
    reframe(enframe(quantile(pred_count, quantiles_needed), "quant_level")) %>%
    arrange(date, age_group)
  
  post_pred_quants <- format_quantiles(post_pred_quants, forecast_date)
  
  # Return results
  return(post_pred_quants)
  
}