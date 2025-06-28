
############
#Wrangle
#############

wrangle_baseline <- function(dataframe,
                             forecast_date
                             ){
  library(dplyr)
  library(readr)
  library(lubridate)
  library(epiprocess)
  
  forecast_date <- as.Date(forecast_date)
  
  reference_date <- forecast_date
  
  # Determine config from data_to_drop
  config <- switch(
    data_to_drop,
    "0 weeks" = list(days_before = 0, weeks_ahead = 4),
    "1 week"  = list(days_before = 4, weeks_ahead = 5),
    "2 week"  = list(days_before = 11, weeks_ahead = 6),
    stop("Invalid data_to_drop option")
  )
  
  days_before <- config$days_before
  
  # Read and filter the target data
  target_tbl <- read_csv(dataframe,
                         col_types = cols_only(
                           year = col_double(),
                           week = col_double(),
                           date = col_date(),
                           age_group = col_character(),
                           inc_sari_hosp = col_double()
                         )) %>%
    filter(age_group %in% c("Pediatric", "Adult", "Overall"))
  
  # Drop recent data based on config
  target_tbl <- target_tbl %>%
    mutate(date = as.Date(date)) %>%
    filter(date < ymd(forecast_date) - days(days_before))
  
  # Convert to epi_df format
  target_edf <- target_tbl %>%
    transmute(
      geo_value = age_group,
      time_value = date,
      weekly_count = inc_sari_hosp
    ) %>%
    as_epi_df()
  
  # Set max usable date
  desired_max_time_value <- reference_date - 7L
  
  list(
    target_edf = target_edf,
    forecast_date = forecast_date,
    desired_max_time_value = desired_max_time_value,
    base_weeks_ahead = config$weeks_ahead
  )
}

#############
#run
###########

fit_process_baseline <- function(target_edf, forecast_date, desired_max_time_value, base_weeks_ahead) {
  library(epipredict)
  library(withr)
  library(tidyr)
  library(dplyr)
  
  forecast_date <- as.Date(forecast_date)
  
  reference_date <- forecast_date
  
  # Adjust forecast horizon if necessary
  weeks_ahead <- base_weeks_ahead + (as.numeric(forecast_horizons) - 4)
  
  rng_seed <- as.integer((59460707 + as.numeric(reference_date)) %% 2e9)
  
  fcst <- with_rng_version("4.0.0", with_seed(rng_seed, {
    epipredict::cdc_baseline_forecaster(
      target_edf %>% filter(time_value <= desired_max_time_value),
      "weekly_count",
      epipredict::cdc_baseline_args_list(
        aheads = seq_len(weeks_ahead),
        nsims = 1e5
      )
    )
  }))
  
  preds <- fcst$predictions %>%
    mutate(
      forecast_date = reference_date,
      ahead = as.integer(target_date - reference_date) %/% 7L
    )
  
  preds_formatted <- preds %>%
    pivot_quantiles_longer(.pred_distn) %>%
    mutate(
      target = "inc sari hosp",
      output_type = "quantile",
      age_group = factor(geo_value, levels = c("Pediatric", "Adult", "Overall"))
    ) %>%
    rename(
      reference_date = forecast_date,
      target_end_date = target_date,
      horizon = ahead,
      output_type_id = quantile_levels,
      value = values
    ) %>%
    arrange(target, horizon, age_group) %>%
    select(
      reference_date, horizon, target, target_end_date, age_group,
      output_type, output_type_id, value
    )
  
  preds_formatted
}

############
#test space
#############

# Global variables
dataframe <- "/Users/maya/Documents/Paraguay_2024Jul/Shiny_app/test_data.csv"
forecast_date <- "2024-06-29"
data_to_drop <- "1 week"
forecast_horizons <- 5

# Step 1: Wrangle the data
baseline_data <- wrangle_baseline(dataframe, forecast_date)

# Step 2: Fit and process
baseline_preds <- fit_process_baseline(
  baseline_data$target_edf,
  baseline_data$forecast_date,
  baseline_data$desired_max_time_value,
  baseline_data$base_weeks_ahead
)