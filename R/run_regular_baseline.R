# Params
dataframe <- "raw-data/microhub-data-template.csv"
forecast_date <- "2024-06-29"
data_to_drop <- "1 week"
forecast_horizons <- 5

source("R/regular_baseline_main_fxns.R")

# Step 1: Wrangle
regular_baseline_data <- wrangle_baseline(dataframe, forecast_date, data_to_drop)

# Output directory for regular baseline forecasts
output_dir <- "output/regular_baseline_forecast"

# Step 2: Fit and Process Forecasts
regular_baseline_forecast <- fit_process_baseline(
  target_edf = regular_baseline_data$target_edf,
  forecast_date = regular_baseline_data$forecast_date,
  desired_max_time_value = regular_baseline_data$desired_max_time_value,
  base_weeks_ahead = regular_baseline_data$base_weeks_ahead,
  forecast_horizons = forecast_horizons,
  output_dir = output_dir
)
