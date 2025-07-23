

# Params
dataframe <- "raw-data/microhub-data-template.csv"
forecast_date <- "2024-06-29"
data_to_drop <- "1 week"
forecast_horizons <- 5


source("R/opt_baseline_main_fxns.R")

output_dir <- "output/opt_baseline_forecast"

# Step 1: Wrangle
baseline_data <- wrangle_baseline(dataframe, forecast_date, data_to_drop)

baseline_forecast <- fit_process_baseline(
  target_edf = baseline_data$target_edf,
  forecast_date = baseline_data$forecast_date,
  desired_max_time_value = baseline_data$desired_max_time_value,
  base_weeks_ahead = baseline_data$base_weeks_ahead,
  forecast_horizons = 5,
  output_dir = output_dir
)


