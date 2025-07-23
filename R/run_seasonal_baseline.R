# ===============================
# Seasonal Baseline Forecasting
source("R/seasonal_baseline_hlpr_fxns.R")
# ===============================
# --- 4. Run for a Single Date ---
forecast_date <- as.Date("2024-06-29")
raw_data <- read_csv("raw-data/microhub-data-template.csv")
clean_data <- wrangle_seasonal_baseline(raw_data, forecast_date)
forecast_df <- fit_process_seasonal_baseline(clean_data, forecast_date, data_to_drop = "1 week")
save_seasonal_baseline_forecast_outputs(forecast_df, forecast_date, output_dir = "output/seasonal_baseline")

