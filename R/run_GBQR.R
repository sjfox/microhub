# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)

#  Load input data
raw_data <- read_csv("raw-data/microhub-data-template.csv")
population_df <- read_csv("raw-data/Population_Data.csv")

#source main and helper fxns
source("R/GBQR_main_fxns.R")
source("R/GBQR_helper_fxns.R")

# Define forecast date once
forecast_date <- as.Date("2024-06-29")
#forecast_date <- Sys.Date() #eventually


# Wrangle data
clean_data <- wrangle_function(
  raw_data = raw_data,
  population_df = population_df,
  forecast_date = forecast_date,
  country = "Paraguay",
  in_season_weeks = list("Paraguay" = list(c(8, 50))),
  transform = TRUE,
  plot = TRUE
)

# Fit and process GBQR
forecast_df <- fit_and_process_gbqr(
  clean_data = clean_data,
  forecast_date = forecast_date,
  in_season_weeks = list("Paraguay" = list(c(8, 50))),
  season_week_windows = list(c(8, 50)),
  q_levels = sort(unique(c(0.01, 0.025, 0.05, seq(0.1, 0.9, by = 0.05), 0.95, 0.975, 0.99))),
  num_bags = 10,
  bag_frac_samples = 0.7,
  nrounds = 10
)
