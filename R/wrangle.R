###################
#INLA wrangle 
###################

#Main wrangle function to call #######################
wrangle_INLA <- function(dataframe, forecast_date, data_to_drop, forecast_horizons) {
  # Ensure the required libraries are loaded
  library(tidyverse)
  library(lubridate)
  
  # Preprocess the data
  data_preprocessed <- dataframe %>%
    rename(
      epiweek = week,
      count = inc_sari_hosp
    ) %>%
    filter(year > 2021) %>%
    filter(age_group %in% c('Pediatric', 'Adult', 'Overall'))
  
  # Add population column
  data_with_population <- add_population_column("data/Population_Data.csv", data_preprocessed, 2023)
  
  # Determine configuration based on selected data to drop option
  config <- switch(data_to_drop,
                   "1 week" = list(days_before = 2, weeks_ahead = 5),
                   "2 week" = list(days_before = 7, weeks_ahead = 6),
                   "3 week" = list(days_before = 14, weeks_ahead = 7),
                   stop("Invalid data_to_drop option"))
  
  days_before <- config$days_before
  base_weeks_ahead <- config$weeks_ahead
  
  # Adjust weeks_ahead based on forecast horizons
  weeks_ahead <- base_weeks_ahead + (as.numeric(forecast_horizons) - 4)
  
  # Final data wrangling
  wrangled_data <- data_with_population %>%
    mutate(date = as.Date(date)) %>%
    filter(date < ymd(forecast_date) - days_before)
  
  fit_df <- prep_fit_data(wrangled_data, weeks_ahead) 
  
  return(fit_df)
}

#support function 1 #######################################
#prep_pop_data<
add_population_column <- function(file_path, data_frame, year) {
  # Read the population data from the specified file
  pop_data <- read_csv(file_path)
  
  # Define the new column names
  new_names <- c('Name', 'Code', 'Age', 'Code2', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023')
  
  # Rename the columns
  colnames(pop_data)[1:length(new_names)] <- new_names
  
  # Define age groups
  under_20_groups <- c('Population ages 0-14, total', 'Population ages 15-19, female', 'Population ages 15-19, male')
  over_20_groups <- c('Population ages 20-24, female', 'Population ages 20-24, male', 'Population ages 25-29, female', 'Population ages 25-29, male', 'Population ages 30-34, female', 'Population ages 30-34, male', 'Population ages 35-39, female', 'Population ages 35-39, male', 'Population ages 40-44, female', 'Population ages 40-44, male', 'Population ages 45-49, female', 'Population ages 45-49, male', 'Population ages 50-54, female', 'Population ages 50-54, male', 'Population ages 55-59, female', 'Population ages 55-59, male', 'Population ages 60-64, female', 'Population ages 60-64, male', 'Population ages 65 and above, female', 'Population ages 65 and above, male')
  
  # Calculate the population sums for the specified year
  sum_under_20 <- pop_data %>%
    filter(Age %in% under_20_groups) %>%
    summarise(Sum = sum(as.numeric(.data[[as.character(year)]]))) %>%
    pull(Sum)
  
  sum_over_20 <- pop_data %>%
    filter(Age %in% over_20_groups) %>%
    summarise(Sum = sum(as.numeric(.data[[as.character(year)]]))) %>%
    pull(Sum)
  
  sum_total <- sum_under_20 + sum_over_20
  
  # Update the input data frame with the new population data
  data_frame <- data_frame %>%
    mutate(population = case_when(
      age_group == "Pediatric" ~ sum_under_20,
      age_group == "Adult" ~ sum_over_20,
      age_group == "Overall" ~ sum_total,
      TRUE ~ NA_real_  # Assign NA for other cases
    ))
  
  return(data_frame)
}

#Support function 2 ###############################
#Prep fit data 
prep_fit_data <- function(input_data, weeks_ahead = weeks_ahead) {
  input_data$date <- as.Date(input_data$date)
  
  ret <- input_data |> 
    filter(age_group != "Overall") |>
    group_by(date) |> 
    mutate(t=cur_group_id(), .after=date) |> # add a time counter starting from 1 for earliest week
    ungroup() |> 
    mutate(
      snum=as.numeric(fct_inorder(age_group)), # INLA needs groups as ints starting from 1, so add numeric state code
      ex_lam=population
    )
  
  # make a dataframe to hold group info for forecasting
  pred_df <- expand_grid(
    tibble(
      date=duration(1:weeks_ahead, "week") + max(ret$date),
      t=1:weeks_ahead + max(ret$t),
      epiweek=epiweek(date)
    ),
    distinct(ret, age_group, snum, population) # makes pairs of new times X each state
  ) |> 
    left_join(distinct(ret, age_group, epiweek, ex_lam)) # go and find `ex_lam` values for each state and epiweek
  
  bind_rows(ret, pred_df) |> # add to data for counts to be NAs
    arrange(t)
}

###########################
#Wrangle copycat
############################
library(tidyverse)

wrangle_copycat <- function(dataframe, curr_resp_season) {
  recent_sari <- dataframe |> 
    filter(age_group %in% c('Pediatric', 'Adult', 'Overall'),
           year == curr_resp_season,
           week >= 1) |> 
    group_by(age_group) |> 
    arrange(week)
  
  historic_sari <- dataframe |> 
    filter(age_group %in% c('Pediatric', 'Adult', 'Overall'),
           year != curr_resp_season,
           week >= 1) |> 
    group_by(age_group) |> 
    arrange(week)
  
  list(recent_sari = recent_sari, historic_sari = historic_sari)
}

#############################
#Wrangle SIRSeea
#############################
library(dplyr)
library(lubridate)
library(forcats)
library(splines)

wrangle_SIRsea <- function(dataframe, forecast_date, data_to_drop, forecast_horizons) {
  # Define the configuration based on data_to_drop
  config <- switch(data_to_drop,
                   "1 week" = list(days_before = 2, weeks_ahead = 5),
                   "2 week" = list(days_before = 7, weeks_ahead = 6),
                   "3 week" = list(days_before = 14, weeks_ahead = 7),
                   stop("Invalid data_to_drop option"))
  
  weeks_ahead <- config$weeks_ahead
  
  # Wrangle data
  wrangled_data <- dataframe |>
    filter(age_group != "Unknown") |>  # Remove rows with "Unknown" age group
    filter(date < as.Date(forecast_date) - days(config$days_before)) |>  # Remove recent data
    rename(epiweek = week)
  
  # Subset data for mechanistic model
  subset_data <- wrangled_data |>
    filter(date > "2021-09-01", age_group %in% c("Pediatric", "Adult")) |>  # Filter relevant data
    mutate(age_group = fct_relevel(age_group, "Pediatric")) |>  # Reorder factor levels
    filter(epiweek != 53) |>  # Exclude epiweek 53 for consistency
    arrange(age_group, date)
  
  if (!"inc_sari_hosp" %in% colnames(subset_data)) {
    stop("The column 'inc_sari_hosp' is not present in the dataframe.")
  }
  
  if (nrow(subset_data) %% 2 != 0) {
    stop("The number of rows in 'subset_data' must be even to construct 'ymat'.")
  }
  
  Tmax <- length(unique(subset_data$date)) + weeks_ahead
  spline_df <- round(Tmax/7) # the number of knots to place evenly (approx every 7 weeks)
  B <- t(bs(1:Tmax, spline_df, Boundary.knots=c(-2, Tmax+3)))
  
  ymat <- matrix(subset_data$inc_sari_hosp, nrow=2, byrow=TRUE) 
  
  # Prepare data for Stan
  stan_dat <- list(
    `T`=length(unique(subset_data$date)),
    H=weeks_ahead,
    G=2,
    N=c(1e6, 1e6), # let's just say a million people in each group at risk
    df=spline_df,
    B=B,
    y=ymat,
    # i_init=c(0.005, 0.005),
    alpha=c(0.9, 0.7), # pediatrics recover more quickly
    sd_phi_df=3,
    sd_phi_scale=5
  )
  
  return(list(subset_data = subset_data, stan_dat = stan_dat))
}


