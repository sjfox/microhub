## Script contains app-specific functions for data


# Reading the data for forecasting ----------------------------------------

read_raw_data <- function(file_path){
  ## Reads in the raw data and makes sure the date is nicely formatted
  read_csv(file_path) |>
    mutate(date = mdy(date))
}

check_overall_completeness <- function(df){
  ## Checking to see if target groups sum to the overall category

  # Get vector of target groups
  target_groups <- df |>
    distinct(target_group) |>
    pull()

  # Check if "overall" category equals the sum of individual components
  overall_df <- df |>
    filter(!target_group == "Overall") |>
    summarize(target_sum = sum(value), .by = c("date")) |>
    left_join(
      df |> filter(target_group == "Overall"),
      by = join_by(date)
    ) |>
    mutate(overall_equal_sum = ifelse(target_sum == value, TRUE, FALSE))

  # Set `overall` reactive var to single_target or aggregate
  # If any of the dates the total of the target groups doesn't equal the overall
  # Only safe thing to do is assume the overall isn't aggregated
  ifelse(any(overall_df$overall_equal_sum == FALSE),
         "single_target",
         "aggregate")
}

get_weeks_to_drop <- function(data_to_drop){
  ## Takes the character input and outputs a numeric value
  switch(
    data_to_drop,
    "0 weeks" = 0,
    "1 week" = 1,
    "2 week" = 2,
    stop("Invalid data_to_drop option")
  )
}

get_fcast_data <- function(df,
                           forecast_date,
                           data_to_drop){
  ## This takes the raw data and makes sure that:
  ## the most recent data are set before the forecast data and
  ## removes the correct number of recent weeks

  ## First make sure you get the data frame to before the forecast date
  ## We allow keeping data on the forecast date, because theoretically
  ## You could produce the data and make your forecast for the future on the same day
  df <-   df |>
    filter(date <= forecast_date)



  ## Now find the most recent n dates that will be removed
  dates_to_remove <- df |>
    dplyr::distinct(date) |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::slice_head(n = get_weeks_to_drop(data_to_drop)) |>
    dplyr::pull(date)

    df |>
      dplyr::filter(!(date %in% dates_to_remove)) |>
      arrange(date)
}

get_plot_data <- function(df,
                           forecast_date,
                           data_to_drop){
  ## This takes the raw data and gets the data for plotting
  ## The key difference with the fcast data is that includes the
  ## data that are supposed to be dropped rather than removing them

  ## First make sure you get the data frame to before the forecast date
  ## We allow keeping data on the forecast date, because theoretically
  ## You could produce the data and make your forecast for the future on the same day
  df <-   df |>
    filter(date <= forecast_date)


  ## Now find the most recent n dates that will be removed
  dates_to_remove <- df |>
    dplyr::distinct(date) |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::slice_head(n = get_weeks_to_drop(data_to_drop)) |>
    dplyr::pull(date)

  df |>
    mutate(dropped_week = (date %in% dates_to_remove)) |>
    arrange(date)
}



get_fcast_horizon <- function(fcast_horizon,
                              data_to_drop){
  ## Calculates the number of total weeks of forecasts needed accounting
  ## For the desired horizon and the number of weeks removed


  fcast_horizon + get_weeks_to_drop(data_to_drop)
}



# Validation functions for data -------------------------------------------
# Function to validate data
validate_data <- function(file) {
  error_list <- list()
  df <- read_csv(file)

  # Check 1: Does the csv have the required columns?
  curr_cols <- colnames(df)
  req_cols <- c("date", "target_group", "value")
  check1 <- all(req_cols %in% curr_cols)

  if (!check1) {
    missing_cols <- setdiff(req_cols, curr_cols)
    error_list$check1 <-
      paste(
        "Missing columns:",
        paste(missing_cols, collapse = ", ")
      )
  }

  return(error_list)
}

# Function to validate population data

validate_population <- function(file) {
  error_list <- list()
  df <- read.csv(file)

  # Check 1: Does the csv have the required columns?
  curr_cols <- colnames(df)
  req_cols <- c("target_group", "population")
  check1 <- all(req_cols %in% curr_cols)

  if (!check1) {
    missing_cols <- setdiff(req_cols, curr_cols)
    error_list$check1 <-
      paste(
        "Missing columns:",
        paste(missing_cols, collapse = ", ")
      )
  }

  return(error_list)
}

## Format the forecasts for final use
format_forecasts <- function(forecast_df,
                             model_name,
                             data_df,
                             data_to_drop){

  most_recent_date <- data_df |>
    pull(date) |>
    max()

  weeks_to_drop <- get_weeks_to_drop(data_to_drop)

  forecast_df |>
    mutate(reference_date = most_recent_date + weeks(weeks_to_drop+1), ## Makes the reference date the date of first forecast
           target_end_date = most_recent_date + weeks(horizon),
           horizon = horizon-weeks_to_drop-1,
           model = model_name
    ) |>
    filter(horizon>=0) |>
    dplyr::select(
      model,
      reference_date,
      horizon,
      target_end_date,
      target_group,
      output_type,
      output_type_id,
      value
    )
}
