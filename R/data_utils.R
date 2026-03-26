## Script contains app-specific functions for data


# Reading the data for forecasting ----------------------------------------

read_raw_data <- function(file_path){
  ## Reads in the raw data and makes sure the date is nicely formatted
  read_csv(file_path,
           col_types = cols(date = col_character(),
                            target_group = col_character(),
                            value = col_double())
  ) |>
    mutate(date = parse_date_time(date, orders = c("mdy", "ymd")) |> as.Date())
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
  df <- read_csv(file, show_col_types = FALSE)

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
    # Cannot continue further checks without required columns
    return(error_list)
  }

  # Check 2: Are dates parseable?
  parsed_dates <- suppressWarnings(parse_date_time(df$date, orders = c("mdy", "ymd", "dmy")))
  bad_dates <- df$date[is.na(parsed_dates)]
  if (length(bad_dates) > 0) {
    example <- head(bad_dates, 3) |> paste(collapse = ", ")
    error_list$check2 <- paste0(
      "The 'date' column contains values that could not be parsed as dates (e.g., ", example, "). ",
      "Ensure dates are in MM/DD/YYYY, MM-DD-YYYY, or YYYY-MM-DD format."
    )
  }

  # Check 3: Is value numeric and non-negative?
  if (!is.numeric(df$value)) {
    non_numeric <- unique(df$value[suppressWarnings(is.na(as.numeric(df$value)))])
    example <- head(non_numeric, 3) |> paste(collapse = ", ")
    error_list$check3 <- paste0(
      "The 'value' column must contain numbers only. Non-numeric values found: ", example, "."
    )
  } else {
    # Check 3b: No negative values
    if (any(df$value < 0, na.rm = TRUE)) {
      error_list$check3b <-
        "The 'value' column contains negative values. Counts must be zero or positive."
    }

    # Check 3c: No missing values
    n_na <- sum(is.na(df$value))
    if (n_na > 0) {
      error_list$check3c <- paste0(
        "The 'value' column contains ", n_na, " missing (NA) value(s). All rows must have a count."
      )
    }
  }

  # Check 4: No duplicate (date, target_group) combinations
  dup_counts <- df |>
    dplyr::count(date, target_group) |>
    dplyr::filter(n > 1)
  if (nrow(dup_counts) > 0) {
    example_row <- dup_counts[1, ]
    error_list$check4 <- paste0(
      "Duplicate rows found for ", nrow(dup_counts), " date/target_group combination(s) ",
      "(e.g., ", example_row$date, " + ", example_row$target_group, "). ",
      "Each combination must appear exactly once."
    )
  }

  # Check 5: No gaps > 1 week in the date sequence (per target_group)
  if (!is.null(parsed_dates) && sum(!is.na(parsed_dates)) > 1) {
    df_dates <- df |>
      dplyr::mutate(parsed_date = parsed_dates) |>
      dplyr::filter(!is.na(parsed_date)) |>
      dplyr::group_by(target_group) |>
      dplyr::arrange(parsed_date) |>
      dplyr::mutate(gap_days = as.numeric(difftime(parsed_date, dplyr::lag(parsed_date), units = "days"))) |>
      dplyr::filter(!is.na(gap_days) & gap_days > 8) |>  # allow up to 8 days to handle rounding
      dplyr::ungroup()

    if (nrow(df_dates) > 0) {
      example_row <- df_dates[1, ]
      prev_date <- example_row$parsed_date - lubridate::days(round(example_row$gap_days))
      error_list$check5 <- paste0(
        "Missing weeks detected in the time series ",
        "(e.g., gap between ", format(prev_date, "%Y-%m-%d"), " and ",
        format(example_row$parsed_date, "%Y-%m-%d"), " in group '", example_row$target_group, "'). ",
        "The data should have one row per week per target group."
      )
    }
  }

  return(error_list)
}

# Function to validate population data

# validate_population <- function(file) {
#   error_list <- list()
#   df <- read.csv(file)
#
#   # Check 1: Does the csv have the required columns?
#   curr_cols <- colnames(df)
#   req_cols <- c("target_group", "population")
#   check1 <- all(req_cols %in% curr_cols)
#
#   if (!check1) {
#     missing_cols <- setdiff(req_cols, curr_cols)
#     error_list$check1 <-
#       paste(
#         "Missing columns:",
#         paste(missing_cols, collapse = ", ")
#       )
#   }
#
#   return(error_list)
# }

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
