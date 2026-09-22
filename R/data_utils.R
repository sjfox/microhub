## Script contains app-specific functions for data


# Reading the data for forecasting ----------------------------------------

parse_microhub_dates <- function(date) {
  date_chr <- as.character(date)
  numeric_parts <- regmatches(
    date_chr,
    regexec("^\\s*(\\d{1,2})[/-](\\d{1,2})[/-]\\d{2,4}\\s*$", date_chr)
  )
  first_part <- suppressWarnings(as.integer(vapply(
    numeric_parts,
    function(parts) if (length(parts) >= 3) parts[[2]] else NA_character_,
    character(1)
  )))

  ambiguous_order <- if (any(first_part > 12, na.rm = TRUE)) {
    "dmy"
  } else {
    "mdy"
  }

  parse_date_time(date_chr, orders = c("ymd", ambiguous_order)) |> as.Date()
}

read_raw_data <- function(file_path){
  ## Reads in the raw data and makes sure the date is nicely formatted
  data <- read_csv(
    file_path,
    col_types = cols(
      date = col_character(),
      target_group = col_character(),
      value = col_double(),
      .default = col_guess()
    )
  ) |>
    mutate(date = parse_microhub_dates(date))

  if ("population" %in% names(data)) {
    data <- data |> mutate(population = as.numeric(population))
  }

  # Trim the optional retrospective_group column so "Argentina" and
  # "Argentina " (a stray trailing space from a spreadsheet export) are
  # treated as the same group rather than silently splitting into two.
  if ("retrospective_group" %in% names(data)) {
    data <- data |> mutate(retrospective_group = trimws(as.character(retrospective_group)))
  }

  data
}

normalize_country_match_text <- function(value) {
  value |>
    tools::file_path_sans_ext() |>
    tolower() |>
    gsub("[^a-z0-9]+", " ", x = _) |>
    trimws()
}

country_from_upload_filename <- function(filename, epizone_data, default = "Paraguay") {
  if (is.null(filename) || is.na(filename) || !("COUNTRY" %in% names(epizone_data))) {
    return(default)
  }

  normalized_filename <- paste0(" ", normalize_country_match_text(basename(filename)), " ")
  countries <- unique(epizone_data$COUNTRY[!is.na(epizone_data$COUNTRY)])

  match_tbl <- tibble(
    country = countries,
    normalized_country = vapply(countries, normalize_country_match_text, character(1))
  ) |>
    mutate(
      is_match = vapply(
        normalized_country,
        function(country_text) {
          grepl(
            paste0(" ", country_text, " "),
            normalized_filename,
            fixed = TRUE
          )
        },
        logical(1)
      )
    ) |>
    filter(
      nchar(normalized_country) > 0,
      is_match
    ) |>
    mutate(match_length = nchar(normalized_country)) |>
    arrange(desc(match_length), country)

  if (nrow(match_tbl) == 0) {
    return(default)
  }

  match_tbl$country[[1]]
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
    "3 week" = 3,
    "4 week" = 4,
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
                              data_df,
                              forecast_date){
  ## Calculates the total weeks of forecasts needed accounting for any
  ## gap between the last available training date and the first forecast week

  reference_date <- get_reference_date(
    data_df = data_df,
    forecast_date = forecast_date
  )

  most_recent_date <- data_df |>
    pull(date) |>
    as.Date() |>
    max(na.rm = TRUE)

  bridge_weeks <- max(
    0,
    as.integer(as.numeric(difftime(reference_date, most_recent_date, units = "days")) / 7) - 1
  )

  fcast_horizon + bridge_weeks
}



# Validation functions for data -------------------------------------------
# Function to validate data
## Infer whether a value column holds counts or 0-1 proportions, for use as
## the DEFAULT of the Data Type control on upload. The user can always
## override; this only picks the starting position.
##
## The obvious rule -- "everything in [0, 1] means proportion" -- is not quite
## safe on its own, because count data legitimately lands there: a rare outcome
## in a small jurisdiction gives a column of 0s and 1s, as do early-season weeks
## for almost any respiratory target. Integrality breaks the tie, since a
## genuine proportion series will essentially always contain a non-integer.
##
## Ambiguity therefore resolves to `default` ("count"), which is both the app's
## historical default and the lenient one for validation: every valid
## proportion row is also a valid count row, so a wrong guess here surfaces as
## a control the user flips rather than as a rejected upload.
detect_data_type <- function(values, default = "count") {
  values <- suppressWarnings(as.numeric(values))
  values <- values[!is.na(values) & is.finite(values)]

  if (length(values) == 0) {
    return(default)
  }

  # Negative values are invalid for both types; "count" gives the clearer
  # error message from validate_data(), so don't steer toward "proportion".
  if (any(values < 0)) {
    return("count")
  }

  # Anything above 1 cannot be a proportion on microhub's 0-1 scale.
  if (any(values > 1)) {
    return("count")
  }

  # Everything is within [0, 1]. All-integer means 0/1 counts, not proportions.
  if (all(values == floor(values))) {
    return(default)
  }

  "proportion"
}

## Detect a type per group, for the retrospective tab's multi-group upload
## where each retrospective_group carries its own Data Type. Returns a named
## list keyed by group value.
detect_data_type_by_group <- function(values, groups, default = "count") {
  groups <- as.character(groups)
  split_values <- split(values, groups)

  stats::setNames(
    lapply(split_values, detect_data_type, default = default),
    names(split_values)
  )
}

validate_data <- function(file, data_type = "count") {
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
  parsed_dates <- suppressWarnings(parse_microhub_dates(df$date))
  bad_dates <- df$date[is.na(parsed_dates)]
  if (length(bad_dates) > 0) {
    example <- head(bad_dates, 3) |> paste(collapse = ", ")
    error_list$check2 <- paste0(
      "The 'date' column contains values that could not be parsed as dates (e.g., ", example, "). ",
      "Ensure dates are in YYYY-MM-DD, MM/DD/YYYY, MM-DD-YYYY, DD/MM/YYYY, or DD-MM-YYYY format."
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
    # Check 3b: value must be within the valid range for the selected data type
    if (identical(data_type, "proportion")) {
      if (any(df$value < 0 | df$value > 1, na.rm = TRUE)) {
        error_list$check3b <-
          "The 'value' column contains values outside the 0-1 range. Proportions must be expressed as a fraction between 0 and 1 (e.g. 0.42, not 42 or 42%)."
      }
    } else {
      if (any(df$value < 0, na.rm = TRUE)) {
        error_list$check3b <-
          "The 'value' column contains negative values. Counts must be zero or positive."
      }
    }

    # Check 3c: No missing values
    n_na <- sum(is.na(df$value))
    if (n_na > 0) {
      error_list$check3c <- paste0(
        "The 'value' column contains ", n_na, " missing (NA) value(s). All rows must have a count."
      )
    }
  }

  # An optional `retrospective_group` column lets the retrospective tab run
  # every model independently for each value (e.g. one per country), each
  # using only that value's own rows. When present, the duplicate-row and
  # gap checks below are scoped per group as well as per target_group, and
  # an additional check requires every group to share identical week
  # coverage (the retrospective UI applies a single reference-week range
  # to all groups at once).
  has_group_col <- "retrospective_group" %in% curr_cols
  dup_key_cols <- if (has_group_col) c("retrospective_group", "date", "target_group") else c("date", "target_group")
  gap_group_cols <- if (has_group_col) c("retrospective_group", "target_group") else "target_group"

  if (has_group_col) {
    # Trim before every check below, and before read_raw_data() loads this
    # same file for the actual run -- so "Argentina" and "Argentina " (a
    # stray trailing space) are validated, and later forecast, as the same
    # group rather than silently splitting into two thin, separate ones.
    df$retrospective_group <- trimws(as.character(df$retrospective_group))

    blank_groups <- sum(is.na(df$retrospective_group) | !nzchar(df$retrospective_group))
    if (blank_groups > 0) {
      error_list$check_group <- paste0(
        "The 'retrospective_group' column contains ", blank_groups,
        " blank or missing value(s). Every row must specify a group when this column is used."
      )
    }
  }

  # Check 4: No duplicate (date, target_group[, retrospective_group]) combinations
  dup_counts <- df |>
    dplyr::count(dplyr::across(dplyr::all_of(dup_key_cols))) |>
    dplyr::filter(n > 1)
  if (nrow(dup_counts) > 0) {
    example_row <- dup_counts[1, ]
    combo_label <- paste(
      vapply(dup_key_cols, function(col) paste0(col, "=", example_row[[col]]), character(1)),
      collapse = ", "
    )
    error_list$check4 <- paste0(
      "Duplicate rows found for ", nrow(dup_counts), " combination(s) of ",
      paste(dup_key_cols, collapse = "/"), " ",
      "(e.g., ", combo_label, "). ",
      "Each combination must appear exactly once."
    )
  }

  # Check 5: No gaps > 1 week in the date sequence (per target_group, and per
  # retrospective_group when that column is present)
  if (!is.null(parsed_dates) && sum(!is.na(parsed_dates)) > 1) {
    df_dates <- df |>
      dplyr::mutate(parsed_date = parsed_dates) |>
      dplyr::filter(!is.na(parsed_date)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(gap_group_cols))) |>
      dplyr::arrange(parsed_date) |>
      dplyr::mutate(gap_days = as.numeric(difftime(parsed_date, dplyr::lag(parsed_date), units = "days"))) |>
      dplyr::filter(!is.na(gap_days) & gap_days > 8) |>  # allow up to 8 days to handle rounding
      dplyr::ungroup()

    if (nrow(df_dates) > 0) {
      example_row <- df_dates[1, ]
      prev_date <- example_row$parsed_date - lubridate::days(round(example_row$gap_days))
      group_label <- if (has_group_col) {
        paste0("group '", example_row$retrospective_group, "', target group '", example_row$target_group, "'")
      } else {
        paste0("group '", example_row$target_group, "'")
      }
      error_list$check5 <- paste0(
        "Missing weeks detected in the time series ",
        "(e.g., gap between ", format(prev_date, "%Y-%m-%d"), " and ",
        format(example_row$parsed_date, "%Y-%m-%d"), " in ", group_label, "). ",
        "The data should have one row per week per target group."
      )
    }
  }

  # Check 6: When retrospective_group is used, every group must cover the
  # exact same set of observed weeks, since the retrospective UI applies one
  # shared reference-week range across all groups in a single run.
  if (has_group_col && !is.null(parsed_dates) && sum(!is.na(parsed_dates)) > 0) {
    group_dates <- df |>
      dplyr::mutate(parsed_date = parsed_dates) |>
      dplyr::filter(!is.na(parsed_date), !is.na(retrospective_group), nzchar(trimws(as.character(retrospective_group)))) |>
      dplyr::distinct(retrospective_group, parsed_date)

    if (nrow(group_dates) > 0) {
      date_sets <- split(group_dates$parsed_date, group_dates$retrospective_group)
      reference_group <- names(date_sets)[[1]]
      reference_dates_set <- sort(unique(date_sets[[reference_group]]))
      mismatched <- names(date_sets)[
        vapply(date_sets, function(x) !identical(sort(unique(x)), reference_dates_set), logical(1))
      ]

      if (length(mismatched) > 0) {
        error_list$check6 <- paste0(
          "Every value of 'retrospective_group' must cover the same set of weeks. ",
          "Group(s) with different week coverage than '", reference_group, "': ",
          paste(mismatched, collapse = ", "), "."
        )
      }
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

## Data-type-aware clipping + rounding applied to every model's final output.
## Centralizing this here (rather than inside each fit_process_*()) means a
## model that hasn't been updated to respect the proportion scale internally
## still can't emit an impossible value (negative, or above 1 for a
## proportion), and rounding precision matches the scale of the number.
finalize_forecast_value <- function(value, data_type = "count") {
  value <- pmax(as.numeric(value), 0)

  if (identical(data_type, "proportion")) {
    value <- pmin(value, 1)
    round(value, 4)
  } else {
    round(value, 0)
  }
}

## Format the forecasts for final use
get_reference_date <- function(data_df, forecast_date) {
  data_dates <- data_df |>
    pull(date) |>
    as.Date()

  if (length(data_dates) == 0 || all(is.na(data_dates))) {
    stop("data_df must contain at least one valid date")
  }

  weekday_counts <- table(lubridate::wday(data_dates, week_start = 1))
  series_weekday <- as.integer(names(weekday_counts)[which.max(weekday_counts)])

  forecast_date <- as.Date(forecast_date)
  forecast_weekday <- lubridate::wday(forecast_date, week_start = 1)
  days_ahead <- (series_weekday - forecast_weekday) %% 7

  if (days_ahead == 0) {
    days_ahead <- 7
  }

  forecast_date + days(days_ahead)
}

format_forecasts <- function(forecast_df,
                             model_name,
                             data_df,
                             data_to_drop,
                             forecast_date,
                             forecast_output = "all",
                             data_type = "count"){

  reference_date <- get_reference_date(
    data_df = data_df,
    forecast_date = forecast_date
  )

  most_recent_date <- data_df |>
    pull(date) |>
    as.Date() |>
    max(na.rm = TRUE)

  bridge_weeks <- max(
    0,
    as.integer(as.numeric(difftime(reference_date, most_recent_date, units = "days")) / 7) - 1
  )

  formatted_forecasts <- forecast_df |>
    mutate(reference_date = reference_date,
           horizon = horizon-bridge_weeks-1,
           target_end_date = reference_date + weeks(horizon),
           model = model_name
    ) |>
    dplyr::select(
      model,
      reference_date,
      horizon,
      target_end_date,
      target_group,
      output_type,
      output_type_id,
      value
    ) |>
    dplyr::mutate(value = finalize_forecast_value(value, data_type))

  if (identical(forecast_output, "horizon_gte_0")) {
    formatted_forecasts <- formatted_forecasts |>
      filter(horizon >= 0)
  } else if (!identical(forecast_output, "all")) {
    stop("Invalid forecast_output option")
  }

  formatted_forecasts
}
