## Validation logic for user-uploaded outside model forecasts
## Returns a list with:
##   $errors   — named list of blocking error strings (file not added if any exist)
##   $warnings — named list of non-blocking warning strings (file still added)
##   $data     — cleaned data frame ready for use, or NULL if errors exist

validate_outside_model <- function(file, reference_df) {
  error_list   <- list()
  warning_list <- list()

  # ── Read file ────────────────────────────────────────────────────────────────
  df <- tryCatch(
    readr::read_csv(file, show_col_types = FALSE),
    error = function(e) NULL
  )

  if (is.null(df) || nrow(df) == 0) {
    error_list$read <- "Could not read the uploaded file, or the file is empty. Ensure it is a valid CSV."
    return(list(errors = error_list, warnings = warning_list, data = NULL))
  }

  # ── Check 1: Required columns ─────────────────────────────────────────────
  req_cols     <- c("model", "reference_date", "horizon",
                    "target_end_date", "target_group",
                    "output_type", "output_type_id", "value")
  missing_cols <- setdiff(req_cols, colnames(df))
  if (length(missing_cols) > 0) {
    error_list$cols <- paste0(
      "Missing required column(s): ", paste(missing_cols, collapse = ", "),
      ". Download the template to see the expected format."
    )
    # Cannot run further checks without required columns
    return(list(errors = error_list, warnings = warning_list, data = NULL))
  }

  # ── Check 2: Model name ────────────────────────────────────────────────────
  model_names <- unique(df$model)
  if (any(is.na(model_names)) || any(trimws(model_names) == "")) {
    error_list$model_name <- "The 'model' column contains blank or missing values. Every row must have a model name."
  }
  if (length(model_names) > 1) {
    warning_list$multi_model <- paste0(
      "Multiple model names found: ", paste(model_names, collapse = ", "),
      ". All rows will be included under the first name: \"", model_names[1], "\"."
    )
    df <- dplyr::mutate(df, model = model_names[1])
  }

  # ── Check 3: output_type must be "quantile" ────────────────────────────────
  non_quantile <- unique(df$output_type[df$output_type != "quantile"])
  if (length(non_quantile) > 0) {
    error_list$output_type <- paste0(
      "The 'output_type' column must be \"quantile\" for all rows. ",
      "Found unexpected value(s): ", paste(non_quantile, collapse = ", "), "."
    )
  }

  # ── Check 4: output_type_id must match expected quantile levels ────────────
  expected_quantiles <- sort(unique(as.numeric(reference_df$output_type_id)))
  found_quantiles    <- sort(unique(as.numeric(df$output_type_id)))
  missing_q          <- setdiff(expected_quantiles, found_quantiles)
  extra_q            <- setdiff(found_quantiles, expected_quantiles)

  if (length(missing_q) > 0) {
    shown <- paste(head(missing_q, 5), collapse = ", ")
    if (length(missing_q) > 5) shown <- paste0(shown, "...")
    error_list$missing_quantiles <- paste0(
      "Missing ", length(missing_q), " required quantile level(s): ", shown, ". ",
      "The uploaded file must contain every quantile level present in the other model forecasts."
    )
  }
  if (length(extra_q) > 0) {
    shown <- paste(head(extra_q, 5), collapse = ", ")
    if (length(extra_q) > 5) shown <- paste0(shown, "...")
    warning_list$extra_quantiles <- paste0(
      length(extra_q), " unrecognized quantile level(s) will be ignored: ", shown, "."
    )
    df <- dplyr::filter(df, as.numeric(output_type_id) %in% expected_quantiles)
  }

  # ── Check 5: target_group values must match ────────────────────────────────
  expected_groups <- sort(unique(as.character(reference_df$target_group)))
  found_groups    <- sort(unique(as.character(df$target_group)))
  missing_groups  <- setdiff(expected_groups, found_groups)
  extra_groups    <- setdiff(found_groups, expected_groups)

  if (length(missing_groups) > 0) {
    error_list$missing_groups <- paste0(
      "Missing target group(s): ", paste(missing_groups, collapse = ", "), ". ",
      "Expected: ", paste(expected_groups, collapse = ", "), "."
    )
  }
  if (length(extra_groups) > 0) {
    error_list$extra_groups <- paste0(
      "Unrecognized target group(s): ", paste(extra_groups, collapse = ", "), ". ",
      "Expected: ", paste(expected_groups, collapse = ", "), "."
    )
  }

  # ── Check 6: value column is numeric, non-negative, no NAs ─────────────────
  if (!is.numeric(df$value)) {
    error_list$value_type <- paste0(
      "The 'value' column must be numeric. ",
      "Found non-numeric entries — ensure all forecast values are numbers."
    )
  } else {
    n_na <- sum(is.na(df$value))
    if (n_na > 0) {
      error_list$value_na <- paste0(
        "The 'value' column contains ", n_na, " missing (NA) value(s). ",
        "Every row must have a forecast value."
      )
    }
    if (any(df$value < 0, na.rm = TRUE)) {
      n_neg <- sum(df$value < 0, na.rm = TRUE)
      error_list$value_neg <- paste0(
        "The 'value' column contains ", n_neg, " negative value(s). ",
        "Forecast counts must be zero or positive."
      )
    }
  }

  # ── Check 7: No duplicate (target_end_date, target_group, output_type_id) ──
  dup_counts <- df |>
    dplyr::count(target_end_date, target_group, output_type_id) |>
    dplyr::filter(n > 1)
  if (nrow(dup_counts) > 0) {
    ex <- dup_counts[1, ]
    error_list$duplicates <- paste0(
      nrow(dup_counts), " duplicate (target_end_date / target_group / quantile) combination(s) found ",
      "(e.g., ", ex$target_end_date, " / ", ex$target_group,
      " / quantile ", ex$output_type_id, "). ",
      "Each combination must appear exactly once."
    )
  }

  # ── Check 8: Quantile monotonicity within each forecast cell ─────────────
  non_mono <- df |>
    dplyr::filter(output_type == "quantile") |>
    dplyr::arrange(target_end_date, target_group, as.numeric(output_type_id)) |>
    dplyr::group_by(target_end_date, target_group) |>
    dplyr::summarize(monotone = all(diff(value) >= -1e-9), .groups = "drop") |>
    dplyr::filter(!monotone)
  if (nrow(non_mono) > 0) {
    ex <- non_mono[1, ]
    error_list$monotonicity <- paste0(
      "Quantile forecasts are not monotonically non-decreasing in ",
      nrow(non_mono), " (target_end_date / target_group) combination(s) ",
      "(e.g., ", ex$target_end_date, " / ", ex$target_group, "). ",
      "Higher quantile levels must have equal or greater values than lower levels."
    )
  }

  # ── Check 9: forecast dates align with reference forecasts ─────────────────
  ref_dates    <- sort(unique(as.character(reference_df$target_end_date)))
  upload_dates <- sort(unique(as.character(df$target_end_date)))
  missing_dates <- setdiff(ref_dates, upload_dates)
  extra_dates   <- setdiff(upload_dates, ref_dates)

  if (length(missing_dates) > 0) {
    shown <- paste(head(missing_dates, 3), collapse = ", ")
    if (length(missing_dates) > 3) shown <- paste0(shown, "...")
    error_list$missing_dates <- paste0(
      "Missing ", length(missing_dates), " forecast date(s) that other models have: ", shown, ". ",
      "Download the template to see the required forecast dates."
    )
  }
  if (length(extra_dates) > 0) {
    shown <- paste(head(extra_dates, 3), collapse = ", ")
    if (length(extra_dates) > 3) shown <- paste0(shown, "...")
    warning_list$extra_dates <- paste0(
      length(extra_dates), " forecast date(s) not present in other models will be ignored: ", shown, "."
    )
    df <- dplyr::filter(df, as.character(target_end_date) %in% ref_dates)
  }

  # ── If no errors, clean and return data ───────────────────────────────────
  if (length(error_list) > 0) {
    return(list(errors = error_list, warnings = warning_list, data = NULL))
  }

  df_clean <- df |>
    dplyr::mutate(
      reference_date  = as.Date(lubridate::parse_date_time(reference_date,  orders = c("ymd", "mdy"))),
      target_end_date = as.Date(lubridate::parse_date_time(target_end_date, orders = c("ymd", "mdy"))),
      horizon         = as.numeric(horizon),
      output_type_id  = as.numeric(output_type_id),
      value           = round(as.numeric(value), 0)
    ) |>
    dplyr::select(model, reference_date, horizon, target_end_date,
                  target_group, output_type, output_type_id, value)

  list(errors = error_list, warnings = warning_list, data = df_clean)
}
