# Function to validate data

validate_data <- function(file) {
  error_list <- list()
  df <- read.csv(file)

  # Check 1: Does the csv have the required columns?
  curr_cols <- colnames(df)
  req_cols <- c("year", "week", "date", "target_group", "value")
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

# Function for modal popups

show_modal <- function(title, id, md) {
  showModal(
    modalDialog(
      title = title,
      tags$div(
        id = id,
        withMathJax(includeMarkdown(
          normalizePath(paste0("www/content/", md, ".md"))
        )),
      ),
      easyClose = TRUE,
      size = "l"
    )
  )
}

# Function to get the closest Wednesday to a given date

closest_wednesday <- function(date) {
  weekday_num <- as.integer(format(date, "%u")) # 1 = Monday, ..., 7 = Sunday
  offset <- 3 - weekday_num
  if (abs(offset) > 3) {
    offset <- ifelse(offset > 0, offset - 7, offset + 7)
  }
  return(date + offset)
}
