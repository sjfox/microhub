

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
