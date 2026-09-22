

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

# Models offered by the Data tab's "Run Models" panel. Mirrors
# retrospective_model_choices (R/retrospective.R) so the two panels present the
# same menu, with the Ensemble appended: it is a step of this suite rather than
# a model the user configures, and it must run last since it combines whatever
# the other steps produced.
# Standard models first (the Ensemble among them -- it is a standard feature,
# and it executes last regardless, since model_steps in server/model_runs.R
# declares it last), then the development ones. The picker renders this as a
# plain ordered list, so this order is what the user sees.
run_all_model_choices <- c(
  "Regular Baseline" = "baseline_regular",
  "Seasonal Baseline" = "baseline_seasonal",
  "Opt Baseline" = "baseline_opt",
  "INFLAenza" = "inla",
  "Copycat" = "copycat",
  "newGBQR" = "newgbqr",
  "STArima" = "starima",
  "Ensemble" = "ensemble",
  "CalCopycat" = "calcopycat",
  "parGBQR" = "pargbqr",
  "FourCAT" = "fourcat"
)

# Ticked by default: exactly the set "Run All Models" ran before it became
# selectable, so an untouched panel reproduces the previous behaviour. The
# others are available but off, matching how the retrospective tab offers a
# wider menu than it preselects.
run_all_default_model_choices <- c(
  "baseline_regular", "baseline_seasonal", "baseline_opt",
  "inla", "copycat", "newgbqr", "starima", "ensemble"
)
