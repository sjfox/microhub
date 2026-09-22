suppressPackageStartupMessages({
  library(testthat)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(lubridate)
  library(ggplot2)
  library(cowplot)
  library(slider)
  # epiprocess is intentionally NOT loaded here -- it's a heavy transitive
  # dependency (tsibble, plotly, ...) unrelated to the retrospective
  # multi-group feature under test, and none of these tests exercise the
  # STArima code paths that actually call epiprocess:: functions.
  library(MMWRweek)
  # Needed only for the handful of pure server/retrospective.R helper
  # functions extracted below (retrospective_score_summary_table() calls
  # DT::datatable()/formatStyle()/styleEqual() unnamespaced).
  library(DT)
  # Needed for retrospective_parameter_input_widget() (R/retrospective.R),
  # which calls numericInput()/selectInput()/checkboxInput()/textInput()
  # unnamespaced -- building one of these input tags needs no live Shiny
  # session, only shiny's own namespace to resolve the function names.
  library(shiny)
})

# Run this from the repo root: Rscript run_tests.R
root <- getwd()
source(file.path(root, "R/utils.R"))
source(file.path(root, "R/data_utils.R"))
# modal_registry.R / ui_helpers.R are skipped here: they validate
# www/content/*.md paths at source() time that aren't part of this
# standalone check, and nothing in test-retrospective.R calls into them.
source(file.path(root, "R/ensemble.R"))
source(file.path(root, "R/retrospective.R"))
source(file.path(root, "R/plot.R"))
source(file.path(root, "R/baseline-regular.R"))
source(file.path(root, "R/STArima.R"))
source(file.path(root, "R/newGBQR_main_fxns.R"))
source(file.path(root, "R/newGBQR_helper_fxns.R"))
source(file.path(root, "R/parGBQR.R"))

# server/retrospective.R can't be source()'d as a whole outside a live Shiny
# session -- it's full of observeEvent()/reactive()/renderUI() calls that
# need an active reactive domain (session$ns, shinyjs, etc.), none of which
# exist here. But several of the plain helper functions it defines are pure
# R with no reactive dependencies at all, and are exactly the kind of logic
# (DOM id disambiguation, ensemble co-occurrence checks, the score summary
# table, the per-group health label) this review flagged as untested. Rather
# than duplicating their source into the test file (which silently drifts
# out of sync with the real implementation the app actually runs), parse the
# real file and evaluate ONLY the plain `name <- function(...) ...`
# top-level assignments into the global environment, skipping every
# observeEvent/observe/reactive/output$...<- assignment. This is a targeted
# extraction, not a full source(): anything that references reactive state
# (input$..., retrospective$..., other reactives) still can't be CALLED here
# unless the test stubs that dependency first (see test-retrospective.R).
extract_pure_server_functions <- function(path, function_names, envir = globalenv()) {
  exprs <- parse(path)
  found <- character()
  for (e in as.list(exprs)) {
    if (!is.call(e) || length(e) < 3) next
    # Guard against a non-assignment top-level call whose head is itself a
    # call rather than a bare symbol (e.g. a top-level `shinyjs::hidden(...)`
    # statement) -- as.character() on that head would return a multi-element
    # vector (c("::", "shinyjs", "hidden")) and blow up the %in% check below.
    head_sym <- e[[1]]
    if (!is.symbol(head_sym) || !as.character(head_sym) %in% c("<-", "=")) next

    target <- e[[2]]
    rhs <- e[[3]]
    rhs_head <- if (is.call(rhs)) rhs[[1]] else NULL
    if (is.name(target) && as.character(target) %in% function_names &&
        is.call(rhs) && is.symbol(rhs_head) && identical(as.character(rhs_head), "function")) {
      eval(e, envir = envir)
      found <- c(found, as.character(target))
    }
  }
  missing <- setdiff(function_names, found)
  if (length(missing) > 0) {
    stop(
      "extract_pure_server_functions() didn't find: ", paste(missing, collapse = ", "),
      " in ", path, " -- were they renamed or removed?"
    )
  }
  invisible(found)
}

extract_pure_server_functions(
  file.path(root, "server/retrospective.R"),
  c(
    "retrospective_group_dom_id_suffix",
    "retrospective_group_country_input_id",
    "retrospective_group_zone_badge_output_id",
    "retrospective_ensemble_members_co_occur_in_any_group",
    "retrospective_group_health_label",
    "format_retrospective_score_summary",
    "retrospective_score_summary_table",
    "retrospective_scoring_reference_exceptions"
  )
)

cat("=== All source files loaded OK ===\n")

test_results <- test_file(
  file.path(root, "tests/testthat/test-retrospective.R"),
  reporter = "summary"
)
