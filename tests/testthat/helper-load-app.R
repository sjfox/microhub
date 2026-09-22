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
  library(epiprocess)
  library(MMWRweek)
  library(DT)
})

source(test_path("../../R/utils.R"))
source(test_path("../../R/data_utils.R"))
source(test_path("../../R/modal_registry.R"))
source(test_path("../../R/ui_helpers.R"))
source(test_path("../../R/ensemble.R"))
source(test_path("../../R/retrospective.R"))
source(test_path("../../R/plot.R"))
source(test_path("../../R/baseline-regular.R"))
source(test_path("../../R/STArima.R"))
source(test_path("../../R/newGBQR_main_fxns.R"))
source(test_path("../../R/newGBQR_helper_fxns.R"))
source(test_path("../../R/parGBQR.R"))

extract_pure_server_functions <- function(path, function_names, envir = globalenv()) {
  exprs <- parse(path)
  found <- character()
  for (e in as.list(exprs)) {
    if (!is.call(e) || length(e) < 3) next
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
      "extract_pure_server_functions() didn't find: ",
      paste(missing, collapse = ", "),
      " in ",
      path
    )
  }
  invisible(found)
}

extract_pure_server_functions(
  test_path("../../server/retrospective.R"),
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

extract_pure_server_functions(
  test_path("../../server/download.R"),
  c(
    "normalize_download_format",
    "download_file_extension",
    "sanitize_download_model_name",
    "download_model_file_names",
    "write_forecast_export",
    "write_forecast_exports_by_model"
  )
)
