# Data wrangling and manipulation ==============================================
install.packages(c("dplyr", "readr", "lubridate", "tidyr", "purrr", "forcats", "tibble"))

# Plotting =====================================================================
install.packages(c("ggplot2", "scales", "cowplot", "gridExtra", "ggtext"))

# Shiny ========================================================================
install.packages(c("shiny", "shinyjs", "bslib", "DT"))

# Forecasting ==================================================================
install.packages(c("mgcv", "gam", "MMWRweek", "lightgbm", "slider"))

# Package installation helpers =================================================
install_github_package <- function(package_ref) {
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }

  if (requireNamespace("pak", quietly = TRUE)) {
    pak::pkg_install(package_ref, upgrade = FALSE, ask = FALSE)
    return(invisible(TRUE))
  }

  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }

  package_parts <- strsplit(package_ref, "@", fixed = TRUE)[[1]]
  repo <- package_parts[[1]]
  ref <- if (length(package_parts) > 1) package_parts[[2]] else "HEAD"

  remotes::install_github(repo, ref = ref, upgrade = "never")
  invisible(TRUE)
}

## The below packages are not on CRAN

# For help with INLA installation, check R version compatibility at
# https://www.r-inla.org/download-install
install.packages(
  "INLA",
  repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
  dep = TRUE
)

install_github_package("cmu-delphi/epiprocess@main")
install_github_package("reichlab/simplets")
