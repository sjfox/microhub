# Data wrangling and manipulation ==============================================
install.packages(c("dplyr", "lubridate", "tidyr", "purrr", "forcats", "tibble"))

# Plotting =====================================================================
install.packages(c("ggplot2", "cowplot"))

# Shiny ========================================================================
install.packages(c("shiny", "shinyjs", "bslib", "DT"))

# Forecasting ==================================================================
install.packages(c("epiprocess", "mgcv", "gam", "splines", "MMWRweek"))

## The below two packages are not on CRAN

# For help with INLA installation, check R version compatibility at
# https://www.r-inla.org/download-install
install.packages(
  "INLA",
  repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
  dep = TRUE
)

remotes::install_github("reichlab/simplets")