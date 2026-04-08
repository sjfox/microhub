# Force bslib to recompile Sass on every start (prevents stale color cache)
options(sass.cache = FALSE)

# Attach libraries =============================================================

# Data wrangling and manipulation
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(purrr)
library(forcats)
library(tibble)

# Plotting
library(ggplot2)
library(cowplot)

# Forecasting
library(epiprocess)
library(simplets)
library(mgcv)
library(gam)
library(splines)
library(INLA)
library(MMWRweek)
library(lightgbm)
library(slider)

# Shiny
library(shiny)
library(shinyjs)
library(bslib)
library(DT)

# Source R scripts =============================================================

source("R/baseline-regular.R")
source("R/baseline-seasonal.R")
source("R/inla.R")
source("R/copycat.R")
source("R/copycat_cal.R")
source("R/GBQR_main_fxns.R")
source("R/GBQR_helper_fxns.R")
source("R/plot.R")
source("R/utils.R")
source("R/data_utils.R")
source("R/validate_outside_model.R")
source("R/ui_helpers.R")

# Load epizone lookup data =====================================================

epizone_data <- read.csv(
  "data/epizone_assignment_March2026.csv",
  stringsAsFactors = FALSE
) |>
  dplyr::filter(!is.na(epi_zone) & epi_zone != "NA") |>
  dplyr::arrange(COUNTRY)

epizone_choices <- setNames(epizone_data$COUNTRY, epizone_data$COUNTRY)

# Set global options ===========================================================

options(shiny.timeout = 3600)

# Define UI ====================================================================

ui <- page_navbar(
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    useShinyjs(),
  ),
  title = "MicroHub Forecasting",
  theme = bs_theme(
    version    = 5,
    bootswatch = "yeti",
    primary    = "#002454",
    secondary  = "#C3B8B2",
    "link-color"       = "#002454",
    "link-hover-color" = "#001538",
    header_font = font_google("Oswald"),
    base_font   = font_google("Merriweather Sans")
  ),
  navbar_options = navbar_options(bg = "#002454", theme = "dark"),

  source("ui/tab_home.R",     local = TRUE)$value,
  source("ui/tab_data.R",     local = TRUE)$value,
  source("ui/tab_baseline.R", local = TRUE)$value,
  source("ui/tab_inlaenza.R", local = TRUE)$value,
  source("ui/tab_copycat.R",     local = TRUE)$value,
  source("ui/tab_copycat_cal.R", local = TRUE)$value,
  source("ui/tab_gbqr.R",        local = TRUE)$value,
  source("ui/tab_ensemble.R", local = TRUE)$value,
  source("ui/tab_download.R", local = TRUE)$value
)

# Define server ================================================================

server <- function(input, output, session) {
  source("server/init.R",        local = TRUE)
  source("server/modals.R",      local = TRUE)
  source("server/data_upload.R", local = TRUE)
  source("server/baselines.R",   local = TRUE)
  source("server/inlaenza.R",    local = TRUE)
  source("server/copycat.R",     local = TRUE)
  source("server/copycat_cal.R", local = TRUE)
  source("server/gbqr.R",        local = TRUE)
  source("server/download.R",    local = TRUE)
  source("server/ensemble.R",    local = TRUE)
}

shinyApp(ui, server)
