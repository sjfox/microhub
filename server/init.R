# Initialize app ==============================================================

# Reactive values to store data and forecasts
rv <- reactiveValues(
  raw_data = NULL,
  valid_data = NULL,
  pending_upload = NULL,
  active_upload_name = NULL,
  upload_status_message = NULL,
  quantiles_needed = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
  baseline_regular = NULL,
  baseline_seasonal = NULL,
  baseline_opt = NULL,
  inla = NULL,
  valid_pop = NULL,
  copycat     = NULL,
  copycat_cal = NULL,
  gbqr        = NULL,
  ensemble = NULL,
  outside_models = list(),
  outside_model_validations = list(),
  last_outside_model_validation = NULL,
)

# Disable action buttons initially
disable("run_baseline_regular")
disable("run_baseline_opt")
disable("run_baseline_seasonal")
disable("run_inla")
disable("inla_plot_download")
disable("run_copycat")
disable("copycat_plot_download")
disable("run_copycat_cal")
disable("copycat_cal_plot_download")
disable("run_gbqr")
disable("gbqr_plot_download")
disable("run_ensemble")
disable("ensemble_plot_download")
disable("download_results")

# Disable use_population_data button initially
disable("use_population_column")

# Core reactives --------------------------------------------------------------

has_forecast_rows <- function(x) {
  !is.null(x) && NROW(x) > 0
}

get_reference_date_label <- function(x, fallback = Sys.Date()) {
  if (!is.null(x) && NROW(x) > 0 && "reference_date" %in% names(x)) {
    ref_date <- suppressWarnings(as.Date(x$reference_date[[1]]))
    if (!is.na(ref_date)) {
      return(format(ref_date, "%Y-%m-%d"))
    }
  }

  format(as.Date(fallback), "%Y-%m-%d")
}

has_forecasts_in_memory <- function(rv) {
  any(
    has_forecast_rows(rv$baseline_regular),
    has_forecast_rows(rv$baseline_seasonal),
    has_forecast_rows(rv$baseline_opt),
    has_forecast_rows(rv$inla),
    has_forecast_rows(rv$copycat),
    has_forecast_rows(rv$copycat_cal),
    has_forecast_rows(rv$gbqr),
    has_forecast_rows(rv$ensemble),
    length(rv$outside_models) > 0,
    length(rv$outside_model_validations) > 0,
    !is.null(rv$last_outside_model_validation)
  )
}

reset_forecast_state <- function(rv, session) {
  rv$baseline_regular <- NULL
  rv$baseline_seasonal <- NULL
  rv$baseline_opt <- NULL
  rv$inla <- NULL
  rv$copycat <- NULL
  rv$copycat_cal <- NULL
  rv$gbqr <- NULL
  rv$ensemble <- NULL
  rv$outside_models <- list()
  rv$outside_model_validations <- list()
  rv$last_outside_model_validation <- NULL

  output$baseline_regular_plots <- renderPlot(NULL)
  output$baseline_seasonal_plots <- renderPlot(NULL)
  output$baseline_opt_plots <- renderPlot(NULL)
  output$inla_plots <- renderPlot(NULL)
  output$copycat_plots <- renderPlot(NULL)
  output$copycat_cal_plots <- renderPlot(NULL)
  output$gbqr_plots <- renderPlot(NULL)
  output$ensemble_plots <- renderPlot(NULL)

  disable("baseline_regular_plot_download")
  disable("baseline_seasonal_plot_download")
  disable("baseline_opt_plot_download")
  disable("inla_plot_download")
  disable("copycat_plot_download")
  disable("copycat_cal_plot_download")
  disable("gbqr_plot_download")
  disable("ensemble_plot_download")
  disable("use_population_column")
  updateRadioButtons(session, "use_population_column", selected = "No")

  updateSelectizeInput(session, "ensemble_models", choices = NULL, selected = character(0), server = TRUE)
}

target_groups <- reactive({
  ## Pulls the list of target groups that will be forecasted and plotted
  req(rv$raw_data)
  rv$raw_data |> dplyr::distinct(target_group) |> dplyr::pull()
})

fcast_data <- reactive({
  ## Removes the appropriate weeks of data; primary data input for every model
  req(rv$raw_data, input$forecast_date, input$data_to_drop)
  get_fcast_data(df = rv$raw_data,
                 forecast_date = input$forecast_date,
                 data_to_drop = input$data_to_drop)
})

plot_data <- reactive({
  ## Removes data before forecast week and notes what will be dropped; used for plotting
  req(rv$raw_data, input$forecast_date, input$data_to_drop)
  get_plot_data(df = rv$raw_data,
                forecast_date = input$forecast_date,
                data_to_drop = input$data_to_drop)
})

fcast_horizon <- reactive({
  ## Calculates how many weeks ahead need to be forecasted
  req(fcast_data(), input$forecast_date, input$forecast_horizon)
  get_fcast_horizon(fcast_horizon = input$forecast_horizon,
                    data_df = fcast_data(),
                    forecast_date = input$forecast_date)
})

overall_type <- reactive({
  req(rv$raw_data)
  check_overall_completeness(rv$raw_data)
})
