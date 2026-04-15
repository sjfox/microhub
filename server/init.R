# Initialize app ==============================================================

# Reactive values to store data and forecasts
rv <- reactiveValues(
  raw_data = NULL,
  valid_data = NULL,
  quantiles_needed = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
  baseline_regular = NULL,
  baseline_seasonal = NULL,
  baseline_opt = NULL,
  inla = NULL,
  valid_pop = NULL,
  copycat     = NULL,
  calcopycat  = NULL,
  gbqr        = NULL,
  fourcat     = NULL,
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
disable("run_calcopycat")
disable("calcopycat_plot_download")
disable("run_gbqr")
disable("gbqr_plot_download")
disable("run_ensemble")
disable("ensemble_plot_download")
disable("download_results")

# Disable use_population_data button initially
disable("use_population_column")

# Core reactives --------------------------------------------------------------

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
  req(input$data_to_drop, input$forecast_horizon)
  get_fcast_horizon(fcast_horizon = input$forecast_horizon,
                    data_to_drop = input$data_to_drop)
})

overall_type <- reactive({
  req(rv$raw_data)
  check_overall_completeness(rv$raw_data)
})
