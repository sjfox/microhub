# Attach libraries =============================================================

library(shiny)
library(bslib)
library(DT)

# Source R scripts =============================================================

source("R/inla.R")
source("R/sirsea.R")
source("R/copycat.R")
source("R/plot.R")

# Set global options ===========================================================

options(
  # Set timeout to one hour
  shiny.timeout = 3600
  )

# Define UI ====================================================================

ui <- page_navbar(
  title = "Forecasting Tool",
  theme = bs_theme(
    version = 5,
    bootswatch = "yeti",
    primary = "#BA0C2F",
    secondary = "#C8D8EB",
    header_font = font_google("Oswald"),
    base_font = font_google("Merriweather Sans")
  ),

  ## Data upload and common inputs ---------------------------------------------

  nav_panel(
    title = "Data Upload & Settings",
    page_sidebar(
      sidebar = sidebar(
        open = "always",
        width = 400,
        strong("Upload Data"),
        fileInput(
          "dataframe",
          "Choose CSV File",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr(),
        strong("Settings for All Models"),
        dateInput(
          "forecast_date",
          "Forecast Date",
          value = "2024-07-13"
        ),
        selectInput(
          "data_to_drop",
          "Data to Drop",
          choices = c("1 week", "2 weeks" = "2 week", "3 weeks" = "3 week")
        ),
        numericInput(
          "forecast_horizon",
          "Forecast Horizon (Weeks)",
          value = 4,
          min = 1,
          max = 52
        )
      ), # end sidebar
      card(
        card_header("Data Preview"),
        DTOutput("data_preview")
      )
    ) # end page_sidebar
  ), # end nav_panel

  ## INLA tab ------------------------------------------------------------------

  nav_panel(
    title = "INLA",
    page_sidebar(
      sidebar = sidebar(
        open = "always",
        width = 400,
        strong("Configure INLA Model"),
        selectInput(
          "ar_order",
          "Order of AR",
          choices = c(1, 2, 3)
        ),
        selectInput(
          "rw_order",
          "Order of RW",
          choices = c(1, 2)
        ),
        selectInput(
          "seasonal_smoothness",
          "Seasonal Smoothness",
          choices = c("Default" = "default", "Small" = "small", "Tiny" = "tiny")
        ),
        selectInput(
          "forecast_uncertainty_parameter",
          "Forecast Uncertainty Parameter",
          choices = c("Default" = "default", "More" = "more", "Less" = "less")
        ),
        tags$hr(),
        actionButton(
          "run_inla",
          "Run INLA"
        )
      ), # end sidebar
      card(
        card_header("Pediatric Cases"),
        plotOutput("inla_pediatric")
      ),
      card(
        card_header("Adult Cases"),
        plotOutput("inla_adult")
      ),
      card(
        card_header("Overall Cases"),
        plotOutput("inla_overall")
      )
    ) # end page_sidebar
  ), # end nav_panel

  ## SIRsea tab ----------------------------------------------------------------

  nav_panel(
    title = "SIRsea",
    page_sidebar(
      sidebar = sidebar(
        open = "always",
        width = 400,
        helpText(HTML("SIRsea uses CmdStan. Please see the <a href='https://mc-stan.org/cmdstanr/articles/cmdstanr.html' target='_blank'>Getting started with CmdStan article</a> for installation instructions. Once installed, run cmdstan_path() in your R console to get the path to your local CmdStan installation to paste in the below text box.")),
        textInput(
          "cmdstan_path",
          "Path to CmdStan",
          value = "C:/Users/jryan/.cmdstan/cmdstan-2.36.0"
        ),
        tags$hr(),
        actionButton(
          "run_sirsea",
          "Run SIRsea"
        )
      ), # end sidebar
      card(
        card_header("Pediatric Cases"),
        plotOutput("sirsea_pediatric")
      ),
      card(
        card_header("Adult Cases"),
        plotOutput("sirsea_adult")
      ),
      card(
        card_header("Overall Cases"),
        plotOutput("sirsea_overall")
      )
    ) # end page_sidebar
  ), # end nav_panel

  ## Copycat tab ---------------------------------------------------------------

  nav_panel(
    title = "Copycat",
    page_sidebar(
      sidebar = sidebar(
        open = "always",
        width = 400,
        strong("Configure Copycat Model"),
        selectInput(
          "recent_weeks_touse",
          "Recent Weeks to Use",
          choices = c(5, 10, 15, 20, 100)
        ),
        selectInput(
          "resp_week_range",
          "Resp Week Range",
          choices = 0:6,
          selected = 2
        ),
        tags$hr(),
        actionButton(
          "run_copycat",
          "Run Copycat"
        )
      ), # end sidebar
      card(
        card_header("Pediatric Cases"),
        plotOutput("copycat_pediatric")
      ),
      card(
        card_header("Adult Cases"),
        plotOutput("copycat_adult")
      ),
      card(
        card_header("Overall Cases"),
        plotOutput("copycat_overall")
      )
    ) # end page_sidebar
  ), # end nav_panel

  ## Download tab --------------------------------------------------------------

  nav_panel(
    title = "Download",
    page_sidebar(
      sidebar = sidebar(
        open = "always",
        width = 200,
        downloadButton(
          "download_results",
          "Download Results"
        )
      ), # end sidebar
      card(
        card_header("Results Preview"),
        DTOutput("results_preview")
      )
    ) # end page_sidebar
  ) # end nav_panel
) # end page_navbar

# Define server ================================================================

server <- function(input, output, session) {
  # Reactive values to store data and forecasts
  rv <- reactiveValues(
    data = NULL,
    inla = NULL,
    sirsea = NULL,
    copycat = NULL
  )

  # Read uploaded data
  observeEvent(input$dataframe, {
    rv$data <- read.csv(input$dataframe$datapath)
  })

  # Data preview
  output$data_preview <- renderDT(
    datatable(rv$data, rownames = FALSE, filter = "top")
  )

  ## INLA ----------------------------------------------------------------------

  observeEvent(input$run_inla, {
    req(rv$data)

    # Wrangle
    fitted_data <- wrangle_inla(
      dataframe = rv$data,
      forecast_date = input$forecast_date,
      data_to_drop = input$data_to_drop,
      forecast_horizons = input$forecast_horizon
    )

    # Fit
    inla_results <- fit_process_inla(
      fit_df = fitted_data,
      forecast_date = input$forecast_date,
      ar_order = input$ar_order,
      rw_order = input$rw_order,
      seasonal_smoothness = input$seasonal_smoothness,
      forecast_uncertainty_parameter = input$forecast_uncertainty_parameter
    )

    # Save to reactive values
    rv$inla <- inla_results |>
      mutate(model = "INLA", .before = 1)

    # Plot
    inla_plot_df <- prepare_historic_data(
      rv$data,
      inla_results,
      input$forecast_date
    )

    inla_plots <- c("Pediatric", "Adult", "Overall") |>
      map(
        plot_state_forecast_try,
        forecast_date = input$forecast_date,
        curr_season_data = inla_plot_df$curr_season_data,
        forecast_df = inla_plot_df$forecast_df,
        historic_data = inla_plot_df$historic_data
      )

    output$inla_pediatric <- renderPlot({
      inla_plots[[1]]
    })

    output$inla_adult <- renderPlot({
      inla_plots[[2]]
    })

    output$inla_overall <- renderPlot({
      inla_plots[[3]]
    })
  })

  ## SIRsea --------------------------------------------------------------------

  observeEvent(input$run_sirsea, {
    req(rv$data)
    req(input$cmdstan_path)

    # Wrangle
    stan_input <- wrangle_sirsea(
      dataframe = rv$data,
      forecast_date = input$forecast_date,
      data_to_drop = input$data_to_drop,
      forecast_horizons = input$forecast_horizon
    )

    # Fit
    sirsea_results <- fit_process_sirsea(
      dataframe = stan_input$subset_data,
      stan_dat = stan_input$stan_dat,
      forecast_date = input$forecast_date,
      data_to_drop = input$data_to_drop,
      cmdstan_path = input$cmdstan_path
    )

    # Save to reactive values
    rv$sirsea <- sirsea_results |>
      mutate(model = "SIRsea", .before = 1)

    # Plot
    sirsea_plot_df <- prepare_historic_data(
      rv$data,
      sirsea_results,
      input$forecast_date
    )

    sirsea_plots <- c("Pediatric", "Adult", "Overall") |>
      map(
        plot_state_forecast_try,
        forecast_date = input$forecast_date,
        curr_season_data = sirsea_plot_df$curr_season_data,
        forecast_df = sirsea_plot_df$forecast_df,
        historic_data = sirsea_plot_df$historic_data
      )

    output$sirsea_pediatric <- renderPlot({
      sirsea_plots[[1]]
    })

    output$sirsea_adult <- renderPlot({
      sirsea_plots[[2]]
    })

    output$sirsea_overall <- renderPlot({
      sirsea_plots[[3]]
    })
  })

  ## Copycat -------------------------------------------------------------------

  observeEvent(input$run_copycat, {
    req(rv$data)

    # Wrangle
    copycat_input <- wrangle_copycat(
      dataframe = rv$data,
      forecast_date = input$forecast_date
    )

    # Fit
    copycat_results <- fit_process_copycat(
      fit_df = copycat_input$recent_sari,
      historic_df = copycat_input$historic_sari,
      forecast_date = input$forecast_date,
      data_to_drop = input$data_to_drop,
      forecast_horizon = input$forecast_horizon,
      recent_weeks_touse = input$recent_weeks_touse,
      resp_week_range = input$resp_week_range
    )

    # Save to reactive values
    rv$copycat <- copycat_results |>
      mutate(model = "Copycat", .before = 1)

    # Plot
    copycat_plot_df <- prepare_historic_data(
      rv$data,
      copycat_results,
      input$forecast_date
    )

    copycat_plots <- c("Pediatric", "Adult", "Overall") |>
      map(
        plot_state_forecast_try,
        forecast_date = input$forecast_date,
        curr_season_data = copycat_plot_df$curr_season_data,
        forecast_df = copycat_plot_df$forecast_df,
        historic_data = copycat_plot_df$historic_data
      )

    output$copycat_pediatric <- renderPlot({
      copycat_plots[[1]]
    })

    output$copycat_adult <- renderPlot({
      copycat_plots[[2]]
    })

    output$copycat_overall <- renderPlot({
      copycat_plots[[3]]
    })
  })

  # Download tab ---------------------------------------------------------------

  # Data preview
  combined_results <- reactive({
    req(rv$data)
    bind_rows(rv$inla, rv$sirsea, rv$copycat) |>
      mutate(
        model = factor(model),
        reference_date = format(reference_date, "%Y-%m-%d"),
        target_end_date = format(target_end_date, "%Y-%m-%d"),
        horizon = round(horizon, 0),
        value = round(value, 0))
  })

  output$results_preview <- renderDT(
    datatable(combined_results(), rownames = FALSE, filter = "top")
  )

  # Download button
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("model-output_", Sys.Date(), ".csv")
    },
    content = function(filename) {
      write.csv(x = combined_results(), file = filename, row.names = FALSE)
    }
  )
} # end server

shinyApp(ui, server)
