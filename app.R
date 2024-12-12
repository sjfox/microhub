library(shiny)
library(bslib)

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
          value = Sys.Date()
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
        tableOutput("data_preview")
      )
    ) # end page_sidebar
  ), # end nav_panel

  ## INLA tab ------------------------------------------------------------------
  nav_panel(
    title = "INLA",
    page_sidebar(
      sidebar = sidebar(
        open = "always",
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
        actionButton(
          "run_copycat",
          "Run Copycat"
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

  ## Download tab --------------------------------------------------------------
  nav_panel(
    title = "Download",
    page_sidebar(
      sidebar = sidebar(
        open = "always",
        downloadButton(
          "download_results",
          "Download Results"
        )
      ), # end sidebar
      card(
        card_header("Results Preview"),
        tableOutput("results_preview")
      )
    ) # end page_sidebar
  ) # end nav_panel

) # end page_navbar

# Define server ================================================================
server <- function(input, output, session) {

  # Reactive values to store data and forecasts
  rv <- reactiveValues(
    data = NULL,
    all_forecasts = list()
  )

  # Read uploaded data
  observeEvent(input$dataframe, {
    rv$data <- read.csv(input$dataframe$datapath)
  })

  # Data preview
  output$data_preview <- renderTable({
    req(rv$data)
    rv$data
  })
}

shinyApp(ui, server)
