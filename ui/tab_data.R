nav_panel(
  title = "Data Upload & Settings",
  layout_columns(
    col_widths = c(4, 8),
    card(
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
      div(id = "error_message"),
      strong("Settings for All Models"),
      dateInput(
        "forecast_date",
        label = tagList(
          "Forecast Date",
          actionLink(
            "modal_forecast_date",
            icon("info-circle"),
            style = "margin-left: 5px;"
          )
        ),
        value = closest_wednesday(Sys.Date())
      ),
      selectInput(
        "data_to_drop",
        label = tagList(
          "Data to Drop",
          actionLink(
            "modal_data_drop",
            icon("info-circle"),
            style = "margin-left: 5px;"
          )
        ),
        choices = c("0 weeks",
                    "1 week" = "1 week",
                    "2 weeks" = "2 week",
                    "3 weeks" = "3 week",
                    "4 weeks" = "4 week"),
        selected = "0 weeks"
      ),
      # Country selector — drives the hidden seasonality radio below
      selectizeInput(
        inputId  = "country_select",
        label = tagList(
          "Local Seasonality",
          actionLink(
            "modal_seasonality",
            icon("info-circle"),
            style = "margin-left: 5px;"
          )
        ),
        choices  = epizone_choices,
        selected = "Paraguay",
        width    = "100%",
        options  = list(
          placeholder = "Type to search countries\u2026",
          maxOptions  = 8L
        )
      ),
      # Zone badge — updates reactively when country changes
      uiOutput("zone_badge_ui"),
      # Hidden radio — still read by all models as input$seasonality
      shinyjs::hidden(
        radioButtons(
          inputId  = "seasonality",
          label    = NULL,
          choices  = list("A" = "A", "B" = "B", "C" = "C", "D" = "D", "E" = "E"),
          selected = "E"   # Paraguay default
        )
      ),
      numericInput(
        "forecast_horizon",
        label = tagList(
          "Forecast Horizon (Weeks)",
          actionLink(
            "modal_forecast_horizon",
            icon("info-circle"),
            style = "margin-left: 5px;"
          )
        ),
        value = 4,
        min = 1,
        max = 6
      ),
      tags$hr(),
      strong("Download Data Template"),
      helpText(HTML("Download the template and replace the example data with your target data.")),
      actionLink(
        "modal_template",
        " See instructions for using the data template.",
        icon = icon("circle-info"),
        style = "font-size: .875em;"
      ),
      radioButtons(
        "template_choice",
        label = NULL,
        choices = c(
          "Without population" = "microhub-template.csv",
          "With population"    = "microhub-template-population.csv"
        ),
        selected = "microhub-template.csv"
      ),
      downloadButton(
        "download_template",
        label = "Download Template (.csv)"
      )
    ), # end card
    tags$div(
      class = "data-tab-scroll-panel",
      card(
        card_header("Uploaded Time Series"),
        plotOutput("uploaded_time_series_plot", height = "450px")
      ),
      card(
        card_header("Respiratory Season Comparison"),
        plotOutput("uploaded_resp_season_plot", height = "450px")
      ),
      card(
        card_header("Data Preview"),
        DTOutput("data_preview")
      )
    )
  ) # end layout_columns
) # end nav_panel Data Upload & Settings
