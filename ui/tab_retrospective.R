nav_panel(
  title = "Retrospective",
  layout_columns(
    col_widths = c(4, 8),
    card(
      strong("Upload Retrospective Data"),
      fileInput(
        "retrospective_file",
        "Choose CSV File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      uiOutput("retrospective_upload_status_ui"),
      tags$hr(),
      strong("Retrospective Settings"),
      selectizeInput(
        inputId = "retrospective_country_select",
        label = "Local Seasonality",
        choices = epizone_choices,
        selected = "Paraguay",
        width = "100%",
        options = list(
          placeholder = "Type to search countries...",
          maxOptions = 8L
        )
      ),
      shinyjs::hidden(
        radioButtons(
          inputId = "retrospective_seasonality",
          label = NULL,
          choices = list("A" = "A", "B" = "B", "C" = "C", "D" = "D", "E" = "E"),
          selected = "E"
        )
      ),
      uiOutput("retrospective_zone_badge_ui"),
      selectInput(
        "retrospective_start_week",
        "First Reference Week",
        choices = NULL
      ),
      selectInput(
        "retrospective_end_week",
        "Last Reference Week",
        choices = NULL
      ),
      numericInput(
        "retrospective_horizon",
        "Forecast Horizon (Weeks)",
        value = 4,
        min = 1,
        max = 6
      ),
      checkboxGroupInput(
        "retrospective_models",
        "Models",
        choices = retrospective_model_choices,
        selected = retrospective_model_choices
      ),
      actionButton(
        "run_retrospective",
        "Run Retrospective"
      ),
      downloadButton(
        "download_retrospective_zip",
        "Download Retrospective ZIP"
      )
    ),
    tags$div(
      class = "data-tab-scroll-panel",
      card(
        card_header("Run Status"),
        uiOutput("retrospective_run_summary_ui"),
        DTOutput("retrospective_status_table")
      ),
      card(
        card_header("Output Files"),
        DTOutput("retrospective_files_table")
      ),
      card(
        card_header("Data Preview"),
        DTOutput("retrospective_data_preview")
      )
    )
  )
)
