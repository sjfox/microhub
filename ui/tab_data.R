nav_panel(
  title = "Data",
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
      uiOutput("active_dataset_ui"),
      div(id = "error_message"),
      tags$hr(),
      strong("Settings for All Models"),
      radioButtons(
        "data_type",
        label = tagList(
          "Data Type",
          modal_info_link("modal_data_type")
        ),
        choices = c("Counts" = "count", "Proportion (0-1)" = "proportion"),
        selected = "count"
      ),
      dateInput(
        "forecast_date",
        label = tagList(
          "Forecast Date",
          modal_info_link("modal_forecast_date")
        ),
        value = Sys.Date()
      ),
      selectInput(
        "data_to_drop",
        label = tagList(
          "Data to Drop",
          modal_info_link("modal_data_drop")
        ),
        choices = c("0 weeks",
                    "1 week" = "1 week",
                    "2 weeks" = "2 week",
                    "3 weeks" = "3 week",
                    "4 weeks" = "4 week"),
        selected = "0 weeks"
      ),
      selectInput(
        "forecast_output",
        label = "Forecast output",
        choices = c(
          "All" = "all",
          "Horizon >= 0" = "horizon_gte_0"
        ),
        selected = "all"
      ),
      # Country selector — drives the hidden seasonality radio below
      selectizeInput(
        inputId  = "country_select",
        label = tagList(
          "Local Seasonality",
          modal_info_link("modal_seasonality")
        ),
        choices  = epizone_choices,
        selected = "Paraguay",
        width    = "100%",
        options  = list(
          placeholder = "Type to search countries\u2026",
          maxOptions  = length(epizone_choices)
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
          modal_info_link("modal_forecast_horizon")
        ),
        value = 4,
        min = 1,
        max = 6
      ),
      tags$hr(),
      strong("Run Models"),
      helpText(HTML("Pick the models to run with their default settings and the shared data settings above. The Ensemble combines whichever non-baseline models succeed, so it runs last.")),
      checkboxGroupInput(
        "run_all_models",
        label = NULL,
        choiceNames = model_choices_with_divider(
          run_all_model_choices,
          retrospective_development_model_choices
        ),
        choiceValues = unname(run_all_model_choices),
        selected = run_all_default_model_choices
      ),
      div(
        style = "display:flex; gap:8px; margin-bottom:10px;",
        actionButton("select_all_run_models", "Select All", class = "btn-sm btn-outline-secondary", style = "flex:1;"),
        actionButton("clear_all_run_models", "Clear All", class = "btn-sm btn-outline-secondary", style = "flex:1;")
      ),
      actionButton(
        "run_all_default_models",
        "Run Selected Models"
      ),
      uiOutput("run_all_status_ui"),
      tags$hr(),
      strong("Neighbor Graph (optional)"),
      helpText(
        HTML(
          "A two-column CSV naming which target groups border each other. ",
          "Upload one to make the spatial structure available to INFLAenza."
        )
      ),
      uiOutput("neighbor_graph_template_ui"),
      fileInput(
        "neighbor_graph_file",
        label = NULL,
        buttonLabel = "Browse...",
        placeholder = "Upload neighbor graph (.csv)",
        accept = ".csv",
        width = "100%"
      ),
      uiOutput("neighbor_graph_status_ui"),
      tags$hr(),
      strong("Seasonal Groups (optional)"),
      helpText(
        HTML(
          "A two-column CSV assigning target groups to seasonal groups, for ",
          "regions whose seasonality differs from the majority. List only the ",
          "exceptions; everything else shares one curve."
        )
      ),
      uiOutput("season_groups_template_ui"),
      fileInput(
        "season_groups_file",
        label = NULL,
        buttonLabel = "Browse...",
        placeholder = "Upload seasonal groups (.csv)",
        accept = ".csv",
        width = "100%"
      ),
      uiOutput("season_groups_status_ui"),
      tags$hr(),
      strong("Download Data Template"),
      helpText(HTML("Download the template and replace the example data with your target data.")),
      modal_info_link(
        "modal_template",
        label = " See instructions for using the data template.",
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
        card_header("Target Group"),
        layout_columns(
          col_widths = c(2, 8, 2),
          actionButton(
            "previous_data_target_group",
            NULL,
            icon = icon("chevron-left"),
            width = "100%"
          ),
          selectizeInput(
            "data_target_group_select",
            label = NULL,
            choices = NULL,
            width = "100%",
            options = list(
              placeholder = "Choose target group...",
              maxOptions = 12L
            )
          ),
          actionButton(
            "next_data_target_group",
            NULL,
            icon = icon("chevron-right"),
            width = "100%"
          )
        ),
        uiOutput("data_target_group_position_ui")
      ),
      card(
        card_header("Uploaded Time Series"),
        plotOutput("uploaded_time_series_plot", height = "380px")
      ),
      card(
        card_header("Respiratory Season Comparison"),
        plotOutput("uploaded_resp_season_plot", height = "380px")
      ),
      card(
        card_header("Data Preview"),
        DTOutput("data_preview")
      )
    )
  ) # end layout_columns
) # end nav_panel Data
