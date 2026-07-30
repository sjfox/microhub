nav_panel(
  title = "Retrospective",
  layout_columns(
    col_widths = c(4, 8),
    card(
      strong("Data"),
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
          maxOptions = length(epizone_choices)
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
        selected = retrospective_default_model_choices
      ),
      div(
        style = "display:flex; gap:8px; margin:6px 0 14px 0;",
        actionButton(
          "select_all_retrospective_models",
          "Select All",
          style = "flex:1;"
        ),
        actionButton(
          "clear_retrospective_models",
          "Clear All",
          style = "flex:1;"
        )
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
      div(
        class = "alert alert-warning",
        style = "padding:10px 12px; margin-bottom:1rem;",
        tags$strong("Interpret with caution. "),
        "These retrospective results are calculated using the finalized uploaded dataset, not archived data snapshots as they were available in real time. Scores are useful for comparing models on this dataset, but they should not be interpreted as expected real-time forecast performance."
      ),
      card(
        card_header("Retrospective Summary"),
        uiOutput("retrospective_run_summary_ui")
      ),
      card(
        card_header("Forecast Visualization"),
        tags$p(
          class = "plot-helper-text",
          "Observed target data with thinned forecast medians and prediction intervals across the retrospective evaluation period."
        ),
        uiOutput("retrospective_forecast_plot_message_ui"),
        plotOutput("retrospective_ensemble_forecast_plot", height = "460px")
      ),
      card(
        card_header("Scoring Summary"),
        tags$p(
          class = "plot-helper-text",
          "Weighted interval score (WIS) is lower when forecasts are sharper and better calibrated. Relative WIS compares each model to Regular Baseline for the same forecast targets when Regular Baseline was run."
        ),
        navset_card_underline(
          nav_panel(
            "Overall",
            DTOutput("retrospective_score_overall_table")
          ),
          nav_panel(
            "Target Groups",
            downloadButton(
              "download_retrospective_score_target_group_plot",
              "Download Plot"
            ),
            plotOutput("retrospective_score_target_group_plot", height = "420px"),
            DTOutput("retrospective_score_target_group_table")
          ),
          nav_panel(
            "Forecast Dates",
            downloadButton(
              "download_retrospective_score_forecast_date_plot",
              "Download Plot"
            ),
            plotOutput("retrospective_score_forecast_date_plot", height = "420px"),
            DTOutput("retrospective_score_forecast_date_table")
          )
        )
      )
    )
  )
)
