nav_panel(
  title = "Download",
  tags$div(
    class = "page-section",
    tags$div(
      class = "model-summary-panel",
      tags$p(
        class = "compact-model-summary",
        "Download the forecasts you have generated as a single standardized results file. Use the preview below to quickly check the output before exporting."
      )
    ),
    layout_columns(
      col_widths = c(4, 8),
      card(
        class = "app-card controls-card",
        card_header("Export"),
        tags$p(
          class = "plot-helper-text",
          "Choose which forecast models to include, then export the filtered results as one CSV file."
        ),
        selectizeInput(
          "download_models",
          "Models to include:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            placeholder = "Run models to populate this list"
          )
        ),
        downloadButton(
          "download_results",
          "Download Results (.csv)"
        ),
        tags$hr(),
        tags$p(
          class = "plot-helper-text",
          "Export every generated app plot as a multi-page PDF, one model per page."
        ),
        downloadButton(
          "download_plots_pdf",
          "Download Plots (.pdf)"
        ),
        tags$hr(),
        tags$p(
          class = "plot-helper-text",
          "Generate a full ensemble forecast report (PDF): run configuration, data handling, and per-target-group forecasts vs the previous season. Requires a generated Ensemble."
        ),
        selectInput(
          "report_language",
          "Report language:",
          choices = c("English" = "en", "Español" = "es", "Português" = "pt"),
          selected = "en"
        ),
        downloadButton(
          "download_report_pdf",
          "Download Report (.pdf)"
        )
      ),
      card(
        class = "app-card plot-card",
        card_header("Results Preview"),
        tags$p(
          class = "plot-helper-text",
          "Preview the combined output table before downloading."
        ),
        DTOutput("results_preview")
      )
    )
  )
) # end nav_panel Download
