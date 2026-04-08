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
          "Export all currently available forecasts in one CSV file."
        ),
        downloadButton(
          "download_results",
          "Download Results (.csv)"
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
