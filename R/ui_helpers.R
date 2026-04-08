control_section <- function(title, ...) {
  tags$div(
    class = "control-section",
    tags$div(class = "control-section-title", title),
    ...
  )
}

model_tab_shell <- function(
  summary_text,
  methodology_link_id,
  controls,
  plot_output,
  download_button
) {
  tagList(
    tags$div(
      class = "model-summary-panel",
      p(
        class = "model-summary-text compact-model-summary",
        summary_text,
        " ",
        actionLink(
          inputId = methodology_link_id,
          label = "More details"
        )
      )
    ),
    layout_columns(
      col_widths = c(4, 8),
      card(
        class = "app-card controls-card",
        card_header("Run & Settings"),
        controls
      ),
      tags$div(
        class = "model-results-scroll-panel",
        card(
          class = "app-card plot-card",
          card_header("Forecast Plot"),
          tags$p(
            class = "plot-helper-text",
            "Run the model to generate forecast plots. The plot area stays fixed so results remain easy to compare across tabs."
          ),
          plot_output,
          download_button
        )
      )
    )
  )
}
