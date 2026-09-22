control_section <- function(title, ...) {
  tags$div(
    class = "control-section",
    tags$div(class = "control-section-title", title),
    ...
  )
}

# ui_summary() -----------------------------------------------------------------
# Reads a model tab's short inline summary from www/content/summaries/<name>.txt
# instead of a hardcoded string literal in the ui/tab_*.R file. Plain .txt
# (not .md rendered via includeMarkdown()) on purpose: includeMarkdown() emits
# its own block-level wrapper, which would break the current layout where this
# text sits inline inside a single <p> right before the "More details" link.
ui_summary <- function(name) {
  content_root <- if (exists("modal_registry_root", inherits = TRUE)) {
    modal_registry_root
  } else {
    getwd()
  }
  path <- file.path(content_root, "www", "content", "summaries", paste0(name, ".txt"))
  if (!file.exists(path)) {
    stop(
      "ui_summary(): no summary file found at '", path, "'. ",
      "Add www/content/summaries/", name, ".txt, or fix the name passed in.",
      call. = FALSE
    )
  }
  paste(readLines(path, warn = FALSE), collapse = " ")
}

# modal_info_link() -------------------------------------------------------------
# The single way to create a help-icon / "More details" link for a modal.
# Validates against modal_registry (R/modal_registry.R) at UI-build time, so a
# typo'd or unregistered modal id fails loudly here instead of silently doing
# nothing when clicked. Covers both call shapes used across the app: an
# icon-only info link next to a field label (defaults), and a text link with a
# leading icon (pass label = "..." and icon = icon(...)).
modal_info_link <- function(
  id,
  label = icon("info-circle"),
  icon = NULL,
  style = "margin-left: 5px;"
) {
  if (!id %in% modal_registry$id) {
    stop(
      "modal_info_link(): '", id, "' is not in modal_registry ",
      "(see R/modal_registry.R). Add a row there before using this id in the UI.",
      call. = FALSE
    )
  }
  actionLink(inputId = id, label = label, icon = icon, style = style)
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
        modal_info_link(
          methodology_link_id,
          label = "More details",
          style = NULL
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
