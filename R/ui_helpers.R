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


#' Labels for a model checkboxGroupInput, with a heading above the first
#' development model.
#'
#' Shiny's checkboxGroupInput has no notion of option groups, so the heading is
#' folded into the first development model's label via `choiceNames`. It is
#' marked `pointer-events: none` in styles.css so clicking the heading text does
#' not toggle that checkbox.
#'
#' The break is located from `development_ids` rather than a hardcoded index, so
#' it follows the list if models are added or removed -- but it only renders one
#' break, so development entries must be contiguous at the end of `choices`
#' (which is how both retrospective_model_choices and run_all_model_choices are
#' ordered). If they are not, no heading is drawn rather than a misplaced one.
#'
#' @param choices named character vector, names = labels, values = ids
#' @param development_ids ids to place below the divider
#' @param heading text for the divider
#' @return a list suitable for `choiceNames`, matching `unname(choices)` for
#'   `choiceValues`
model_choices_with_divider <- function(choices,
                                       development_ids,
                                       heading = "Development models") {
  ids <- unname(choices)
  labels <- names(choices)

  is_dev <- ids %in% development_ids
  first_dev <- if (any(is_dev)) which(is_dev)[1] else NA_integer_

  # Only draw a break when the development entries really are a contiguous
  # trailing block; otherwise a single divider would misrepresent the grouping.
  contiguous <- !is.na(first_dev) && all(is_dev[first_dev:length(ids)])

  # Styled inline rather than only via the .model-group-divider class in
  # www/styles.css. Shiny serves static assets with caching, so a stylesheet
  # change can lag a code change in an already-open browser -- which showed up
  # here as the heading rendering as plain text run together with the first
  # model name. Inline styles ship with the HTML, so they cannot go stale, and
  # they outrank anything Bootstrap applies to label content. The class is kept
  # so the look can still be overridden from the stylesheet.
  divider_style <- paste(
    "display:block;",
    "margin:12px 0 6px -1.5em;",   # -1.5em pulls back past Bootstrap's checkbox indent
    "padding-top:10px;",
    "border-top:1px solid #dee2e6;",
    "font-size:0.78em;",
    "font-weight:600;",
    "letter-spacing:0.05em;",
    "text-transform:uppercase;",
    "color:#6c757d;",
    "pointer-events:none;",        # a click on the heading must not tick the box
    collapse = " "
  )

  lapply(seq_along(ids), function(i) {
    if (contiguous && i == first_dev) {
      tagList(
        tags$span(
          class = "model-group-divider",
          style = divider_style,
          heading
        ),
        labels[[i]]
      )
    } else {
      labels[[i]]
    }
  })
}
