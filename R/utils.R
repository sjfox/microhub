show_modal <- function(title, id, md) {
  showModal(
    modalDialog(
      title = title,
      tags$div(
        id = id,
        includeMarkdown(normalizePath(paste0("www/content/", md, ".md"))),
      ),
      easyClose = TRUE,
      size = "l"
    )
  )
}