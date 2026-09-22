# Modal observers ==============================================================
# Every help/methodology modal is generated from modal_registry (see
# R/modal_registry.R): one row = one input id, dialog title, DOM id, and
# content file. This loop is the only place that wires input$<id> to
# show_modal(), so a UI actionLink and its server handler can never drift out
# of sync the way the old hand-written observeEvent() blocks occasionally did
# (a missing handler and two methodology links pointed at the wrong content
# file were all found and fixed by hand before this registry existed).
#
# To add a new modal: add a row to modal_registry and use modal_info_link()
# (R/ui_helpers.R) at the UI call site. Nothing needs to change here.

invisible(lapply(seq_len(nrow(modal_registry)), function(i) {
  this_id    <- modal_registry$id[i]
  this_title <- modal_registry$title[i]
  this_dom   <- modal_registry$dom_id[i]
  this_md    <- modal_registry$md[i]

  observeEvent(input[[this_id]], {
    show_modal(title = this_title, id = this_dom, md = this_md)
  })
}))
