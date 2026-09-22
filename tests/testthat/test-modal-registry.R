# Regression tests for the modal registry (R/modal_registry.R).
#
# These exist to catch, automatically, the exact class of bug that a manual
# UI audit previously found by hand: a UI trigger (actionLink/modal_info_link)
# referencing an input id with no corresponding server observer, or a
# methodology link pointed at the wrong content file. Since server/modals.R
# now generates its observers directly from modal_registry, the only way that
# bug class can reappear is a ui/*.R file referencing a modal id that was
# never added to modal_registry — which is exactly what the first test below
# scans for.

test_that("every modal_* id referenced in ui/*.R is registered in modal_registry", {
  ui_files <- list.files(
    test_path("../../ui"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  expect_true(length(ui_files) > 0)

  ids_found <- unique(unlist(lapply(ui_files, function(f) {
    lines <- readLines(f, warn = FALSE)
    matches <- regmatches(lines, gregexpr('"(modal_[A-Za-z0-9_]+)"', lines))
    gsub('"', "", unlist(matches))
  })))

  missing <- setdiff(ids_found, modal_registry$id)
  expect_true(
    length(missing) == 0,
    info = paste(
      "ui/*.R references modal id(s) with no modal_registry row:",
      paste(missing, collapse = ", ")
    )
  )
})

test_that("modal_registry ids and dom ids are unique", {
  expect_false(anyDuplicated(modal_registry$id) > 0)
  expect_false(anyDuplicated(modal_registry$dom_id) > 0)
})

test_that("every modal_registry content file exists under www/content/", {
  paths <- file.path(test_path("../.."), "www", "content", paste0(modal_registry$md, ".md"))
  missing <- modal_registry$md[!file.exists(paths)]
  expect_true(
    length(missing) == 0,
    info = paste("Missing www/content/*.md for:", paste(missing, collapse = ", "))
  )
})

test_that("modal_info_link() rejects an id that is not in modal_registry", {
  expect_error(
    modal_info_link("modal_totally_made_up"),
    "not in modal_registry"
  )
})

test_that("ui_summary() reads a real summary file and errors on an unknown name", {
  txt <- ui_summary("ensemble")
  expect_true(is.character(txt))
  expect_true(nchar(txt) > 0)

  expect_error(
    ui_summary("this_summary_does_not_exist"),
    "no summary file found"
  )
})
