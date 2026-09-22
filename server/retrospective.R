# Retrospective forecasting ===================================================

retrospective <- reactiveValues(
  raw_data = NULL,
  valid_data = FALSE,
  upload_errors = NULL,
  upload_name = NULL,
  result = NULL,
  result_stale = FALSE,
  # TRUE only when a change actually invalidates the training windows behind
  # the current result (reference date range, horizon, or seasonality) --
  # see mark_retrospective_result_stale(). A model configuration being added
  # or removed leaves this FALSE, since that can be applied to the existing
  # result incrementally (see add_retrospective_run_configs() in
  # R/retrospective.R and the input$run_retrospective handler below) rather
  # than requiring a full fresh run.
  time_period_stale = FALSE,
  stale_modal_shown = FALSE,
  pending_upload = NULL,
  # The retrospective tab has its own dataset, so it needs its own neighbor
  # graph validated against THAT dataset's target groups -- the Data tab's
  # rv$neighbor_graph was checked against a possibly different group set.
  neighbor_graph = NULL,
  neighbor_graph_validation = NULL,
  neighbor_graph_name = NULL,
  season_groups = NULL,
  season_groups_validation = NULL,
  season_groups_name = NULL,
  configs = NULL,
  # Global ensemble member selection (model labels), shared across every
  # retrospective group -- NOT keyed per group. "Run Ensemble" applies this
  # same set (skipping any group a given member didn't complete in) to
  # every group at once, so "the ensemble" always means the same models no
  # matter which group is being viewed. See the
  # input$run_retrospective_ensemble handler further down.
  ensemble_members = character(),
  # Global Scoring Reference (baseline model) choice, shared across every
  # retrospective group -- NOT keyed per group, same reasoning as
  # ensemble_members above. Holds the user's raw pick; a group missing that
  # model still falls back via retrospective_resolve_reference_model()'s
  # normal fallback chain, so a specific group's actually-resolved reference
  # (result$scoring_reference[[group]]) can legitimately differ from this --
  # surfaced as a caveat in the all-groups summary rather than hidden. Seeded
  # after every run/load via retrospective_initial_scoring_reference().
  scoring_reference_choice = character(),
  # The user's own optional shorthand label for the CURRENT run -- purely
  # descriptive (see retrospective_sanitize_run_name()/retrospective_run_
  # folder_stamp() in R/retrospective.R). Set from input$retrospective_run_name
  # when a fresh run starts (left untouched across an incremental "add a
  # model" run, since that's still the same run), and restored from the
  # loaded run's own saved settings by process_retrospective_load(). Shown
  # in output$retrospective_run_summary_ui.
  run_name = NULL,
  # "Load Previous Run" state -- see process_retrospective_load() and the
  # input$retrospective_load_run_dir observer further down.
  pending_load_dir = NULL,
  # Zone to pre-select for each group's country selectize the NEXT time
  # retrospective_group_seasonality_ui renders (consumed and cleared by that
  # renderUI) -- set by process_retrospective_load() so a loaded grouped
  # run's per-group seasonality is restored without racing the dynamic
  # selectize inputs into existence (see that renderUI for why this has to
  # be baked into their initial `selected=` rather than pushed via
  # updateSelectizeInput() after the fact).
  pending_group_seasonality = NULL,
  # Same idea as pending_group_seasonality above, but for each group's Data
  # Type radio buttons -- set by process_retrospective_load() so a loaded
  # grouped run's per-group Data Type is restored the same way.
  pending_group_data_type = NULL,
  # While TRUE, mark_retrospective_result_stale() is a no-op. Set around the
  # batch of update*Input() calls process_retrospective_load() makes to
  # restore a loaded run's settings -- those calls echo back as ordinary
  # input changes a moment later, and without this guard the very same
  # observers that watch for a *user* editing the setup (config add/remove,
  # start/end week, horizon, seasonality) would immediately flag the just-
  # loaded result as stale, defeating "loaded run starts a session".
  suppress_stale_marking = FALSE
)

disable("run_retrospective")
disable("download_retrospective_zip")

# Fixed name for the optional "run every model per group" column (see the
# multi-country retrospective feature). A CSV without this column behaves
# exactly as before.
retrospective_group_col_name <- "retrospective_group"

# Retrospective's own Data Type setting -- independent of the live Data
# tab's input$data_type, since retrospective has its own, separate file
# upload (input$retrospective_file) and shouldn't silently inherit whatever
# the live tab happens to be set to. Threaded down into every
# fit_process_*() call, format_retrospective_forecasts(),
# build_retrospective_ensemble(), and validate_data(), mirroring how
# data_type() (server/init.R) flows through the live tabs.
retrospective_data_type <- reactive({
  input_or_default(input$retrospective_data_type, "count")
})

# --- Group-awareness helpers ------------------------------------------------
# These let almost all of the rendering code below stay untouched: when a
# result isn't grouped, group_scoped() and friends are no-ops.

retrospective_upload_group_col <- reactive({
  data <- retrospective$raw_data
  if (is.null(data) || !(retrospective_group_col_name %in% names(data))) {
    return(NULL)
  }
  if (dplyr::n_distinct(data[[retrospective_group_col_name]], na.rm = TRUE) == 0) {
    return(NULL)
  }
  retrospective_group_col_name
})

retrospective_upload_group_values <- reactive({
  group_col <- retrospective_upload_group_col()
  req(group_col)
  data <- retrospective$raw_data
  sort(unique(as.character(data[[group_col]][!is.na(data[[group_col]])])))
})

# Group column actually present on the *completed run's* forecasts (kept
# separate from the upload-time check above since a result can outlive
# changes to the upload).
retrospective_result_group_col <- reactive({
  result <- retrospective$result
  if (is.null(result) || is.null(result$group_col)) {
    return(NULL)
  }
  if (!result$group_col %in% names(result$forecasts)) {
    return(NULL)
  }
  result$group_col
})

retrospective_result_groups <- reactive({
  group_col <- retrospective_result_group_col()
  if (is.null(group_col)) {
    return(character())
  }
  result <- retrospective$result
  # A group that fails completely (run_retrospective_forecasts_single()
  # errored for it -- see retrospective_group_failure_result()) contributes
  # zero rows to $forecasts, so reading group values from $forecasts alone
  # would silently drop it from the Group selector and hide its failure
  # message from the UI. $failures always has at least one row for a group
  # like that, so union both.
  forecast_groups <- if (group_col %in% names(result$forecasts)) {
    as.character(result$forecasts[[group_col]])
  } else {
    character()
  }
  failure_groups <- if (group_col %in% names(result$failures)) {
    as.character(result$failures[[group_col]])
  } else {
    character()
  }
  sort(unique(c(forecast_groups, failure_groups)))
})

selected_retrospective_group <- reactive({
  groups <- retrospective_result_groups()
  if (length(groups) == 0) {
    return(NULL)
  }
  if (!is.null(input$retrospective_selected_group) && input$retrospective_selected_group %in% groups) {
    input$retrospective_selected_group
  } else {
    groups[[1]]
  }
})

# Filter a combined (group-tagged) tibble down to one group -- the
# currently selected one by default, or an explicit `group_value` (used to
# loop over every group, e.g. the global "Run Ensemble" handler below).
# Returns `tbl` unchanged when the result isn't grouped.
group_scoped <- function(tbl, group_value = selected_retrospective_group()) {
  group_col <- retrospective_result_group_col()
  if (is.null(group_col) || is.null(tbl) || !group_col %in% names(tbl)) {
    return(tbl)
  }
  if (is.null(group_value)) {
    return(tbl)
  }
  tbl |> dplyr::filter(.data[[group_col]] == group_value)
}

# Like group_scoped(), but also drops the (now-constant) group column --
# for tables/plots that shouldn't show a redundant single-valued column.
group_scoped_drop <- function(tbl) {
  scoped <- group_scoped(tbl)
  group_col <- retrospective_result_group_col()
  if (is.null(scoped) || is.null(group_col) || !group_col %in% names(scoped)) {
    return(scoped)
  }
  scoped |> dplyr::select(-dplyr::all_of(group_col))
}

# The uploaded raw data, filtered to one group (the selected one by default,
# or an explicit `group_value`) and with the group column dropped -- i.e.
# exactly what that group's slice looked like when it was forecast. Needed
# to rescore/re-ensemble a group in isolation.
group_scoped_raw_data <- function(group_value = selected_retrospective_group()) {
  data <- retrospective$raw_data
  group_col <- retrospective_upload_group_col()
  if (is.null(group_col) || is.null(data) || !group_col %in% names(data)) {
    return(data)
  }
  if (is.null(group_value)) {
    return(data)
  }
  data |>
    dplyr::filter(as.character(.data[[group_col]]) == group_value) |>
    dplyr::select(-dplyr::all_of(group_col))
}

# Disambiguating suffix for a group's per-group DOM ids below. Sanitizing
# group_value alone (the original approach) had no collision handling --
# "A B", "A-B", "A_B" and "A.B" all sanitize to the identical string, so two
# groups with differently-punctuated names would silently render their
# local-seasonality input/badge on top of the same element id, with one
# group's control invisibly reading or displaying another group's zone.
# `groups` is always the full, deterministically-ordered vector of every
# group value for the current upload (retrospective_upload_group_values()
# sort()s its distinct values), so a group's 1-based position in it is a
# stable, trivially-unique id suffix for the life of that upload -- no
# sanitizing or suffix-collision bookkeeping required. Falls back to the
# sanitized string only if `group_value` isn't found in `groups` at all
# (stale/mismatched call), so this never errors even then.
retrospective_group_dom_id_suffix <- function(group_value, groups) {
  idx <- match(as.character(group_value), as.character(groups))
  if (is.na(idx)) {
    return(gsub("[^A-Za-z0-9]+", "_", as.character(group_value)))
  }
  as.character(idx)
}

retrospective_group_country_input_id <- function(group_value, groups) {
  paste0("retrospective_group_country_", retrospective_group_dom_id_suffix(group_value, groups))
}

retrospective_group_zone_badge_output_id <- function(group_value, groups) {
  paste0("retrospective_group_zone_badge_", retrospective_group_dom_id_suffix(group_value, groups))
}

retrospective_group_data_type_input_id <- function(group_value, groups) {
  paste0("retrospective_group_data_type_", retrospective_group_dom_id_suffix(group_value, groups))
}

# Looks up the epi_zone for whatever's currently selected in a given group's
# country selectize, falling back to "E" (the same default the single-country
# selector effectively starts from via its "Paraguay" default) if nothing's
# selected yet or the lookup somehow misses.
retrospective_group_selected_zone <- function(group_value, groups) {
  country <- input[[retrospective_group_country_input_id(group_value, groups)]]
  if (is.null(country)) {
    return("E")
  }
  zone <- epizone_data$epi_zone[epizone_data$COUNTRY == country]
  if (length(zone) == 1 && !is.na(zone)) zone else "E"
}

# Falls back to "count" (the same default used everywhere else in the app)
# if this group's Data Type control hasn't rendered/been touched yet.
retrospective_group_selected_data_type <- function(group_value, groups) {
  value <- input[[retrospective_group_data_type_input_id(group_value, groups)]]
  if (is.null(value)) "count" else value
}

# Named list of group value -> chosen seasonality zone, derived from the
# per-group country dropdowns rendered by retrospective_group_seasonality_ui
# (same country -> zone lookup the single-location selector uses).
current_retrospective_group_seasonality <- reactive({
  group_col <- retrospective_upload_group_col()
  if (is.null(group_col)) {
    return(NULL)
  }
  groups <- retrospective_upload_group_values()
  values <- purrr::map_chr(groups, retrospective_group_selected_zone, groups = groups)
  stats::setNames(as.list(values), groups)
})

# Named list of group value -> chosen Data Type ("count"/"proportion"),
# derived from the per-group radio buttons rendered by
# retrospective_group_seasonality_ui below -- same shape/purpose as
# current_retrospective_group_seasonality() above, one setting earlier.
current_retrospective_group_data_type <- reactive({
  group_col <- retrospective_upload_group_col()
  if (is.null(group_col)) {
    return(NULL)
  }
  groups <- retrospective_upload_group_values()
  values <- purrr::map_chr(groups, retrospective_group_selected_data_type, groups = groups)
  stats::setNames(as.list(values), groups)
})

output$retrospective_group_seasonality_ui <- renderUI({
  group_col <- retrospective_upload_group_col()
  if (is.null(group_col)) {
    return(NULL)
  }
  groups <- retrospective_upload_group_values()
  req(length(groups) > 0)

  # Captured (and immediately cleared) here rather than read fresh per group
  # below: this whole tagList is built in one synchronous pass, so there's
  # no risk of process_retrospective_load() re-setting it mid-render, and
  # clearing it now means a later, unrelated data upload for a differently-
  # named set of groups can't accidentally reuse a stale zone that happens
  # to share a group name.
  pending_zones <- retrospective$pending_group_seasonality
  retrospective$pending_group_seasonality <- NULL
  pending_data_types <- retrospective$pending_group_data_type
  retrospective$pending_group_data_type <- NULL

  tagList(
    tags$p(
      class = "plot-helper-text",
      tagList(
        paste0(
          "Each value of '", group_col, "' is forecast independently, with its ",
          "own Local Seasonality and Data Type. Set its local seasonality by ",
          "searching for its country below:"
        ),
        modal_info_link("modal_seasonality")
      )
    ),
    lapply(groups, function(group_value) {
      country_input_id <- retrospective_group_country_input_id(group_value, groups)
      badge_output_id <- retrospective_group_zone_badge_output_id(group_value, groups)
      data_type_input_id <- retrospective_group_data_type_input_id(group_value, groups)

      # Registered here (rather than as a static output$... assignment)
      # because there's one badge per uploaded group value, and the set of
      # group values isn't known until data is uploaded.
      output[[badge_output_id]] <- renderUI({
        zone <- retrospective_group_selected_zone(group_value, groups)
        color <- zone_colors[zone]
        tags$span(
          style = paste0(
            "display:inline-block; background:", color,
            "; color:white; font-weight:600; padding:2px 10px;",
            " border-radius:12px; font-size:.8em; margin-bottom:6px;"
          ),
          paste0("Zone ", zone)
        )
      })

      # A loaded run's persisted zone (see load_retrospective_run()) can't
      # generally be traced back to the one specific country that was
      # originally selected -- several countries can share a zone -- so this
      # picks any country whose zone matches, which is all that matters for
      # forecasting/incremental-run purposes (only the resolved zone is ever
      # used downstream, never the country name itself).
      initial_country <- if (!is.null(pending_zones) && group_value %in% names(pending_zones)) {
        zone_for_group <- pending_zones[[group_value]]
        matching_country <- epizone_data$COUNTRY[epizone_data$epi_zone == zone_for_group]
        if (length(matching_country) > 0) matching_country[[1]] else "Paraguay"
      } else {
        "Paraguay"
      }

      initial_data_type <- if (!is.null(pending_data_types) && group_value %in% names(pending_data_types)) {
        pending_data_types[[group_value]]
      } else {
        "count"
      }

      tagList(
        selectizeInput(
          country_input_id,
          label = paste0("Country — ", group_value),
          choices = epizone_choices,
          selected = initial_country,
          width = "100%",
          options = list(
            placeholder = "Type to search countries...",
            maxOptions = length(epizone_choices)
          )
        ),
        uiOutput(badge_output_id),
        radioButtons(
          data_type_input_id,
          label = tagList(
            paste0("Data Type — ", group_value),
            modal_info_link("modal_data_type")
          ),
          choices = c("Counts" = "count", "Proportion (0-1)" = "proportion"),
          selected = initial_data_type
        )
      )
    })
  )
})

observeEvent(retrospective_upload_group_col(), {
  if (is.null(retrospective_upload_group_col())) {
    shinyjs::show(id = "retrospective_single_seasonality_block")
    shinyjs::show(id = "retrospective_single_data_type_block")
  } else {
    shinyjs::hide(id = "retrospective_single_seasonality_block")
    shinyjs::hide(id = "retrospective_single_data_type_block")
  }
}, ignoreNULL = FALSE)

# Re-validate the currently-loaded dataset whenever any group's Data Type
# changes -- validate_data() at upload time can only apply one lenient,
# universal check (see process_retrospective_upload()) since different
# groups may end up assigned different types; this is the per-group
# counterpart that actually enforces each group's own [0,1]/non-negative
# bound once it's known, mirroring the live tab's re-validation-on-change
# observer in server/data_upload.R.
observeEvent(current_retrospective_group_data_type(), {
  group_types <- current_retrospective_group_data_type()
  group_col <- retrospective_upload_group_col()
  req(group_types, group_col, retrospective$raw_data)

  data <- retrospective$raw_data
  bad_groups <- character()

  for (group_value in names(group_types)) {
    rows <- data[as.character(data[[group_col]]) == group_value, , drop = FALSE]
    vals <- suppressWarnings(as.numeric(rows$value))
    out_of_range <- if (identical(group_types[[group_value]], "proportion")) {
      any(!is.na(vals) & (vals < 0 | vals > 1))
    } else {
      any(!is.na(vals) & vals < 0)
    }
    if (isTRUE(out_of_range)) {
      bad_groups <- c(bad_groups, group_value)
    }
  }

  if (length(bad_groups) > 0) {
    retrospective$valid_data <- FALSE
    retrospective$upload_errors <- paste0(
      "Group \"", bad_groups, "\" has values outside the range allowed for its ",
      "selected Data Type. Fix the data or change that group's Data Type before running."
    )
    disable("run_retrospective")
  } else {
    retrospective$valid_data <- TRUE
    retrospective$upload_errors <- NULL
    enable("run_retrospective")
  }
}, ignoreInit = TRUE)

# A quick per-group health read (all succeeded / some failures / the whole
# group's run failed) so trouble spots are visible right in the selector,
# without having to switch to every group and check its own summary.
retrospective_group_health_label <- function(result, group_col, group_value) {
  failures <- result$failures
  successes <- result$successes
  n_failures <- if (!is.null(failures) && group_col %in% names(failures)) {
    nrow(failures[as.character(failures[[group_col]]) == group_value, , drop = FALSE])
  } else {
    0L
  }
  n_successes <- if (!is.null(successes) && group_col %in% names(successes)) {
    nrow(successes[as.character(successes[[group_col]]) == group_value, , drop = FALSE])
  } else {
    0L
  }

  if (n_successes == 0 && n_failures > 0) {
    paste0("⛔ ", group_value, " (run failed)")
  } else if (n_failures > 0) {
    paste0("⚠️ ", group_value, " (", n_failures, " failure", if (n_failures != 1) "s" else "", ")")
  } else {
    paste0("✅ ", group_value)
  }
}

output$retrospective_group_select_ui <- renderUI({
  groups <- retrospective_result_groups()
  if (length(groups) < 1) {
    return(NULL)
  }
  result <- retrospective$result
  group_col <- retrospective_result_group_col()

  choice_labels <- vapply(groups, function(g) retrospective_group_health_label(result, group_col, g), character(1))

  selectInput(
    "retrospective_selected_group",
    label = paste0("Group (", group_col, ")"),
    choices = stats::setNames(groups, choice_labels),
    selected = selected_retrospective_group(),
    width = "100%"
  )
})

retrospective_empty_configs <- function() {
  tibble::tibble(
    run_id = character(),
    model_id = character(),
    model_label = character(),
    run_label = character(),
    params = list()
  )
}

retrospective_has_population <- reactive({
  !is.null(retrospective$raw_data) && "population" %in% names(retrospective$raw_data)
})

retrospective_default_config <- function(model_id) {
  settings <- retrospective_default_settings(
    has_population = retrospective_has_population()
  )

  retrospective_build_run_configs(
    models = model_id,
    settings = settings,
    has_population = retrospective_has_population(),
    labels_use_base_model = FALSE
  )
}

make_unique_retrospective_label <- function(label, existing_labels) {
  label <- trimws(label)
  if (!nzchar(label)) {
    label <- "Retrospective run"
  }
  if (!label %in% existing_labels) {
    return(label)
  }

  suffix <- 2L
  repeat {
    candidate <- paste0(label, " #", suffix)
    if (!candidate %in% existing_labels) {
      return(candidate)
    }
    suffix <- suffix + 1L
  }
}

sync_retrospective_configs_to_models <- function(selected_models) {
  selected_models <- retrospective_null_coalesce(selected_models, character())
  configs <- retrospective_null_coalesce(retrospective$configs, retrospective_empty_configs())
  if (!all(c("run_id", "model_id", "model_label", "run_label", "params") %in% names(configs))) {
    configs <- retrospective_empty_configs()
  }
  configs <- configs |> dplyr::filter(model_id %in% selected_models)

  missing_models <- setdiff(selected_models, configs$model_id)
  if (length(missing_models) > 0) {
    new_configs <- purrr::map_dfr(missing_models, retrospective_default_config)
    configs <- dplyr::bind_rows(configs, new_configs)
  }

  retrospective$configs <- configs
}

reset_retrospective_configs_to_defaults <- function(selected_models) {
  selected_models <- retrospective_null_coalesce(selected_models, character())
  retrospective$configs <- if (length(selected_models) == 0) {
    retrospective_empty_configs()
  } else {
    purrr::map_dfr(selected_models, retrospective_default_config)
  }
}

clear_retrospective_result <- function() {
  retrospective$result <- NULL
  retrospective$result_stale <- FALSE
  retrospective$time_period_stale <- FALSE
  retrospective$stale_modal_shown <- FALSE
  retrospective$ensemble_members <- character()
  retrospective$scoring_reference_choice <- character()
  disable("download_retrospective_zip")
}

#' Flag that the currently-shown retrospective result no longer matches the
#' live setup. `time_period_changed` distinguishes *why*: TRUE for a change
#' to the reference date range, horizon, or seasonality -- something that
#' changes every model's training window, so the next "Run Retrospective"
#' click must be a full fresh run. FALSE (the default) for a model
#' configuration being added, removed, or reset -- the time period itself is
#' unchanged, so the next click can instead add/drop just those models from
#' the existing result in place (see add_retrospective_run_configs()).
mark_retrospective_result_stale <- function(time_period_changed = FALSE) {
  if (is.null(retrospective$result) || isTRUE(retrospective$suppress_stale_marking)) {
    return(invisible(FALSE))
  }

  retrospective$result_stale <- TRUE
  if (isTRUE(time_period_changed)) {
    retrospective$time_period_stale <- TRUE
  }

  if (!isTRUE(retrospective$stale_modal_shown)) {
    retrospective$stale_modal_shown <- TRUE
    body <- if (isTRUE(time_period_changed)) {
      tags$p(
        "Download the retrospective ZIP before starting a new run if you want to keep those results. Running again will start a fresh run over the new time period and replace the current tables and plots."
      )
    } else {
      tags$p(
        "Running again will add the newly configured model(s) to these results (and drop any you removed) without starting over -- the current tables and plots will update in place rather than being replaced."
      )
    }
    showModal(modalDialog(
      title = "Current retrospective results are still visible",
      tags$p(
        "This setup change does not match the completed retrospective results currently shown."
      ),
      body,
      footer = modalButton("OK"),
      easyClose = TRUE
    ))
  }

  invisible(TRUE)
}

retrospective_param_input_id <- function(model_id, param_name) {
  paste("retrospective_param", model_id, param_name, sep = "_")
}

observeEvent(input$retrospective_country_select, {
  req(input$retrospective_country_select)
  zone <- epizone_data$epi_zone[epizone_data$COUNTRY == input$retrospective_country_select]
  if (length(zone) == 1 && !is.na(zone)) {
    updateRadioButtons(session, "retrospective_seasonality", selected = zone)
  }
}, ignoreInit = FALSE)

output$retrospective_zone_badge_ui <- renderUI({
  zone <- input$retrospective_seasonality
  req(zone)

  color <- zone_colors[zone]

  tags$div(
    style = "margin-bottom: 6px;",
    tags$span(
      style = paste0(
        "display:inline-block; background:", color,
        "; color:white; font-weight:600; padding:3px 12px;",
        " border-radius:12px; font-size:.85em;"
      ),
      paste0("Zone ", zone)
    )
  )
})

process_retrospective_upload <- function(upload_info) {
  # A file with a `retrospective_group` column can assign different Data
  # Types to different groups (set per-group after upload, once the group
  # values are known -- see retrospective_group_seasonality_ui and the
  # current_retrospective_group_data_type() re-validation observer further
  # down). Upload-time validation can't yet know each group's type, so it
  # applies only the lenient, universal "count" check (non-negative values)
  # in that case -- "count" mode's check is a strict subset of "proportion"
  # mode's (every valid proportion row is also non-negative), so this can
  # never let through a row that would fail every possible per-group
  # assignment. A file with no group column is unambiguous and keeps the
  # exact original behavior: validated against the single selected type.
  header <- tryCatch(
    names(readr::read_csv(upload_info$datapath, n_max = 0, show_col_types = FALSE)),
    error = function(e) character()
  )
  has_group_col <- retrospective_group_col_name %in% header

  # Default the Data Type control(s) to the scale of the uploaded values, the
  # same way the live Data tab does. With a group column each group gets its
  # own detected type, seeded through pending_group_data_type -- the same
  # channel "Load Previous Run" uses to restore saved per-group types.
  peek <- tryCatch(
    readr::read_csv(upload_info$datapath, show_col_types = FALSE),
    error = function(e) NULL
  )

  if (!is.null(peek) && "value" %in% names(peek)) {
    if (has_group_col) {
      retrospective$pending_group_data_type <- detect_data_type_by_group(
        peek$value, peek[[retrospective_group_col_name]]
      )
    } else {
      detected_type <- detect_data_type(peek$value)
      if (!identical(detected_type, isolate(retrospective_data_type()))) {
        updateRadioButtons(session, "retrospective_data_type", selected = detected_type)
      }
    }
  }

  # A grouped file still validates leniently as "count" at upload time, since
  # different groups may end up with different types; the per-group observer
  # enforces each group's own bound once known.
  upload_validation_data_type <- if (has_group_col) {
    "count"
  } else if (exists("detected_type")) {
    detected_type
  } else {
    retrospective_data_type()
  }

  validation_results <- tryCatch(
    validate_data(upload_info$datapath, data_type = upload_validation_data_type),
    error = function(e) list(error = paste("Error:", e$message))
  )

  if (length(validation_results) > 0) {
    retrospective$upload_errors <- unlist(validation_results, recursive = TRUE)
    return(invisible(NULL))
  }

  data <- read_raw_data(upload_info$datapath)
  reference_dates <- available_retrospective_reference_dates(data)

  if (length(reference_dates) == 0) {
    retrospective$upload_errors <- "The dataset must contain at least two observed weeks."
    return(invisible(NULL))
  }

  retrospective$raw_data <- NULL
  retrospective$valid_data <- FALSE
  retrospective$upload_errors <- NULL
  retrospective$upload_name <- NULL
  retrospective$result <- NULL
  retrospective$result_stale <- FALSE
  retrospective$time_period_stale <- FALSE
  retrospective$stale_modal_shown <- FALSE
  retrospective$configs <- retrospective_empty_configs()
  retrospective$ensemble_members <- character()
  retrospective$scoring_reference_choice <- character()
  retrospective$run_name <- NULL
  disable("run_retrospective")
  disable("download_retrospective_zip")
  updateSelectInput(session, "retrospective_start_week", choices = NULL)
  updateSelectInput(session, "retrospective_end_week", choices = NULL)
  # A brand-new dataset is a brand-new run -- don't let a name typed for
  # whatever was uploaded/run before linger and mislabel this one.
  updateTextInput(session, "retrospective_run_name", value = "")

  retrospective$raw_data <- data
  retrospective$valid_data <- TRUE
  retrospective$upload_name <- upload_info$name
  sync_retrospective_configs_to_models(input$retrospective_models)

  # Mirror the main Data tab's country_select behavior (server/data_upload.R):
  # guess the seasonality country from the uploaded FILENAME (e.g.
  # "argentina_data.csv" -> "Argentina") rather than leaving it on whatever
  # was previously selected. Only meaningful for an ungrouped upload -- a
  # grouped file's country is set per-group instead (retrospective_group_
  # seasonality_ui), and one filename can't sensibly guess for every group.
  if (!has_group_col) {
    upload_country <- country_from_upload_filename(
      filename = upload_info$name,
      epizone_data = epizone_data,
      default = "Paraguay"
    )
    updateSelectizeInput(
      session,
      "retrospective_country_select",
      choices = epizone_choices,
      selected = upload_country
    )
  }

  date_choices <- setNames(
    format(reference_dates, "%Y-%m-%d"),
    format(reference_dates, "%Y-%m-%d")
  )

  updateSelectInput(
    session,
    "retrospective_start_week",
    choices = date_choices,
    selected = date_choices[[1]]
  )
  updateSelectInput(
    session,
    "retrospective_end_week",
    choices = date_choices,
    selected = date_choices[[length(date_choices)]]
  )
  enable("run_retrospective")
}

# --- Load Previous Run -------------------------------------------------
# Lets a folder from a previously downloaded (and extracted) retrospective
# ZIP -- or, running locally, one of this app's own output/retrospective/
# <run_stamp>/ folders directly -- be read back in and continued: viewed
# exactly as it was, and extended with more models without losing anything
# already run. See load_retrospective_run()/add_retrospective_run_configs()
# in R/retrospective.R for the engine half of this.

# Somewhere to point the folder browser by default; created defensively
# since it may not exist yet on a totally fresh checkout with no runs yet.
dir.create(file.path("output", "retrospective"), recursive = TRUE, showWarnings = FALSE)
retrospective_load_run_roots <- c(
  `Retrospective Output` = normalizePath(file.path("output", "retrospective"), mustWork = FALSE),
  Home = normalizePath("~", mustWork = FALSE)
)
shinyFiles::shinyDirChoose(
  input,
  "retrospective_load_run_dir",
  roots = retrospective_load_run_roots,
  session = session
)

# Neighbor graph upload (retrospective) =======================================

retrospective_target_groups <- reactive({
  req(retrospective$raw_data)
  unique(as.character(retrospective$raw_data$target_group))
})

observeEvent(input$retrospective_neighbor_graph_file, {
  req(input$retrospective_neighbor_graph_file)

  if (is.null(retrospective$raw_data)) {
    retrospective$neighbor_graph_validation <- list(
      errors = list(no_data = "Upload your retrospective data before the neighbor graph, so group names can be checked against it."),
      warnings = list(),
      data = NULL
    )
    return(invisible(NULL))
  }

  result <- validate_neighbor_graph(
    file = input$retrospective_neighbor_graph_file$datapath,
    target_groups = retrospective_target_groups()
  )

  retrospective$neighbor_graph_validation <- result

  if (!is.null(result$data)) {
    retrospective$neighbor_graph <- result$data
    retrospective$neighbor_graph_name <- input$retrospective_neighbor_graph_file$name
  } else {
    retrospective$neighbor_graph <- NULL
    retrospective$neighbor_graph_name <- NULL
  }

  mark_retrospective_result_stale()
})

output$retrospective_neighbor_graph_status_ui <- renderUI({
  result <- retrospective$neighbor_graph_validation
  if (is.null(result)) {
    return(NULL)
  }

  if (length(result$errors) > 0) {
    return(div(
      class = "alert alert-danger",
      icon("circle-exclamation", style = "margin-right:2px"),
      tags$strong(paste0(length(result$errors), " error(s) \u2014 neighbor graph was not added")),
      tags$ul(lapply(unlist(result$errors, recursive = TRUE), tags$li))
    ))
  }

  tagList(
    div(
      class = "alert alert-success",
      icon("circle-check", style = "margin-right:2px"),
      paste0(
        "Neighbor graph loaded",
        if (!is.null(retrospective$neighbor_graph_name)) paste0(" (", retrospective$neighbor_graph_name, ")") else "",
        ": ", nrow(retrospective$neighbor_graph), " neighbor pair(s). ",
        "Add an INFLAenza configuration with Group structure = \"besagproper\" ",
        "under Advanced Setup to use it."
      )
    ),
    if (length(result$warnings) > 0) {
      div(
        class = "alert alert-warning",
        icon("triangle-exclamation", style = "margin-right:2px"),
        tags$ul(lapply(unlist(result$warnings, recursive = TRUE), tags$li))
      )
    }
  )
})

observeEvent(input$retrospective_season_groups_file, {
  req(input$retrospective_season_groups_file)

  if (is.null(retrospective$raw_data)) {
    retrospective$season_groups_validation <- list(
      errors = list(no_data = "Upload your retrospective data before the seasonal groups, so group names can be checked against it."),
      warnings = list(),
      data = NULL
    )
    return(invisible(NULL))
  }

  result <- validate_season_groups(
    file = input$retrospective_season_groups_file$datapath,
    target_groups = retrospective_target_groups()
  )

  retrospective$season_groups_validation <- result

  if (!is.null(result$data)) {
    retrospective$season_groups <- result$data
    retrospective$season_groups_name <- input$retrospective_season_groups_file$name
  } else {
    retrospective$season_groups <- NULL
    retrospective$season_groups_name <- NULL
  }

  mark_retrospective_result_stale()
})

output$retrospective_season_groups_status_ui <- renderUI({
  result <- retrospective$season_groups_validation
  if (is.null(result)) return(NULL)

  if (length(result$errors) > 0) {
    return(div(
      class = "alert alert-danger",
      icon("circle-exclamation", style = "margin-right:2px"),
      tags$strong(paste0(length(result$errors), " error(s) \u2014 seasonal groups were not added")),
      tags$ul(lapply(unlist(result$errors, recursive = TRUE), tags$li))
    ))
  }

  tagList(
    div(
      class = "alert alert-success",
      icon("circle-check", style = "margin-right:2px"),
      paste0(
        "Seasonal groups loaded",
        if (!is.null(retrospective$season_groups_name)) paste0(" (", retrospective$season_groups_name, ")") else "",
        ": ", nrow(retrospective$season_groups), " target group(s) assigned across ",
        length(unique(retrospective$season_groups$season_group)), " named group(s). ",
        "Add an INFLAenza configuration with Seasonality = \"season_group\" ",
        "under Advanced Setup to use it."
      )
    ),
    if (length(result$warnings) > 0) {
      div(
        class = "alert alert-warning",
        icon("triangle-exclamation", style = "margin-right:2px"),
        tags$ul(lapply(unlist(result$warnings, recursive = TRUE), tags$li))
      )
    }
  )
})

process_retrospective_load <- function(source_dir) {
  retrospective$upload_errors <- NULL

  loaded <- tryCatch(
    load_retrospective_run(source_dir),
    error = function(e) e
  )
  if (inherits(loaded, "error")) {
    retrospective$upload_errors <- conditionMessage(loaded)
    return(invisible(NULL))
  }

  # Validate against the data type THIS RUN WAS SAVED WITH (see
  # retrospective_load_validation_data_type() in R/retrospective.R), never
  # whatever input$retrospective_data_type currently happens to show -- that
  # radio button reflects leftover UI state from before "Load Previous Run"
  # was clicked and has nothing to do with the file being reloaded here.
  # Using it caused validate_data()'s data-type range check (check3b) to
  # reject perfectly valid saved data whenever the two didn't happen to
  # match, aborting the load before retrospective$raw_data was ever set --
  # which looks exactly like "the loaded run has no data to run new models
  # against."
  validation_results <- tryCatch(
    validate_data(loaded$source_data_path, data_type = retrospective_load_validation_data_type(loaded)),
    error = function(e) list(error = paste("Error:", e$message))
  )
  if (length(validation_results) > 0) {
    retrospective$upload_errors <- c(
      "The data saved with this run failed validation -- it may have been edited outside the app:",
      unlist(validation_results, recursive = TRUE)
    )
    return(invisible(NULL))
  }

  data <- read_raw_data(loaded$source_data_path)
  reference_dates <- available_retrospective_reference_dates(data)
  if (length(reference_dates) == 0) {
    retrospective$upload_errors <- "The data saved with this run must contain at least two observed weeks."
    return(invisible(NULL))
  }

  # Suppressed for the duration of this load (and briefly after, via the
  # shinyjs::delay() below) -- see the field's own comment in the
  # `retrospective` reactiveValues block up top for why this is needed.
  retrospective$suppress_stale_marking <- TRUE

  # If ANY of the setup below throws, the guard above must still come back
  # down -- otherwise it stays stuck TRUE for the rest of the session,
  # silently disabling mark_retrospective_result_stale() (and therefore
  # fresh-vs-incremental run detection) app-wide, with no visible symptom
  # beyond "the stale-results warning stopped showing up." Wrapping this in
  # tryCatch means a failure here clears the guard immediately and reports
  # as an ordinary load error instead.
  load_setup_result <- tryCatch({
    retrospective$raw_data <- data
    retrospective$valid_data <- TRUE
    retrospective$run_name <- retrospective_null_coalesce(loaded$run_name, "")
    retrospective$upload_name <- if (nzchar(retrospective$run_name)) {
      paste0(retrospective$run_name, " (", basename(source_dir), ")")
    } else {
      paste0("Loaded run: ", basename(source_dir))
    }
    retrospective$ensemble_members <- retrospective_null_coalesce(loaded$result$ensemble_models, character())
    # Restored so a loaded run can be re-run or extended with spatial configs
    # without the user having to find and re-upload the same graph file.
    retrospective$season_groups <- loaded$season_groups
    retrospective$season_groups_name <- if (!is.null(loaded$season_groups)) {
      paste0("saved with run (", nrow(loaded$season_groups), " assignments)")
    } else {
      NULL
    }
    retrospective$season_groups_validation <- if (!is.null(loaded$season_groups)) {
      list(errors = list(), warnings = list(), data = loaded$season_groups)
    } else {
      NULL
    }
    retrospective$neighbor_graph <- loaded$neighbor_graph
    retrospective$neighbor_graph_name <- if (!is.null(loaded$neighbor_graph)) {
      paste0("saved with run (", nrow(loaded$neighbor_graph), " pairs)")
    } else {
      NULL
    }
    retrospective$neighbor_graph_validation <- if (!is.null(loaded$neighbor_graph)) {
      list(errors = list(), warnings = list(), data = loaded$neighbor_graph)
    } else {
      NULL
    }

    date_choices <- setNames(format(reference_dates, "%Y-%m-%d"), format(reference_dates, "%Y-%m-%d"))
    updateSelectInput(session, "retrospective_start_week", choices = date_choices, selected = date_choices[[1]])
    updateSelectInput(session, "retrospective_end_week", choices = date_choices, selected = date_choices[[length(date_choices)]])
    updateNumericInput(session, "retrospective_horizon", value = loaded$horizon)
    updateRadioButtons(session, "retrospective_data_type", selected = retrospective_null_coalesce(loaded$data_type, "count"))
    # Restore the name into the input box too, so it carries forward if the
    # user keeps adding models to this same run (an incremental run leaves
    # retrospective$run_name untouched -- see the input$run_retrospective
    # handler -- so this is really only cosmetic, but it means the box
    # doesn't show blank right after a load that DID have a name).
    updateTextInput(session, "retrospective_run_name", value = retrospective$run_name)

    if (!is.null(loaded$group_col)) {
      # retrospective_group_seasonality_ui (re)renders once retrospective_
      # upload_group_col() picks up the group column set above, and consumes
      # this to seed each group's initial country selection -- see that
      # renderUI for why it's done this way rather than via
      # updateSelectizeInput() on inputs that don't exist yet.
      retrospective$pending_group_seasonality <- loaded$group_seasonality
      retrospective$pending_group_data_type <- loaded$group_data_type
    } else {
      zone <- retrospective_null_coalesce(loaded$seasonality, "E")
      updateRadioButtons(session, "retrospective_seasonality", selected = zone)
      country_for_zone <- epizone_data$COUNTRY[epizone_data$epi_zone == zone]
      if (length(country_for_zone) > 0) {
        updateSelectizeInput(session, "retrospective_country_select", selected = country_for_zone[[1]])
      }
    }

    # Set BEFORE syncing the "Models" checkboxes below: sync_retrospective_
    # configs_to_models() only ever filters/adds to whatever's already in
    # retrospective$configs, so setting the real loaded configs first means
    # that sync is a no-op pass-through rather than something to race against.
    retrospective$configs <- loaded$result$run_configs
    retrospective$result <- loaded$result
    retrospective$result_stale <- FALSE
    retrospective$time_period_stale <- FALSE
    retrospective$stale_modal_shown <- FALSE
    retrospective$scoring_reference_choice <- retrospective_initial_scoring_reference(loaded$result)

    updateCheckboxGroupInput(
      session,
      "retrospective_models",
      selected = unique(retrospective$configs$model_id)
    )

    # Local app, not a hosted multi-user server -- a few hundred ms is ample
    # for the update*Input() calls above to round-trip through the browser
    # and for every observer they wake (config-sync, start/end/horizon/
    # seasonality staleness) to run, after which it's safe to stop
    # suppressing.
    shinyjs::delay(500, {
      retrospective$suppress_stale_marking <- FALSE
    })

    TRUE
  }, error = function(e) e)

  if (inherits(load_setup_result, "error")) {
    retrospective$suppress_stale_marking <- FALSE
    retrospective$upload_errors <- paste("Error loading this run:", conditionMessage(load_setup_result))
    return(invisible(NULL))
  }

  enable("run_retrospective")
  if (!is.null(retrospective$result$zip_path) && file.exists(retrospective$result$zip_path)) {
    enable("download_retrospective_zip")
  }
}

observeEvent(input$retrospective_load_run_dir, {
  chosen <- shinyFiles::parseDirPath(retrospective_load_run_roots, input$retrospective_load_run_dir)
  req(length(chosen) == 1, nzchar(chosen))

  if (!is.null(retrospective$result)) {
    retrospective$pending_load_dir <- chosen
    showModal(modalDialog(
      title = "Load a previous retrospective run?",
      tags$p(
        "Loading a previous run will replace the completed retrospective results currently shown."
      ),
      tags$p(
        "Download the retrospective ZIP first if you want to keep those results."
      ),
      footer = tagList(
        actionButton("cancel_retrospective_load_run", "Keep current results"),
        actionButton("confirm_retrospective_load_run", "Load previous run", class = "btn-danger")
      ),
      easyClose = FALSE
    ))
    return(invisible(NULL))
  }

  process_retrospective_load(chosen)
})

observeEvent(input$cancel_retrospective_load_run, {
  retrospective$pending_load_dir <- NULL
  removeModal()
}, ignoreInit = TRUE)

observeEvent(input$confirm_retrospective_load_run, {
  req(retrospective$pending_load_dir)
  chosen <- retrospective$pending_load_dir
  retrospective$pending_load_dir <- NULL
  removeModal()
  process_retrospective_load(chosen)
}, ignoreInit = TRUE)

# Download the example multi-country retrospective_group template
output$download_retrospective_template <- downloadHandler(
  filename = function() {
    paste0("microhub-template-retrospective-groups_", Sys.Date(), ".csv")
  },
  content = function(file) {
    file.copy(file.path("data", "microhub-template-retrospective-groups.csv"), file)
  }
)

observeEvent(input$retrospective_file, {
  req(input$retrospective_file)

  upload_info <- input$retrospective_file
  if (!is.null(retrospective$result)) {
    retrospective$pending_upload <- upload_info
    showModal(modalDialog(
      title = "Load new retrospective data?",
      tags$p(
        "Uploading a new data file will clear the completed retrospective results currently shown."
      ),
      tags$p(
        "Download the retrospective ZIP first if you want to keep those results."
      ),
      footer = tagList(
        actionButton(
          "cancel_retrospective_file_upload",
          "Keep current results"
        ),
        actionButton(
          "confirm_retrospective_file_upload",
          "Load new file",
          class = "btn-danger"
        )
      ),
      easyClose = FALSE
    ))
    return(invisible(NULL))
  }

  process_retrospective_upload(upload_info)
})

observeEvent(input$cancel_retrospective_file_upload, {
  retrospective$pending_upload <- NULL
  removeModal()
}, ignoreInit = TRUE)

observeEvent(input$confirm_retrospective_file_upload, {
  req(retrospective$pending_upload)
  upload_info <- retrospective$pending_upload
  retrospective$pending_upload <- NULL
  removeModal()
  process_retrospective_upload(upload_info)
}, ignoreInit = TRUE)

output$retrospective_upload_status_ui <- renderUI({
  if (!is.null(retrospective$upload_errors)) {
    return(div(
      class = "alert alert-danger",
      style = "padding:8px 12px; margin-bottom:8px;",
      tags$strong("Validation errors:"),
      tags$ul(lapply(retrospective$upload_errors, tags$li))
    ))
  }

  if (isTRUE(retrospective$valid_data)) {
    return(div(
      class = "alert alert-success",
      style = "padding:8px 12px; margin-bottom:8px;",
      icon("circle-check", style = "margin-right:2px"),
      "Loaded ",
      tags$strong(retrospective$upload_name)
    ))
  }

  NULL
})

output$retrospective_data_preview <- renderDT({
  req(retrospective$raw_data)
  datatable(
    retrospective$raw_data |> arrange(desc(date)),
    rownames = FALSE,
    filter = "top",
    selection = "none"
  )
})

observeEvent(input$select_all_retrospective_models, {
  updateCheckboxGroupInput(
    session,
    "retrospective_models",
    selected = retrospective_model_choices
  )
})

observeEvent(input$clear_retrospective_models, {
  updateCheckboxGroupInput(
    session,
    "retrospective_models",
    selected = character(0)
  )
})

observeEvent(input$retrospective_models, {
  sync_retrospective_configs_to_models(input$retrospective_models)
  mark_retrospective_result_stale()
}, ignoreInit = FALSE)

output$retrospective_parameter_inputs_ui <- renderUI({
  req(input$retrospective_config_model)

  specs <- retrospective_parameter_specs(
    has_population = retrospective_has_population()
  )[[input$retrospective_config_model]]

  if (length(specs) == 0) {
    return(tags$p(
      class = "plot-helper-text",
      "This model does not expose additional retrospective parameters."
    ))
  }

  # Widget construction itself lives in retrospective_parameter_input_widget()
  # (R/retrospective.R) so it can be unit-tested against every spec outside a
  # live Shiny session -- see that function's own comment for why a spec
  # omitting an optional bound (e.g. max_matches has no `max`) used to crash
  # this render with "argument is of length zero".
  tagList(lapply(names(specs), function(param_name) {
    spec <- specs[[param_name]]
    input_id <- retrospective_param_input_id(input$retrospective_config_model, param_name)
    retrospective_parameter_input_widget(input_id, spec)
  }))
})

current_retrospective_config_params <- reactive({
  req(input$retrospective_config_model)
  specs <- retrospective_parameter_specs(
    has_population = retrospective_has_population()
  )[[input$retrospective_config_model]]

  params <- purrr::imap(specs, function(spec, param_name) {
    input[[retrospective_param_input_id(input$retrospective_config_model, param_name)]]
  })

  retrospective_validate_model_params(
    model_id = input$retrospective_config_model,
    params = params,
    has_population = retrospective_has_population()
  )
})

observeEvent(input$add_retrospective_config, {
  req(input$retrospective_config_model)

  params <- tryCatch(
    current_retrospective_config_params(),
    error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    }
  )
  if (is.null(params)) {
    return(invisible(NULL))
  }

  current_configs <- retrospective_null_coalesce(
    retrospective$configs,
    retrospective_empty_configs()
  )
  existing_labels <- current_configs$run_label
  auto_label <- retrospective_make_run_label(
    input$retrospective_config_model,
    params
  )
  requested_label <- input$retrospective_config_label
  run_label <- make_unique_retrospective_label(
    if (nzchar(trimws(requested_label))) requested_label else auto_label,
    existing_labels
  )
  run_id <- retrospective_make_run_id(
    input$retrospective_config_model,
    params,
    paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", input$add_retrospective_config)
  )

  new_config <- tibble::tibble(
    run_id = run_id,
    model_id = input$retrospective_config_model,
    model_label = retrospective_model_label(input$retrospective_config_model),
    run_label = run_label,
    params = list(params)
  )

  retrospective$configs <- dplyr::bind_rows(current_configs, new_config)
  mark_retrospective_result_stale()
  updateCheckboxGroupInput(
    session,
    "retrospective_models",
    selected = union(input$retrospective_models, input$retrospective_config_model)
  )
  updateTextInput(session, "retrospective_config_label", value = "")
})

observeEvent(input$reset_retrospective_configs, {
  reset_retrospective_configs_to_defaults(input$retrospective_models)
  mark_retrospective_result_stale()
})

output$retrospective_remove_config_ui <- renderUI({
  configs <- retrospective$configs
  if (is.null(configs) || nrow(configs) == 0) {
    return(NULL)
  }

  tagList(
    selectInput(
      "retrospective_config_to_remove",
      "Remove Configuration",
      choices = stats::setNames(configs$run_id, configs$run_label),
      selected = configs$run_id[[nrow(configs)]]
    ),
    actionButton(
      "remove_retrospective_config",
      "Remove Selected",
      class = "btn-sm btn-outline-danger"
    )
  )
})

observeEvent(input$remove_retrospective_config, {
  req(input$retrospective_config_to_remove)
  configs <- retrospective_null_coalesce(
    retrospective$configs,
    retrospective_empty_configs()
  )

  retrospective$configs <- configs |>
    dplyr::filter(run_id != input$retrospective_config_to_remove)
  mark_retrospective_result_stale()
})

observeEvent(
  list(
    input$retrospective_start_week,
    input$retrospective_end_week,
    input$retrospective_horizon,
    input$retrospective_seasonality,
    current_retrospective_group_seasonality()
  ),
  {
    # These specifically change every model's training window, so the next
    # run cannot be applied incrementally -- see mark_retrospective_result_
    # stale()'s time_period_changed argument.
    mark_retrospective_result_stale(time_period_changed = TRUE)
  },
  ignoreInit = TRUE
)

output$retrospective_config_table <- renderDT({
  configs <- retrospective$configs
  if (is.null(configs) || nrow(configs) == 0) {
    return(datatable(
      tibble::tibble(`Run label` = character(), Model = character(), Parameters = character()),
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t")
    ))
  }

  config_table <- configs |>
    dplyr::mutate(
      Parameters = vapply(params, retrospective_params_label, character(1))
    ) |>
    dplyr::transmute(
      `Run label` = run_label,
      Model = model_label,
      Parameters = Parameters
    )

  datatable(
    config_table,
    rownames = FALSE,
    selection = "none",
    options = list(dom = "t", pageLength = 8, scrollX = TRUE)
  )
})

output$retrospective_run_size_ui <- renderUI({
  req(retrospective$raw_data)
  n_dates <- length(selected_retrospective_reference_dates())
  n_configs <- if (is.null(retrospective$configs)) 0L else nrow(retrospective$configs)
  # retrospective_upload_group_values() req()s on there being a group column,
  # which would otherwise silently abort this whole renderUI for the common
  # (non-grouped) upload -- guard it the same way every other call site does.
  n_groups <- if (is.null(retrospective_upload_group_col())) {
    1L
  } else {
    length(retrospective_upload_group_values())
  }
  n_runs <- n_dates * n_configs * n_groups

  if (n_runs == 0) {
    return(NULL)
  }

  alert_class <- if (n_runs >= 100) {
    "alert alert-warning"
  } else {
    "alert alert-info"
  }

  group_span <- if (n_groups > 1) {
    paste0(" x ", n_groups, " group(s)")
  } else {
    ""
  }

  div(
    class = alert_class,
    style = "padding:8px 12px; margin:10px 0;",
    tags$strong("Planned runs: "),
    paste0(n_runs, " model-date fit(s)"),
    tags$span(
      style = "display:block; font-size:.875em;",
      paste0(n_dates, " reference week(s) x ", n_configs, " model configuration(s)", group_span)
    )
  )
})

selected_retrospective_reference_dates <- reactive({
  req(retrospective$raw_data)
  req(input$retrospective_start_week, input$retrospective_end_week)

  retrospective_reference_range(
    retrospective$raw_data,
    input$retrospective_start_week,
    input$retrospective_end_week
  )
})

observe({
  can_run <- isTRUE(retrospective$valid_data) &&
    length(selected_retrospective_reference_dates()) > 0 &&
    !is.null(retrospective$configs) &&
    nrow(retrospective$configs) > 0

  toggleState("run_retrospective", condition = can_run)
})

observeEvent(input$run_retrospective, {
  req(retrospective$raw_data, isTRUE(retrospective$valid_data))
  req(retrospective$configs, nrow(retrospective$configs) > 0)

  reference_dates <- selected_retrospective_reference_dates()
  req(length(reference_dates) > 0)

  # A result already exists for exactly this time period (reference dates,
  # horizon, seasonality all unchanged -- see time_period_stale) -> this
  # click adds/drops model configuration(s) to/from that SAME result and
  # SAME output_dir rather than starting a brand-new run from scratch. Only
  # when there's no existing result yet, or the time period itself changed,
  # does this fall back to today's full-fresh-run behavior below. This is
  # what "start a session" means for the Retrospective tab: the on-disk
  # folder/zip a user already has keeps accumulating models across repeated
  # "Run Retrospective" clicks instead of each click discarding it.
  incremental <- !is.null(retrospective$result) && !isTRUE(retrospective$time_period_stale)

  disable("run_retrospective")
  disable("download_retrospective_zip")
  retrospective$result_stale <- FALSE
  retrospective$time_period_stale <- FALSE
  retrospective$stale_modal_shown <- FALSE
  on.exit(enable("run_retrospective"), add = TRUE)

  progress_message <- if (incremental) {
    "Adding model(s) to the current retrospective results"
  } else {
    "Running retrospective forecasts"
  }

  withProgress(message = progress_message, value = 0, {
    setProgress(value = 0, detail = "Preparing run...")
    last_progress_bucket <- -1L

    # reference_date_index/total_reference_dates describe progress through
    # ONE group's own date loop; group_index/total_groups (NULL when the
    # run isn't grouped) place that within the overall multi-group run so
    # the bar climbs monotonically across the whole run instead of
    # completing and restarting once per group.
    progress_callback <- function(reference_date_index, total_reference_dates, reference_date,
                                  group = NULL, group_index = NULL, total_groups = NULL) {
      within_group_fraction <- reference_date_index / total_reference_dates
      overall_fraction <- if (!is.null(group_index) && !is.null(total_groups) && total_groups > 0) {
        ((group_index - 1) + within_group_fraction) / total_groups
      } else {
        within_group_fraction
      }
      progress_bucket <- floor(overall_fraction * 100 / 10) * 10

      if (progress_bucket > last_progress_bucket) {
        group_detail <- if (!is.null(group) && !is.null(total_groups)) {
          paste0("group ", group, " (", group_index, " of ", total_groups, "), ")
        } else {
          ""
        }
        setProgress(
          value = overall_fraction,
          detail = paste0(
            progress_bucket,
            "% complete; ",
            group_detail,
            "running reference date ",
            format(reference_date, "%Y-%m-%d"),
            " (",
            reference_date_index,
            " of ",
            total_reference_dates,
            ")"
          )
        )
        last_progress_bucket <<- progress_bucket
      }
    }

    result <- if (incremental) {
      add_retrospective_run_configs(
        existing_result = retrospective$result,
        data = retrospective$raw_data,
        reference_dates = reference_dates,
        horizon = input$retrospective_horizon,
        seasonality = input$retrospective_seasonality,
        quantiles_needed = rv$quantiles_needed,
        run_configs = retrospective$configs,
        neighbor_graph = retrospective$neighbor_graph,
        season_groups = retrospective$season_groups,
        group_col = retrospective_group_col_name,
        group_seasonality = current_retrospective_group_seasonality(),
        group_data_type = current_retrospective_group_data_type(),
        progress_callback = progress_callback,
        data_type = retrospective_data_type()
      )
    } else {
      run_stamp <- retrospective_run_folder_stamp(input$retrospective_run_name)
      output_dir <- file.path("output", "retrospective", run_stamp)

      run_retrospective_forecasts(
        data = retrospective$raw_data,
        reference_dates = reference_dates,
        horizon = input$retrospective_horizon,
        seasonality = input$retrospective_seasonality,
        quantiles_needed = rv$quantiles_needed,
        output_dir = output_dir,
        run_configs = retrospective$configs,
        neighbor_graph = retrospective$neighbor_graph,
        season_groups = retrospective$season_groups,
        ensemble_models = NULL,
        auto_ensemble = FALSE,
        group_col = retrospective_group_col_name,
        group_seasonality = current_retrospective_group_seasonality(),
        group_data_type = current_retrospective_group_data_type(),
        progress_callback = progress_callback,
        data_type = retrospective_data_type(),
        run_name = input$retrospective_run_name
      )
    }

    setProgress(value = 1, detail = "100% complete")
    retrospective$result <- result
    retrospective$result_stale <- FALSE
    retrospective$time_period_stale <- FALSE
    retrospective$stale_modal_shown <- FALSE

    if (incremental) {
      # add_retrospective_run_configs() already carried the user's true
      # global scoring-reference choice forward correctly -- it re-resolves
      # each group starting from THAT GROUP's OWN previously-resolved value,
      # not from the global choice. That means retrospective_initial_
      # scoring_reference(result) (which just reads back whichever value
      # group 1 happens to have) is NOT a safe stand-in for "the global
      # choice" here the way it is right after a fresh run: if group 1 had
      # already fallen back to a different model than the user's real pick
      # (e.g. because the user's chosen baseline wasn't fit for that one
      # group), reseeding from it would silently overwrite the user's actual
      # global choice with group 1's fallback -- and the "Except: ..."
      # caveat built from the mismatch would then name the wrong group(s).
      # Leaving retrospective$scoring_reference_choice untouched keeps the
      # user's real choice (or the very first fresh-run default, if they
      # never touched the selector) intact across incremental runs.
    } else {
      # A fresh run has no prior global choice to preserve -- every group
      # was just resolved from scratch against the same (absent) requested
      # reference, so seeding from the actual result here is exactly what
      # "no explicit choice yet" should show.
      retrospective$scoring_reference_choice <- retrospective_initial_scoring_reference(result)
      # Same reasoning as the run_stamp/run_name passed into
      # run_retrospective_forecasts() above: a fresh run's name is whatever
      # was typed in the box just now. An incremental run keeps whatever
      # retrospective$run_name already was -- it's still the same run. Kept
      # as the user's own original text here (not the folder-safe slug) --
      # this copy is only ever displayed, never used as a path.
      retrospective$run_name <- trimws(retrospective_null_coalesce(input$retrospective_run_name, ""))
      # Likewise, any previously-selected ensemble members belonged to the
      # PRIOR result (different run_configs, possibly different models
      # available) and are not carried forward by run_retrospective_
      # forecasts() the way add_retrospective_run_configs() carries them
      # forward for an incremental run -- reset them so the Summary card
      # can't show stale members alongside a "no ensemble built yet" state.
      retrospective$ensemble_members <- character()
    }
  })

  if (!is.null(retrospective$result$zip_path) &&
      file.exists(retrospective$result$zip_path)) {
    enable("download_retrospective_zip")
  }
})

# Pull out the scoring reference relevant to one group -- the currently
# selected one by default, or an explicit `group_value` (used to loop over
# every group). `result$scoring_reference` is a plain single value for an
# ungrouped run, or a named vector (group value -> resolved reference
# model) for a grouped one -- see combine_retrospective_group_results().
current_group_scoring_reference <- function(result, group_value = selected_retrospective_group()) {
  if (is.null(result)) {
    return(NULL)
  }
  if (is.null(group_value) || is.null(names(result$scoring_reference))) {
    return(result$scoring_reference)
  }
  retrospective_null_coalesce(result$scoring_reference[[group_value]], NULL)
}

# What to seed retrospective$scoring_reference_choice with right after a
# run/load completes -- the value that was actually resolved (for the first
# group, if grouped; every group starts out resolving to the same requested
# reference model by construction, differing only if a specific group's own
# fallback kicked in). Pure function of `result`; does not read
# retrospective$scoring_reference_choice itself.
retrospective_initial_scoring_reference <- function(result) {
  if (is.null(result)) {
    return(character())
  }
  ref <- result$scoring_reference
  if (!is.null(names(ref)) && length(ref) > 0) {
    return(unname(ref[[1]]))
  }
  retrospective_null_coalesce(ref, character())
}

# The single global Scoring Reference (baseline model) choice, applied to
# every group at once -- mirrors retrospective$ensemble_members. Returns
# character() if nothing has ever been resolved yet (no result at all);
# callers display "Regular Baseline" as the fallback label in that case.
global_scoring_reference_choice <- function() {
  retrospective_null_coalesce(retrospective$scoring_reference_choice, character())
}

# Groups whose ACTUAL resolved scoring reference differs from the single
# global choice above -- the chosen baseline didn't complete there, so that
# group fell back to something else. Shared by the Retrospective Summary
# card's "Except: ..." note and the pooled Overall Score Summary card's own
# caveat (see output$retrospective_overall_pooled_card_ui below), both of
# which need to warn the user about the same underlying situation: a
# per-group Relative WIS and the pooled table's Relative WIS for the same
# model are only guaranteed comparable when every group actually scored
# against the same reference model.
retrospective_scoring_reference_exceptions <- function(result, scoring_reference_choice) {
  is_grouped <- !is.null(retrospective_result_group_col())
  if (is_grouped && !is.null(result) && !is.null(names(result$scoring_reference))) {
    resolved <- result$scoring_reference
    resolved[!is.na(resolved) & resolved != scoring_reference_choice]
  } else {
    character()
  }
}

# Every model that completed successfully in AT LEAST ONE group (union, not
# intersection) -- deliberately NOT group_scoped(), since the ensemble
# member picker draws from every group at once (see
# output$retrospective_ensemble_controls_ui and the global
# input$run_retrospective_ensemble handler below). A group that didn't
# happen to complete a given member just skips it when the ensemble is
# rebuilt for that group.
completed_retrospective_model_labels_across_groups <- reactive({
  result <- retrospective$result
  successes <- result$successes
  if (is.null(result) || is.null(successes) || nrow(successes) == 0) {
    return(character())
  }

  successes |>
    dplyr::filter(model != "Ensemble") |>
    dplyr::distinct(model) |>
    dplyr::arrange(model) |>
    dplyr::pull(model)
})

output$retrospective_ensemble_controls_ui <- renderUI({
  result <- retrospective$result
  if (is.null(result)) {
    return(NULL)
  }
  # Both controls below are global settings applied to every retrospective
  # group at once (see global_scoring_reference_choice() and
  # retrospective$ensemble_members), so they're computed from every group's
  # forecasts combined -- never group_scoped() to just the one currently
  # being viewed.
  all_forecasts <- result$forecasts

  completed_labels <- completed_retrospective_model_labels_across_groups()
  non_baseline_labels <- completed_labels[!is_retrospective_baseline_model(completed_labels)]
  available_score_references <- retrospective_available_reference_models(all_forecasts)
  scoring_reference <- retrospective_resolve_reference_model(
    forecasts = all_forecasts,
    run_configs = result$run_configs,
    requested_reference_model = global_scoring_reference_choice()
  )
  scoring_control <- if (length(available_score_references) > 0) {
    tagList(
      selectInput(
        "retrospective_scoring_reference",
        "Baseline Model (Scoring Reference)",
        choices = available_score_references,
        selected = scoring_reference,
        width = "100%"
      ),
      helpText(
        "Applies to every group at once -- relative WIS everywhere is measured against this model. ",
        "A group where it didn't complete falls back automatically (see the summary above for exceptions)."
      )
    )
  } else {
    NULL
  }

  if (length(completed_labels) < 2) {
    return(tagList(
      tags$hr(style = "margin:10px 0;"),
      scoring_control,
      div(
        class = "alert alert-info",
        style = "padding:8px 12px; margin-top:10px;",
        tags$strong("Ensemble unavailable. "),
        "At least two model configurations must complete successfully."
      )
    ))
  }

  # Default to non-baseline models, same as the live Ensemble tab -- but keep
  # every completed model (baselines included) selectable, so a user can
  # deliberately add a baseline to the ensemble. The choices span every
  # group (any model that completed anywhere), and membership is now a
  # single global selection -- "Run Ensemble" applies it to every group at
  # once, so the ensemble always means the same set of models regardless of
  # which group is currently being viewed.
  current_members <- retrospective_null_coalesce(retrospective$ensemble_members, character())
  selected <- intersect(current_members, completed_labels)
  if (length(selected) == 0) {
    selected <- if (length(non_baseline_labels) > 0) non_baseline_labels else completed_labels
  }
  selected_method <- retrospective_null_coalesce(retrospective$result$ensemble_method, "median")

  tagList(
    tags$hr(style = "margin:10px 0;"),
    scoring_control,
    radioButtons(
      "retrospective_ensemble_method",
      "Ensemble Method",
      choices = c(
        "Median (per quantile)"         = "median",
        "Mean (per quantile)"           = "mean",
        "Linear pool (distributional)"  = "linear_pool"
      ),
      selected = selected_method,
      inline = TRUE
    ),
    selectizeInput(
      "retrospective_ensemble_models",
      "Ensemble Members",
      choices = completed_labels,
      selected = selected,
      multiple = TRUE,
      width = "100%"
    ),
    actionButton(
      "run_retrospective_ensemble",
      "Run Ensemble"
    ),
    helpText(
      "Rebuilds the ensemble for every group at once from these members -- ",
      "a group where one of them didn't complete just leaves it out of ",
      "that group's ensemble."
    )
  )
})

# Apply a single global Scoring Reference (baseline model) choice to EVERY
# group at once -- mirrors the global "Run Ensemble" handler below, so "the
# baseline" always means the same model no matter which group is being
# viewed. A group missing the requested model still resolves via
# retrospective_resolve_reference_model()'s normal fallback chain (its own
# resolved value can then legitimately differ -- see the caveat shown in the
# all-groups summary), rather than failing outright.
observeEvent(input$retrospective_scoring_reference, {
  req(retrospective$result, retrospective$raw_data)
  result <- retrospective$result

  # Skip when this is just the control re-rendering with its already-current
  # value (e.g. after an unrelated result update), not an actual user pick.
  if (identical(global_scoring_reference_choice(), input$retrospective_scoring_reference)) {
    return(invisible(NULL))
  }

  group_col <- retrospective_result_group_col()
  group_values <- if (is.null(group_col)) list(NULL) else as.list(retrospective_result_groups())

  for (group_value in group_values) {
    group_forecasts <- group_scoped(result$forecasts, group_value)

    group_scoring_reference <- retrospective_resolve_reference_model(
      forecasts = group_forecasts,
      run_configs = result$run_configs,
      requested_reference_model = input$retrospective_scoring_reference
    )
    group_scores <- score_retrospective_forecasts(
      group_forecasts,
      group_scoped_raw_data(group_value),
      reference_model = group_scoring_reference
    )

    result$scores$rows <- retrospective_replace_group_rows(result$scores$rows, group_col, group_value, group_scores$rows)
    result$scores$overall <- retrospective_replace_group_rows(result$scores$overall, group_col, group_value, group_scores$overall)
    result$scores$by_target_group <- retrospective_replace_group_rows(
      result$scores$by_target_group, group_col, group_value, group_scores$by_target_group
    )
    result$scores$by_forecast_date <- retrospective_replace_group_rows(
      result$scores$by_forecast_date, group_col, group_value, group_scores$by_forecast_date
    )

    if (!is.null(group_col) && !is.null(group_value)) {
      updated_reference <- retrospective_null_coalesce(result$scoring_reference, character())
      updated_reference[[group_value]] <- group_scoring_reference
      result$scoring_reference <- updated_reference
    } else {
      result$scoring_reference <- group_scoring_reference
    }
  }

  retrospective$scoring_reference_choice <- input$retrospective_scoring_reference
  retrospective$result <- rewrite_retrospective_output_files(result)
  enable("download_retrospective_zip")
}, ignoreInit = TRUE)

# Whether the currently globally-selected ensemble members actually
# co-occur (>= 2 of them completed) within at least one single group's
# results. The button used to enable purely on the GLOBAL member count
# (>= 2 selected anywhere across all groups), which lets a user pick two
# models that each only completed in DIFFERENT groups -- every group's
# rebuild would then silently produce zero ensemble rows (build_
# retrospective_ensemble() requires 2+ members present in that group's own
# forecasts), while ensemble_members/ensemble_models/ensemble_method are
# still recorded as though the ensemble succeeded, with nothing in the UI
# to say otherwise. Ungrouped runs have no group_col to split by, so the
# global count is already the right check there.
retrospective_ensemble_members_co_occur_in_any_group <- function(members, result) {
  members <- retrospective_null_coalesce(members, character())
  if (length(members) < 2 || is.null(result) || is.null(result$successes) || nrow(result$successes) == 0) {
    return(FALSE)
  }

  successes <- result$successes |> dplyr::filter(model %in% members)
  group_col <- result$group_col

  if (is.null(group_col) || !group_col %in% names(successes)) {
    return(dplyr::n_distinct(successes$model) >= 2)
  }

  per_group_counts <- successes |>
    dplyr::distinct(.data[[group_col]], model) |>
    dplyr::count(.data[[group_col]], name = "n_members")

  nrow(per_group_counts) > 0 && any(per_group_counts$n_members >= 2)
}

observe({
  toggleState(
    "run_retrospective_ensemble",
    condition = retrospective_ensemble_members_co_occur_in_any_group(
      input$retrospective_ensemble_models,
      retrospective$result
    )
  )
})

# Rebuild the ensemble for EVERY group at once, from the same chosen member
# labels + method -- so "the ensemble" always means the same set of models
# no matter which group is being viewed, rather than each group carrying
# whatever was last built while it happened to be the selected one. A group
# missing 2+ of the chosen members simply ends up with no Ensemble rows for
# this rebuild (build_retrospective_ensemble() returns NULL in that case)
# instead of being left with a stale Ensemble built from a different member
# set. Rescoring uses the global Scoring Reference choice (also applied to
# every group -- see global_scoring_reference_choice()), so both global
# settings stay consistent with each other after either one is touched.
observeEvent(input$run_retrospective_ensemble, {
  req(retrospective$result, retrospective$raw_data)
  req(length(input$retrospective_ensemble_models) >= 2)
  # Defense in depth: the "Run Ensemble" button is only ever enabled when
  # retrospective_ensemble_members_co_occur_in_any_group() is TRUE (see the
  # toggleState() observer above), but req() here guards against acting on
  # a stale click if the result changed out from under the button between
  # render and click.
  req(retrospective_ensemble_members_co_occur_in_any_group(input$retrospective_ensemble_models, retrospective$result))

  result <- retrospective$result
  group_col <- retrospective_result_group_col()
  group_values <- if (is.null(group_col)) list(NULL) else as.list(retrospective_result_groups())
  group_produced_ensemble <- logical(0)

  for (group_value in group_values) {
    group_forecasts <- group_scoped(result$forecasts, group_value)
    individual_forecasts <- group_forecasts |>
      dplyr::filter(model != "Ensemble")

    ensemble_result <- build_retrospective_ensemble(
      individual_forecasts,
      ensemble_members = input$retrospective_ensemble_models,
      method = input$retrospective_ensemble_method
    )
    has_ensemble <- !is.null(ensemble_result) && nrow(ensemble_result) > 0
    group_produced_ensemble <- c(group_produced_ensemble, has_ensemble)

    updated_group_forecasts <- if (has_ensemble) {
      dplyr::bind_rows(individual_forecasts, ensemble_result)
    } else {
      individual_forecasts
    }
    result$forecasts <- retrospective_replace_group_rows(result$forecasts, group_col, group_value, updated_group_forecasts)

    ensemble_successes <- if (has_ensemble) {
      ensemble_result |>
        dplyr::summarize(rows = dplyr::n(), .by = reference_date) |>
        dplyr::mutate(
          run_id = "ensemble",
          model_id = "ensemble",
          model = "Ensemble",
          .after = reference_date
        ) |>
        dplyr::select(reference_date, run_id, model_id, model, rows)
    } else {
      NULL
    }

    updated_group_successes <- group_scoped(result$successes, group_value) |>
      dplyr::filter(model != "Ensemble") |>
      dplyr::bind_rows(ensemble_successes) |>
      dplyr::arrange(reference_date, model)
    result$successes <- retrospective_replace_group_rows(result$successes, group_col, group_value, updated_group_successes)

    group_scoring_reference <- retrospective_resolve_reference_model(
      forecasts = updated_group_forecasts,
      run_configs = result$run_configs,
      requested_reference_model = global_scoring_reference_choice()
    )
    group_scores <- score_retrospective_forecasts(
      updated_group_forecasts,
      group_scoped_raw_data(group_value),
      reference_model = group_scoring_reference
    )
    result$scores$rows <- retrospective_replace_group_rows(result$scores$rows, group_col, group_value, group_scores$rows)
    result$scores$overall <- retrospective_replace_group_rows(result$scores$overall, group_col, group_value, group_scores$overall)
    result$scores$by_target_group <- retrospective_replace_group_rows(
      result$scores$by_target_group, group_col, group_value, group_scores$by_target_group
    )
    result$scores$by_forecast_date <- retrospective_replace_group_rows(
      result$scores$by_forecast_date, group_col, group_value, group_scores$by_forecast_date
    )

    if (!is.null(group_col) && !is.null(group_value)) {
      updated_reference <- retrospective_null_coalesce(result$scoring_reference, character())
      updated_reference[[group_value]] <- group_scoring_reference
      result$scoring_reference <- updated_reference
    } else {
      result$scoring_reference <- group_scoring_reference
    }
  }

  retrospective$ensemble_members <- input$retrospective_ensemble_models
  result$ensemble_models <- input$retrospective_ensemble_models
  result$ensemble_method <- input$retrospective_ensemble_method

  retrospective$result <- rewrite_retrospective_output_files(result)
  enable("download_retrospective_zip")

  # The toggleState()/req() checks above only guarantee the chosen members
  # co-occur in AT LEAST ONE group -- a group missing 2+ of them still ends
  # up with zero ensemble rows for this rebuild by design (see the comment
  # above this handler), which is expected for a single such group but
  # worth a heads-up if it happened to be every group in this run.
  if (length(group_produced_ensemble) > 0 && !any(group_produced_ensemble)) {
    showNotification(
      "The ensemble produced no rows for any group -- none of the selected members completed together in the same group.",
      type = "warning",
      duration = 8
    )
  } else if (length(group_produced_ensemble) > 1 && !all(group_produced_ensemble)) {
    showNotification(
      paste0(
        "The ensemble was rebuilt, but ", sum(!group_produced_ensemble), " of ",
        length(group_produced_ensemble), " group(s) had fewer than 2 of the selected members complete, ",
        "so those groups have no Ensemble rows."
      ),
      type = "warning",
      duration = 8
    )
  }
})

# The all-groups Retrospective Summary -- deliberately NEVER group_scoped():
# every field here covers every retrospective group at once (or the one
# implicit "group" for an ungrouped upload), so a user gets the full health
# picture of a multi-country run without clicking through each group one at
# a time. Per-group detail (which specific group has trouble, its own score
# tables) lives in the Individual Group section further down instead.
output$retrospective_run_summary_ui <- renderUI({
  result <- retrospective$result
  if (is.null(result)) {
    return(tags$p(
      class = "plot-helper-text",
      "Run retrospective forecasts to summarize model performance across past forecast dates."
    ))
  }

  group_col <- retrospective_result_group_col()
  is_grouped <- !is.null(group_col)
  all_groups <- if (is_grouped) retrospective_result_groups() else character()

  all_successes <- result$successes
  all_failures <- result$failures
  all_forecasts <- result$forecasts

  completed_dates <- bind_rows(
    all_successes |> select(reference_date),
    all_failures |> select(reference_date)
  ) |>
    distinct(reference_date) |>
    arrange(reference_date) |>
    pull(reference_date)

  forecast_date_label <- if (length(completed_dates) == 0) {
    "No forecast dates completed"
  } else if (length(completed_dates) == 1) {
    format(completed_dates, "%Y-%m-%d")
  } else {
    paste0(
      format(min(completed_dates), "%Y-%m-%d"),
      " to ",
      format(max(completed_dates), "%Y-%m-%d"),
      " (",
      length(completed_dates),
      " dates)"
    )
  }

  target_dates <- all_forecasts |>
    distinct(target_end_date) |>
    arrange(target_end_date) |>
    pull(target_end_date) |>
    as.Date()

  target_date_label <- if (length(target_dates) == 0) {
    "No target dates predicted"
  } else if (length(target_dates) == 1) {
    format(target_dates, "%Y-%m-%d")
  } else {
    paste0(
      format(min(target_dates), "%Y-%m-%d"),
      " to ",
      format(max(target_dates), "%Y-%m-%d")
    )
  }

  attempted_models <- bind_rows(
    all_successes |> select(model),
    all_failures |> select(model)
  ) |>
    distinct(model) |>
    arrange(model) |>
    pull(model)

  # Per-model success breakdown ACROSS GROUPS: for a grouped run, "succeeded"
  # is no longer a single flat list, since a model can succeed in some
  # groups and fail in others -- e.g. "INFLAenza: 3/4 groups (failed in
  # CountryB)" surfaces that without having to click into every group.
  model_breakdown_rows <- if (is_grouped) {
    lapply(attempted_models, function(m) {
      # `all_successes$model == m` is NA (not FALSE) for every row whenever
      # `m` is NA -- and indexing a vector with an all-NA logical mask
      # returns one NA per row rather than dropping them, so
      # unique(as.character(...)) collapsed that down to a single NA_
      # character_ "match" instead of zero. That silently produced a
      # self-contradictory line ("NA: succeeded in 1/N groups (failed in
      # all)": n_succeeded read 1 from the stray NA while missing_in still
      # listed every group). A model name should never actually be NA in
      # practice, but treat it explicitly as "never succeeded anywhere"
      # rather than let the comparison's NA-propagation produce a bogus
      # count. The `!is.na(all_successes$model)` guard covers the mirror
      # case of a non-NA `m` against a corrupted NA row in `all_successes`.
      succeeded_in <- if (is.na(m)) {
        character(0)
      } else {
        unique(as.character(all_successes[[group_col]][!is.na(all_successes$model) & all_successes$model == m]))
      }
      missing_in <- setdiff(all_groups, succeeded_in)
      list(model = m, n_succeeded = length(succeeded_in), n_total = length(all_groups), missing_in = sort(missing_in))
    })
  } else {
    list()
  }

  successful_models <- all_successes |>
    distinct(model) |>
    arrange(model) |>
    pull(model)

  failed_models <- all_failures |>
    distinct(model) |>
    arrange(model) |>
    pull(model)

  n_failures <- nrow(all_failures)
  status_class <- if (n_failures > 0) "alert alert-warning" else "alert alert-success"
  failure_items <- if (n_failures > 0) {
    all_failures |>
      arrange(reference_date, model) |>
      mutate(
        summary = paste0(
          if (is_grouped) paste0("[", .data[[group_col]], "] ") else "",
          format(as.Date(reference_date), "%Y-%m-%d"),
          " - ",
          model,
          ": ",
          message
        )
      ) |>
      pull(summary)
  } else {
    character()
  }
  shown_failure_items <- head(failure_items, 8)
  remaining_failures <- max(0, length(failure_items) - length(shown_failure_items))

  # Scoring reference is a single global choice (see
  # global_scoring_reference_choice()), but a specific group can still
  # legitimately resolve to something else if it never completed the
  # requested model -- surface that instead of hiding it.
  scoring_reference_choice <- retrospective_null_coalesce(global_scoring_reference_choice(), "Regular Baseline")
  scoring_reference_exceptions <- retrospective_scoring_reference_exceptions(result, scoring_reference_choice)

  stale_notice <- if (isTRUE(retrospective$result_stale)) {
    div(
      class = "alert alert-warning",
      style = "padding:8px 12px; margin-bottom:10px;",
      tags$strong("Setup changed after this run. "),
      if (isTRUE(retrospective$time_period_stale)) {
        "These results are still available for review and download, but they do not reflect the current retrospective setup. Running again will start a fresh run and replace them."
      } else {
        "These results are still available for review and download, but the model configuration has changed. Running again will add the newly configured model(s) to these results (and drop any you removed) without starting over."
      }
    )
  } else {
    NULL
  }

  tagList(
    stale_notice,
    div(
      class = status_class,
      style = "padding:10px 12px; margin-bottom:8px;",
      tags$strong("Retrospective run complete"),
      tags$dl(
        style = "display:grid; grid-template-columns:max-content 1fr; column-gap:12px; row-gap:4px; margin:8px 0 0 0;",
        if (nzchar(retrospective_null_coalesce(retrospective$run_name, ""))) tagList(
          tags$dt("Session name"),
          tags$dd(style = "margin:0;", tags$strong(retrospective$run_name))
        ),
        if (is_grouped) tagList(
          tags$dt("Retrospective groups"),
          tags$dd(style = "margin:0;", length(all_groups))
        ),
        tags$dt("Forecast dates run"),
        tags$dd(style = "margin:0;", forecast_date_label),
        tags$dt("Target dates predicted"),
        tags$dd(style = "margin:0;", target_date_label),
        tags$dt("Models run"),
        tags$dd(style = "margin:0;", if (length(attempted_models) > 0) paste(attempted_models, collapse = ", ") else "None"),
        if (!is_grouped) tagList(
          tags$dt("Succeeded"),
          tags$dd(style = "margin:0;", if (length(successful_models) > 0) paste(successful_models, collapse = ", ") else "None")
        ),
        tags$dt("Baseline model (Scoring Reference)"),
        tags$dd(
          style = "margin:0;",
          tagList(
            scoring_reference_choice,
            if (length(scoring_reference_exceptions) > 0) {
              tags$span(
                style = "display:block; font-size:.8em; color:#8a6d3b;",
                paste0(
                  "Except: ",
                  paste(
                    paste0(names(scoring_reference_exceptions), " (", scoring_reference_exceptions, ")"),
                    collapse = ", "
                  ),
                  " -- chosen baseline didn't complete there."
                )
              )
            }
          )
        ),
        tags$dt(if (is_grouped) "Ensemble members (all groups)" else "Ensemble members"),
        tags$dd(
          style = "margin:0;",
          {
            ensemble_members <- retrospective_null_coalesce(retrospective$ensemble_members, character())
            if (length(ensemble_members) > 0) paste(ensemble_members, collapse = ", ") else "No ensemble run"
          }
        ),
        tags$dt("Ensemble method"),
        tags$dd(
          style = "margin:0;",
          if (length(result$ensemble_models) > 0) ensemble_method_label(retrospective_null_coalesce(result$ensemble_method, "median")) else "—"
        ),
        tags$dt("Failures"),
        tags$dd(style = "margin:0;", if (n_failures > 0) paste0(n_failures, " failed model-date-group run(s)") else "None")
      ),
      if (length(model_breakdown_rows) > 0) {
        tagList(
          tags$hr(style = "margin:8px 0;"),
          tags$strong("Model completion across groups"),
          tags$ul(
            style = "margin:6px 0 0 0; padding-left:18px;",
            lapply(model_breakdown_rows, function(row) {
              tags$li(
                paste0(
                  row$model, ": succeeded in ", row$n_succeeded, "/", row$n_total, " groups",
                  if (length(row$missing_in) > 0) paste0(" (failed in ", paste(row$missing_in, collapse = ", "), ")") else ""
                )
              )
            })
          )
        )
      },
      if (length(failed_models) > 0) {
        tagList(
          tags$hr(style = "margin:8px 0;"),
          tags$strong("Failed forecasts"),
          tags$ul(
            style = "margin:6px 0 0 0; padding-left:18px;",
            lapply(shown_failure_items, tags$li)
          ),
          if (remaining_failures > 0) {
            tags$p(
              style = "margin:6px 0 0 0; font-size:.875em;",
              paste0("Plus ", remaining_failures, " additional failure(s).")
            )
          }
        )
      }
    )
  )
})

retrospective_plot_model_choices <- reactive({
  result <- retrospective$result
  group_forecasts <- group_scoped(result$forecasts)
  if (is.null(result) || is.null(group_forecasts) || nrow(group_forecasts) == 0) {
    return(character())
  }

  group_forecasts |>
    dplyr::filter(output_type == "quantile") |>
    dplyr::distinct(model) |>
    dplyr::arrange(model) |>
    dplyr::pull(model)
})

output$retrospective_plot_model_ui <- renderUI({
  choices <- retrospective_plot_model_choices()
  if (length(choices) < 2) {
    return(NULL)
  }

  selectInput(
    "retrospective_plot_model",
    "Forecast Plot Model",
    choices = choices,
    selected = if ("Ensemble" %in% choices) "Ensemble" else choices[[1]],
    width = "100%"
  )
})

output$retrospective_ensemble_forecast_plot <- renderPlot({
  req(retrospective$result, retrospective$raw_data)

  plot_retrospective_ensemble_forecasts(
    forecasts = group_scoped(retrospective$result$forecasts),
    actual_data = group_scoped_raw_data(),
    forecast_stride = 3L,
    selected_model = input$retrospective_plot_model
  )
})

output$retrospective_forecast_plot_message_ui <- renderUI({
  req(retrospective$result, retrospective$raw_data)

  plot_data <- retrospective_ensemble_plot_data(
    forecasts = group_scoped(retrospective$result$forecasts),
    actual_data = group_scoped_raw_data(),
    forecast_stride = 3L,
    selected_model = input$retrospective_plot_model
  )

  req(!is.na(plot_data$model))

  if (isTRUE(plot_data$is_ensemble)) {
    return(NULL)
  }

  div(
    class = "alert alert-info",
    style = "padding:8px 12px; margin-bottom:10px;",
    tags$strong("No ensemble forecast was generated. "),
    "Showing ",
    tags$strong(plot_data$model),
    " instead. Select at least two completed model configurations in the summary panel to generate an ensemble."
  )
})

output$retrospective_status_table <- renderDT({
  result <- retrospective$result
  req(result)

  successes <- group_scoped_drop(result$successes) |>
    mutate(status = "Complete", message = "") |>
    select(reference_date, model, status, rows, message)

  failures <- group_scoped_drop(result$failures) |>
    mutate(status = "Failed", rows = 0L) |>
    select(reference_date, model, status, rows, message)

  status <- bind_rows(successes, failures) |>
    arrange(reference_date, model)

  datatable(status, rownames = FALSE, filter = "top", selection = "none")
})

output$retrospective_files_table <- renderDT({
  result <- retrospective$result
  req(result)

  datatable(group_scoped(result$files), rownames = FALSE, filter = "top", selection = "none")
})

format_retrospective_score_summary <- function(score_tbl, data_type = "count") {
  # Raw WIS lives on the same scale as `value` itself -- a proportion-scale
  # (0-1) series produces WIS values one to two orders of magnitude smaller
  # than a count-scale series, so rounding to 2 decimals the way count mode
  # does would collapse most proportion-mode WIS values to "0.00". Relative
  # WIS/log WIS/coverage are all already scale-invariant ratios or
  # percentages and don't need this.
  wis_digits <- if (identical(data_type, "proportion")) 4 else 2
  score_tbl |>
    mutate(
      across(any_of(c("reference_date")), ~ format(as.Date(.x), "%Y-%m-%d")),
      mean_wis = round(mean_wis, wis_digits),
      mean_relative_wis = round(mean_relative_wis, 3),
      mean_log_wis = round(mean_log_wis, 3),
      mean_relative_log_wis = round(mean_relative_log_wis, 3),
      coverage_50 = round(100 * coverage_50, 1),
      coverage_95 = round(100 * coverage_95, 1)
    ) |>
    rename(
      `Forecast date` = any_of("reference_date"),
      `Target group` = any_of("target_group"),
      Model = model,
      WIS = mean_wis,
      `Relative WIS` = mean_relative_wis,
      `Log WIS` = mean_log_wis,
      `Relative log WIS` = mean_relative_log_wis,
      `50% coverage` = coverage_50,
      `95% coverage` = coverage_95,
      `Forecast targets` = n_forecast_targets
    )
}

# `highlight_best`: bold + tint the row(s) with the lowest relative WIS (or
# lowest raw WIS, if no reference model is available) so the best performer
# doesn't require scanning the whole table. When the table also breaks out
# by `target_group` or `reference_date` (the Target Groups / Forecast Dates
# tabs), "best" is computed WITHIN each of those subgroups rather than
# across the whole table -- those columns are detected automatically from
# `score_tbl`'s own shape, so this one function serves the Overall table,
# the pooled all-groups table, and both of the other two tabs.
retrospective_score_summary_table <- function(score_tbl, highlight_best = TRUE, data_type = "count") {
  subgroup_vars <- intersect(c("target_group", "reference_date"), names(score_tbl))
  best_col <- if (all(is.na(score_tbl$mean_relative_wis))) "mean_wis" else "mean_relative_wis"

  if (highlight_best) {
    scored <- if (length(subgroup_vars) > 0) {
      score_tbl |> dplyr::group_by(dplyr::across(dplyr::all_of(subgroup_vars)))
    } else {
      score_tbl
    }
    score_tbl <- scored |>
      dplyr::mutate(
        is_best = is.finite(.data[[best_col]]) &
          .data[[best_col]] == suppressWarnings(min(.data[[best_col]], na.rm = TRUE))
      ) |>
      dplyr::ungroup()
  } else {
    score_tbl$is_best <- FALSE
  }

  display_tbl <- format_retrospective_score_summary(score_tbl |> dplyr::select(-is_best), data_type = data_type)
  display_tbl$is_best <- score_tbl$is_best
  is_best_col_index <- which(names(display_tbl) == "is_best") - 1L

  dt <- datatable(
    display_tbl,
    rownames = FALSE,
    filter = "top",
    selection = "none",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      columnDefs = list(list(visible = FALSE, targets = is_best_col_index))
    )
  )

  if (highlight_best) {
    dt <- dt |>
      formatStyle(
        columns = "Model",
        valueColumns = "is_best",
        target = "row",
        fontWeight = styleEqual(c(TRUE, FALSE), c("bold", "normal")),
        backgroundColor = styleEqual(c(TRUE, FALSE), c("#e7f5ea", "white"))
      )
  }
  dt
}

output$retrospective_score_overall_table <- renderDT({
  req(retrospective$result)
  score_tbl <- group_scoped_drop(retrospective$result$scores$overall)
  req(nrow(score_tbl) > 0)

  retrospective_score_summary_table(score_tbl, data_type = retrospective_data_type())
})

# Wraps the whole "Overall Score Summary (All Groups)" card -- hidden
# entirely for an ungrouped (single-country) run OR a grouped upload that
# happens to resolve to exactly one distinct group value, where a table
# "pooled across every group" would just be an exact duplicate of the
# per-group Overall tab below it. Checking retrospective_result_group_col()
# alone (whether a group column exists at all) isn't enough for this --
# a group COLUMN can exist with only one distinct value in it.
output$retrospective_overall_pooled_card_ui <- renderUI({
  result <- retrospective$result
  req(result, retrospective_result_group_col())
  req(length(retrospective_result_groups()) > 1)

  scoring_reference_choice <- retrospective_null_coalesce(global_scoring_reference_choice(), "Regular Baseline")
  scoring_reference_exceptions <- retrospective_scoring_reference_exceptions(result, scoring_reference_choice)

  card(
    card_header("Overall Score Summary (All Groups)"),
    tags$p(
      class = "plot-helper-text",
      "Every model's score pooled across every retrospective group -- weighted by how many forecast targets each group contributed, not averaged group-by-group. See each group's own breakdown in Individual Group Detail below."
    ),
    if (length(scoring_reference_exceptions) > 0) {
      tags$p(
        class = "plot-helper-text",
        style = "color:#8a6d3b;",
        tags$strong("Note: "),
        paste0(
          "Relative WIS here uses '", scoring_reference_choice, "' as the reference for every group. ",
          length(scoring_reference_exceptions), " group(s) (",
          paste(names(scoring_reference_exceptions), collapse = ", "),
          ") actually scored against a different fallback baseline because '", scoring_reference_choice,
          "' didn't complete there -- Relative WIS for the same model can legitimately differ between this ",
          "pooled table and that group's own Overall tab below."
        )
      )
    },
    DTOutput("retrospective_score_overall_pooled_table")
  )
})

# Item 4: one row per model, pooled across every retrospective group -- see
# summarize_retrospective_scores_pooled_across_groups() for why this pools
# already-scored rows rather than re-running hubEvals across groups. Uses
# the SAME literal global Scoring Reference choice for every group's
# contribution here, which is NOT always the same reference a given group's
# OWN per-group Overall tab used to compute ITS relative WIS -- a group
# whose actual resolved reference fell back to something else (see
# retrospective_scoring_reference_exceptions() and the caveat rendered
# above when that happens) scored its own rows against that fallback, not
# this table's chosen reference. So this table's Relative WIS for a model
# is only guaranteed to line up with that model's Relative WIS in a
# specific group's Overall tab when that group is NOT one of the
# exceptions -- it is comparable ACROSS MODELS within this pooled table
# either way, since every model here is being compared to the exact same
# literal reference.
output$retrospective_score_overall_pooled_table <- renderDT({
  result <- retrospective$result
  req(result, retrospective_result_group_col())
  req(length(retrospective_result_groups()) > 1)
  reference_model <- retrospective_null_coalesce(global_scoring_reference_choice(), "Regular Baseline")
  score_tbl <- summarize_retrospective_scores_pooled_across_groups(result$scores$rows, reference_model)
  req(nrow(score_tbl) > 0)

  retrospective_score_summary_table(score_tbl, data_type = retrospective_data_type())
})

# Small caption for the Individual Group section: the Scoring Reference is
# a single global choice (see global_scoring_reference_choice()), but the
# group currently being viewed may have resolved to something else if it
# never completed the requested model -- shown here so it's clear which
# baseline THIS group's score tables below were actually measured against.
output$retrospective_group_scoring_reference_note_ui <- renderUI({
  result <- retrospective$result
  req(result)

  chosen <- retrospective_null_coalesce(global_scoring_reference_choice(), "Regular Baseline")
  resolved <- retrospective_null_coalesce(current_group_scoring_reference(result), chosen)

  tags$p(
    class = "plot-helper-text",
    style = "margin-bottom:4px;",
    if (identical(chosen, resolved)) {
      paste0("Scored against baseline: ", resolved)
    } else {
      paste0(
        "Scored against baseline: ", resolved,
        " (the global choice, ", chosen, ", didn't complete in this group)"
      )
    }
  )
})

output$retrospective_score_target_group_table <- renderDT({
  req(retrospective$result)
  score_tbl <- group_scoped_drop(retrospective$result$scores$by_target_group)
  req(nrow(score_tbl) > 0)

  retrospective_score_summary_table(score_tbl, data_type = retrospective_data_type())
})

output$retrospective_score_forecast_date_table <- renderDT({
  req(retrospective$result)
  score_tbl <- group_scoped_drop(retrospective$result$scores$by_forecast_date)
  req(nrow(score_tbl) > 0)

  retrospective_score_summary_table(score_tbl, data_type = retrospective_data_type())
})

output$retrospective_score_target_group_plot <- renderPlot({
  req(retrospective$result)
  score_tbl <- group_scoped_drop(retrospective$result$scores$by_target_group)
  req(nrow(score_tbl) > 0)

  plot_retrospective_target_group_scores(
    score_tbl,
    reference_model = current_group_scoring_reference(retrospective$result),
    data_type = retrospective_data_type()
  )
})

output$download_retrospective_score_target_group_plot <- downloadHandler(
  filename = function() {
    "retrospective-target-group-scores.png"
  },
  content = function(file) {
    req(retrospective$result)
    score_tbl <- group_scoped_drop(retrospective$result$scores$by_target_group)
    req(nrow(score_tbl) > 0)

    ggplot2::ggsave(
      filename = file,
      plot = plot_retrospective_target_group_scores(
        score_tbl,
        reference_model = current_group_scoring_reference(retrospective$result),
        data_type = retrospective_data_type()
      ),
      width = 12,
      height = 7,
      dpi = 300
    )
  }
)

output$retrospective_score_forecast_date_plot <- renderPlot({
  req(retrospective$result)
  score_tbl <- group_scoped_drop(retrospective$result$scores$by_forecast_date)
  req(nrow(score_tbl) > 0)

  plot_retrospective_forecast_date_scores(
    score_tbl,
    reference_model = current_group_scoring_reference(retrospective$result)
  )
})

output$download_retrospective_score_forecast_date_plot <- downloadHandler(
  filename = function() {
    "retrospective-forecast-date-scores.png"
  },
  content = function(file) {
    req(retrospective$result)
    score_tbl <- group_scoped_drop(retrospective$result$scores$by_forecast_date)
    req(nrow(score_tbl) > 0)

    ggplot2::ggsave(
      filename = file,
      plot = plot_retrospective_forecast_date_scores(
        score_tbl,
        reference_model = current_group_scoring_reference(retrospective$result)
      ),
      width = 12,
      height = 7,
      dpi = 300
    )
  }
)

output$download_retrospective_zip <- downloadHandler(
  filename = function() {
    result <- retrospective$result
    if (is.null(result)) {
      return("retrospective.zip")
    }

    paste0(basename(result$output_dir), ".zip")
  },
  content = function(file) {
    req(retrospective$result$zip_path)
    file.copy(retrospective$result$zip_path, file, overwrite = TRUE)
  }
)
