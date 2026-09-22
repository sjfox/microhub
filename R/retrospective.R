# Retrospective forecasting helpers ==========================================

# Standard models first, then the development ones, so the group divider the
# checkbox pickers draw (model_choices_with_divider(), R/ui_helpers.R) has a
# single clean break to sit on. The divider is positioned from
# retrospective_development_model_choices below, not from a hardcoded index, so
# this order and that set have to stay consistent -- keep development entries
# contiguous at the end.
retrospective_model_choices <- c(
  "Regular Baseline" = "baseline_regular",
  "Seasonal Baseline" = "baseline_seasonal",
  "Opt Baseline" = "baseline_opt",
  "INFLAenza" = "inla",
  "Copycat" = "copycat",
  "newGBQR" = "newgbqr",
  "STArima" = "starima",
  "CalCopycat" = "calcopycat",
  "parGBQR" = "pargbqr",
  "FourCAT" = "fourcat"
)

# CalCopycat is grouped with the other still-maturing models below (it was
# recently rebuilt around a season-free date-indexed matching engine -- see
# R/CalCopycat.R).
retrospective_development_model_choices <- c("calcopycat", "pargbqr", "fourcat")

retrospective_default_model_choices <- retrospective_model_choices[
  !(retrospective_model_choices %in% retrospective_development_model_choices)
]

retrospective_null_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

retrospective_parameter_specs <- function(has_population = FALSE) {
  list(
    baseline_regular = list(),
    baseline_seasonal = list(),
    baseline_opt = list(),
    inla = list(
      forecast_uncertainty = list(
        label = "Forecast uncertainty",
        type = "choice",
        default = "default",
        choices = c("default", "small", "tiny")
      ),
      use_offset = list(
        label = "Use population offset",
        type = "logical",
        default = isTRUE(has_population)
      ),
      # "besagproper" additionally needs a neighbor graph, uploaded on the
      # Retrospective tab itself (not the Data tab -- this tab has its own
      # dataset and validates the graph against ITS target groups). Without a
      # usable one the fit falls back to "exchangeable" and warns.
      #
      # The graph is captured once per run by retrospective_model_runners(),
      # not carried in params, so configs in the same run cannot use DIFFERENT
      # graphs. "With vs without" is expressed by this setting instead: a
      # config on "exchangeable" ignores the graph, one on "besagproper" uses
      # it, and both can sit in one run.
      seasonal = list(
        label = "Seasonality",
        type = "choice",
        default = "shared",
        choices = c("shared", "season_group", "target_group")
      ),
      interaction = list(
        label = "Group structure",
        type = "choice",
        default = "exchangeable",
        choices = c(
          "exchangeable", "iid", "none",
          "exchangeable_main", "iid_main",
          "besagproper"
        )
      )
    ),
    copycat = list(
      recent_weeks_touse = list(
        label = "Weeks to use",
        type = "numeric",
        default = 100L,
        min = 3L,
        max = 100L,
        step = 1L
      ),
      resp_week_range = list(
        label = "Response week range",
        type = "numeric",
        default = 2L,
        min = 0L,
        max = 10L,
        step = 1L
      ),
      share_groups = list(
        label = "Share target groups",
        type = "logical",
        default = TRUE
      ),
      weight_exponent = list(
        label = "Weight exponent",
        type = "numeric",
        default = 2L,
        min = 1L,
        max = 3L,
        step = 1L
      ),
      add_poisson_noise = list(
        label = "Add Poisson noise",
        type = "logical",
        default = TRUE
      ),
      points_per_knot = list(
        label = "Data points per knot",
        type = "numeric",
        default = 5L,
        min = 3L,
        max = 6L,
        step = 1L
      ),
      max_matches = list(
        label = "Max historical matches",
        type = "optional_numeric",
        default = NA_real_,
        min = 1L,
        step = 1L
      )
    ),
    calcopycat = list(
      recent_weeks_touse = list(
        label = "Weeks to use",
        type = "numeric",
        default = 12L,
        min = 3L,
        max = 50L,
        step = 1L
      ),
      resp_week_range = list(
        label = "Response week range",
        type = "numeric",
        default = 2L,
        min = 0L,
        max = 10L,
        step = 1L
      ),
      share_groups = list(
        label = "Share target groups",
        type = "logical",
        default = TRUE
      )
    ),
    newgbqr = list(
      model_type = list(
        label = "Model type",
        type = "choice",
        default = "global",
        choices = c("global", "individual")
      ),
      num_bags = list(
        label = "Bags",
        type = "numeric",
        default = 50L,
        min = 10L,
        max = 100L,
        step = 1L
      ),
      nrounds = list(
        label = "Boosting rounds",
        type = "numeric",
        default = 100L,
        min = 10L,
        max = 300L,
        step = 10L
      ),
      num_leaves = list(
        label = "Max leaves per tree",
        type = "numeric",
        default = 11L,
        min = 3L,
        max = 63L,
        step = 2L
      )
    ),
    pargbqr = list(
      model_type = list(
        label = "Model type",
        type = "choice",
        default = "global",
        choices = c("global", "individual")
      ),
      num_bags = list(
        label = "Bags",
        type = "numeric",
        default = 50L,
        min = 10L,
        max = 100L,
        step = 1L
      ),
      nrounds = list(
        label = "Boosting rounds",
        type = "numeric",
        default = 100L,
        min = 10L,
        max = 300L,
        step = 10L
      ),
      num_leaves = list(
        label = "Max leaves per tree",
        type = "numeric",
        default = 11L,
        min = 3L,
        max = 63L,
        step = 2L
      )
    ),
    starima = list(),
    fourcat = list()
  )
}

retrospective_default_settings <- function(has_population = FALSE) {
  purrr::map(retrospective_parameter_specs(has_population), function(specs) {
    purrr::map(specs, "default")
  })
}

retrospective_model_label <- function(model_id) {
  labels <- stats::setNames(names(retrospective_model_choices), retrospective_model_choices)
  label <- unname(labels[[model_id]])
  if (is.null(label) || is.na(label)) model_id else label
}

retrospective_format_param_value <- function(value) {
  if (length(value) == 1 && is.na(value)) {
    return("default")
  }
  if (is.logical(value)) {
    return(ifelse(isTRUE(value), "true", "false"))
  }
  if (length(value) > 1) {
    return(paste(value, collapse = "+"))
  }
  as.character(value)
}

retrospective_params_label <- function(params) {
  if (is.null(params) || length(params) == 0) {
    return("default")
  }

  paste(
    vapply(
      names(params),
      function(nm) paste0(nm, "=", retrospective_format_param_value(params[[nm]])),
      character(1)
    ),
    collapse = "; "
  )
}

retrospective_make_run_label <- function(model_id, params, compact_default = FALSE) {
  model_label <- retrospective_model_label(model_id)
  param_label <- retrospective_params_label(params)

  if (isTRUE(compact_default) && identical(param_label, "default")) {
    return(model_label)
  }

  paste0(model_label, " (", param_label, ")")
}

retrospective_make_run_id <- function(model_id, params, index = NULL) {
  param_part <- if (is.null(params) || length(params) == 0) {
    "default"
  } else {
    paste(
      vapply(
        names(params),
        function(nm) paste0(
          nm,
          "_",
          gsub("[^A-Za-z0-9]+", "-", retrospective_format_param_value(params[[nm]]))
        ),
        character(1)
      ),
      collapse = "__"
    )
  }

  run_id <- paste(model_id, param_part, sep = "__")
  if (!is.null(index)) {
    run_id <- paste(run_id, index, sep = "__")
  }
  run_id
}

retrospective_validate_model_params <- function(model_id, params, has_population = FALSE) {
  specs <- retrospective_parameter_specs(has_population)[[model_id]]
  if (is.null(specs)) {
    stop("Unknown retrospective model: ", model_id, call. = FALSE)
  }

  unknown_params <- setdiff(names(params), names(specs))
  if (length(unknown_params) > 0) {
    stop(
      "Unknown parameter(s) for ",
      retrospective_model_label(model_id),
      ": ",
      paste(unknown_params, collapse = ", "),
      call. = FALSE
    )
  }

  validated <- purrr::map(specs, "default")
  for (param_name in names(params)) {
    spec <- specs[[param_name]]
    value <- params[[param_name]]

    if (identical(spec$type, "numeric")) {
      value <- as.numeric(value)
      if (length(value) != 1 || is.na(value)) {
        stop(param_name, " must be a single number.", call. = FALSE)
      }
      if (!is.null(spec$min) && value < spec$min) {
        stop(param_name, " must be at least ", spec$min, ".", call. = FALSE)
      }
      if (!is.null(spec$max) && value > spec$max) {
        stop(param_name, " must be at most ", spec$max, ".", call. = FALSE)
      }
      if (identical(spec$default, as.integer(spec$default))) {
        value <- as.integer(value)
      }
    } else if (identical(spec$type, "optional_numeric")) {
      value <- as.numeric(value)
      if (length(value) != 1) {
        stop(param_name, " must be a single number or blank.", call. = FALSE)
      }
      if (is.na(value)) {
        value <- NA_real_
      } else {
        if (!is.null(spec$min) && value < spec$min) {
          stop(param_name, " must be at least ", spec$min, ".", call. = FALSE)
        }
        if (!is.null(spec$max) && value > spec$max) {
          stop(param_name, " must be at most ", spec$max, ".", call. = FALSE)
        }
        if (!is.na(spec$default) && identical(spec$default, as.integer(spec$default))) {
          value <- as.integer(value)
        }
      }
    } else if (identical(spec$type, "choice")) {
      value <- as.character(value)
      if (length(value) != 1 || !value %in% spec$choices) {
        stop(param_name, " must be one of: ", paste(spec$choices, collapse = ", "), call. = FALSE)
      }
    } else if (identical(spec$type, "text")) {
      value <- trimws(as.character(value))
      if (length(value) != 1 || !nzchar(value)) {
        stop(param_name, " must not be blank.", call. = FALSE)
      }
    } else if (identical(spec$type, "logical")) {
      if (is.character(value)) {
        value <- tolower(value) %in% c("true", "yes", "shared", "1")
      }
      value <- isTRUE(value)
    } else if (identical(spec$type, "integer_vector")) {
      if (is.character(value) && length(value) == 1) {
        value <- strsplit(value, ",", fixed = TRUE)[[1]]
      }
      value <- as.integer(trimws(value))
      if (length(value) == 0 || any(is.na(value))) {
        stop(param_name, " must contain one or more integers.", call. = FALSE)
      }
    }

    validated[[param_name]] <- value
  }

  if (model_id %in% c("newgbqr", "pargbqr") &&
      identical(validated$peak_week_method, "fixed") &&
      (is.null(validated$peak_week) || length(validated$peak_week) != 1 || is.na(validated$peak_week))) {
    stop("peak_week must be provided when peak_week_method is fixed.", call. = FALSE)
  }

  validated
}

#' Build the Shiny input widget for one retrospective model parameter, from
#' its `retrospective_parameter_specs()` entry. Pulled out of
#' output$retrospective_parameter_inputs_ui's renderUI() (server/
#' retrospective.R) so every spec's widget can be constructed and checked
#' outside a live Shiny session -- constructing an input tag needs no
#' reactive context, only rendering/wiring it into the page does.
#'
#' NULL vs NA matters here: a spec that simply omits an optional bound (e.g.
#' max_matches has no `max`) makes spec$min/max/step resolve to NULL via
#' plain list `$` access, but shiny::numericInput()'s own body does
#' `if (!is.na(min))` on whatever it's given -- is.na(NULL) is logical(0),
#' and `if` on a length-zero value errors with "argument is of length
#' zero". Every optional numeric bound below is coalesced to NA (shiny's own
#' "no bound" sentinel) specifically to avoid that.
retrospective_parameter_input_widget <- function(input_id, spec) {
  if (identical(spec$type, "numeric")) {
    numericInput(
      input_id,
      spec$label,
      value = spec$default,
      min = retrospective_null_coalesce(spec$min, NA),
      max = retrospective_null_coalesce(spec$max, NA),
      step = retrospective_null_coalesce(spec$step, NA)
    )
  } else if (identical(spec$type, "choice")) {
    selectInput(
      input_id,
      spec$label,
      choices = spec$choices,
      selected = spec$default
    )
  } else if (identical(spec$type, "optional_numeric")) {
    numericInput(
      input_id,
      spec$label,
      value = if (is.na(spec$default)) NA else spec$default,
      min = retrospective_null_coalesce(spec$min, NA),
      max = retrospective_null_coalesce(spec$max, NA),
      step = retrospective_null_coalesce(spec$step, NA)
    )
  } else if (identical(spec$type, "logical")) {
    checkboxInput(
      input_id,
      spec$label,
      value = isTRUE(spec$default)
    )
  } else if (identical(spec$type, "text")) {
    textInput(
      input_id,
      spec$label,
      value = spec$default
    )
  } else if (identical(spec$type, "integer_vector")) {
    textInput(
      input_id,
      spec$label,
      value = paste(spec$default, collapse = ",")
    )
  }
}

retrospective_build_run_configs <- function(models,
                                            settings = retrospective_default_settings(),
                                            has_population = FALSE,
                                            labels_use_base_model = FALSE) {
  if (length(models) == 0) {
    return(tibble::tibble(
      run_id = character(),
      model_id = character(),
      model_label = character(),
      run_label = character(),
      params = list()
    ))
  }

  configs <- purrr::imap(models, function(model_id, i) {
    params <- retrospective_validate_model_params(
      model_id = model_id,
      params = retrospective_null_coalesce(settings[[model_id]], list()),
      has_population = has_population
    )

    tibble::tibble(
      run_id = if (isTRUE(labels_use_base_model)) {
        model_id
      } else {
        retrospective_make_run_id(model_id, params, i)
      },
      model_id = model_id,
      model_label = retrospective_model_label(model_id),
      run_label = if (isTRUE(labels_use_base_model)) {
        retrospective_model_label(model_id)
      } else {
        retrospective_make_run_label(model_id, params)
      },
      params = list(params)
    )
  })

  dplyr::bind_rows(configs)
}

retrospective_validate_run_configs <- function(run_configs, has_population = FALSE) {
  required_cols <- c("run_id", "model_id", "model_label", "run_label", "params")
  missing_cols <- setdiff(required_cols, names(run_configs))
  if (length(missing_cols) > 0) {
    stop("Run configuration table is missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (nrow(run_configs) == 0) {
    stop("Select at least one model configuration.", call. = FALSE)
  }
  if (anyDuplicated(run_configs$run_id) > 0) {
    stop("Retrospective run IDs must be unique.", call. = FALSE)
  }
  if (anyDuplicated(run_configs$run_label) > 0) {
    stop("Retrospective run labels must be unique.", call. = FALSE)
  }

  run_configs$params <- purrr::map2(
    run_configs$model_id,
    run_configs$params,
    retrospective_validate_model_params,
    has_population = has_population
  )

  run_configs
}

retrospective_run_config_metadata <- function(run_configs) {
  if (is.null(run_configs) || nrow(run_configs) == 0) {
    return(tibble::tibble(
      run_id = character(),
      model_id = character(),
      model_label = character(),
      run_label = character(),
      parameter = character(),
      value = character()
    ))
  }

  purrr::pmap_dfr(
    run_configs,
    function(run_id, model_id, model_label, run_label, params, ...) {
      if (length(params) == 0) {
        return(tibble::tibble(
          run_id = run_id,
          model_id = model_id,
          model_label = model_label,
          run_label = run_label,
          parameter = NA_character_,
          value = NA_character_
        ))
      }

      tibble::tibble(
        run_id = run_id,
        model_id = model_id,
        model_label = model_label,
        run_label = run_label,
        parameter = names(params),
        value = vapply(params, retrospective_format_param_value, character(1))
      )
    }
  )
}

retrospective_resolve_reference_model <- function(forecasts,
                                                  run_configs = NULL,
                                                  requested_reference_model = NULL) {
  available_models <- retrospective_available_reference_models(forecasts)

  if (!is.null(requested_reference_model) &&
      length(requested_reference_model) == 1 &&
      requested_reference_model %in% available_models) {
    return(requested_reference_model)
  }

  if ("Regular Baseline" %in% available_models) {
    return("Regular Baseline")
  }

  if (!is.null(run_configs) && nrow(run_configs) > 0) {
    regular_baseline_label <- run_configs |>
      dplyr::filter(model_id == "baseline_regular") |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(run_label)

    if (length(regular_baseline_label) == 1 && regular_baseline_label %in% available_models) {
      return(regular_baseline_label)
    }
  }

  # Neither the requested reference, the literal "Regular Baseline" label,
  # nor a configured Regular Baseline run is actually among this group's
  # completed forecasts (e.g. it was removed via an incremental "remove
  # model" run). Returning the "Regular Baseline" literal here regardless
  # (the old behavior) silently hands every relative-WIS computation
  # downstream a reference model that doesn't exist -- add_direct_baseline_
  # relative_skill() then finds no match and quietly fills the whole column
  # with NA, with nothing anywhere telling the user why. Fall back to any
  # other baseline-labeled model that IS actually available, then to any
  # available model at all (alphabetically first, for determinism), and
  # only return NA (explicitly "no usable reference for this group") when
  # literally nothing completed.
  if (length(available_models) == 0) {
    return(NA_character_)
  }

  baseline_available <- available_models[is_retrospective_baseline_model(available_models)]
  if (length(baseline_available) > 0) {
    return(baseline_available[[1]])
  }

  available_models[[1]]
}

retrospective_available_reference_models <- function(forecasts) {
  if (is.null(forecasts) || nrow(forecasts) == 0) {
    character()
  } else {
    forecasts |>
      dplyr::filter(model != "Ensemble") |>
      dplyr::distinct(model) |>
      dplyr::arrange(model) |>
      dplyr::pull(model)
  }
}

write_retrospective_analysis_metadata <- function(output_dir,
                                                  scoring_reference = NULL,
                                                  ensemble_models = character(),
                                                  ensemble_method = NULL) {
  metadata <- tibble::tibble(
    field = c("scoring_reference", "ensemble_models", "ensemble_method"),
    value = c(
      retrospective_null_coalesce(scoring_reference, NA_character_),
      paste(retrospective_null_coalesce(ensemble_models, character()), collapse = ";"),
      retrospective_null_coalesce(ensemble_method, NA_character_)
    )
  )
  readr::write_csv(metadata, file.path(output_dir, "retrospective_analysis_metadata.csv"))
  invisible(metadata)
}

#' Persist the exact raw data a retrospective run was fit on, so a "Load
#' Previous Run" folder-browse feature can hand this same file straight back
#' through the app's normal validate_data()/read_raw_data() upload pipeline
#' later -- reusing that pipeline (rather than this engine code re-deriving
#' `raw_data` some other way) is what guarantees a loaded run behaves
#' identically to a live upload, including for
#' [add_retrospective_run_configs()]. Written once, at the top of
#' [run_retrospective_forecasts()], before any group-splitting -- so it's the
#' full original upload (with the group column, if any), not one group's
#' slice of it.
write_retrospective_source_data <- function(output_dir, data) {
  readr::write_csv(data, file.path(output_dir, "retrospective_source_data.csv"))
  invisible(data)
}

#' Persist the neighbor graph a run was fit with, so "Load Previous Run" can
#' reproduce a spatial configuration. Written in the same edge-list format the
#' user uploaded, so the saved file is itself a valid upload. Nothing is written
#' when the run used no graph, and load_retrospective_neighbor_graph() treats a
#' missing file as "no graph" -- which is also what a run saved before this
#' feature existed looks like.
write_retrospective_neighbor_graph <- function(output_dir, neighbor_graph) {
  if (is.null(neighbor_graph) || nrow(neighbor_graph) == 0) {
    return(invisible(NULL))
  }
  readr::write_csv(
    neighbor_graph[, c("target_group", "neighbor"), drop = FALSE],
    file.path(output_dir, "retrospective_neighbor_graph.csv")
  )
  invisible(neighbor_graph)
}

#' Persist the seasonal grouping a run was fit with, so "Load Previous Run" can
#' reproduce a per-season-group configuration. Same contract as
#' write_retrospective_neighbor_graph(): written in the uploaded format, absent
#' when unused.
write_retrospective_season_groups <- function(output_dir, season_groups) {
  if (is.null(season_groups) || nrow(season_groups) == 0) {
    return(invisible(NULL))
  }
  readr::write_csv(
    season_groups[, c("target_group", "season_group"), drop = FALSE],
    file.path(output_dir, "retrospective_season_groups.csv")
  )
  invisible(season_groups)
}

#' Read back what write_retrospective_season_groups() wrote. NULL covers both
#' "this run used no seasonal grouping" and "this run predates the feature".
load_retrospective_season_groups <- function(source_dir) {
  path <- file.path(source_dir, "retrospective_season_groups.csv")
  if (!file.exists(path)) {
    return(NULL)
  }
  sg <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  if (nrow(sg) == 0 || !all(c("target_group", "season_group") %in% names(sg))) {
    return(NULL)
  }
  as.data.frame(sg[, c("target_group", "season_group")], stringsAsFactors = FALSE)
}

#' Read back what write_retrospective_neighbor_graph() wrote. Returns NULL when
#' absent, which covers both "this run used no graph" and "this run predates the
#' feature".
load_retrospective_neighbor_graph <- function(source_dir) {
  path <- file.path(source_dir, "retrospective_neighbor_graph.csv")
  if (!file.exists(path)) {
    return(NULL)
  }
  graph <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  if (nrow(graph) == 0 || !all(c("target_group", "neighbor") %in% names(graph))) {
    return(NULL)
  }
  as.data.frame(graph[, c("target_group", "neighbor")], stringsAsFactors = FALSE)
}

#' Persist the run-level settings that a "Load Previous Run" feature can't
#' recover from anywhere else on disk: the forecast horizon, and (only for
#' an ungrouped run) the single seasonality zone used. `seasonality` and
#' `group_col` are recorded as NA when not applicable (grouped runs keep
#' their per-group zones in retrospective_group_folders.csv instead, via
#' [write_retrospective_group_manifest()]). Written once, alongside
#' [write_retrospective_source_data()], and never rewritten afterwards --
#' unlike the config table, none of this changes when a model is added or
#' removed from an existing result. `run_name` is the user's own optional
#' shorthand label for the whole run (see [retrospective_sanitize_run_name()]
#' / [retrospective_run_folder_stamp()]) -- purely descriptive, never
#' validated against anything.
write_retrospective_run_settings <- function(output_dir, horizon, seasonality, group_col, data_type = "count", run_name = NULL) {
  settings <- tibble::tibble(
    field = c("horizon", "seasonality", "group_col", "data_type", "run_name"),
    value = c(
      as.character(horizon),
      retrospective_null_coalesce(seasonality, NA_character_),
      retrospective_null_coalesce(group_col, NA_character_),
      retrospective_null_coalesce(data_type, "count"),
      retrospective_null_coalesce(run_name, "")
    )
  )
  readr::write_csv(settings, file.path(output_dir, "retrospective_run_settings.csv"))
  invisible(settings)
}

#' Read a field/value metadata CSV (as written by
#' [write_retrospective_analysis_metadata()] or
#' [write_retrospective_run_settings()]) into a plain named list, so callers
#' can access fields as `settings$horizon` etc. Missing file -> empty list,
#' so callers can use retrospective_null_coalesce()-style fallbacks without
#' every caller having to check file.exists() first.
read_retrospective_metadata_file <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }
  tbl <- readr::read_csv(path, show_col_types = FALSE)
  stats::setNames(as.list(tbl$value), tbl$field)
}

#' Persist the group -> folder / seasonality mapping for a grouped run, so a
#' "Load Previous Run" feature can find each group's per-date CSVs and
#' restore each group's local-seasonality selector without needing to
#' recompute either (folder names are de-duplicated/sanitized, and a zone
#' can't always be uniquely re-derived from a country search box). Written
#' once, right after [make_unique_retrospective_group_folder_names()] is
#' called in [run_retrospective_forecasts()] -- like
#' [write_retrospective_run_settings()], this never changes after the
#' initial run, since adding/removing a model config never adds/removes a
#' group.
write_retrospective_group_manifest <- function(output_dir, group_values, group_folder_names, group_seasonality_resolved, group_data_type_resolved) {
  manifest <- tibble::tibble(
    group = group_values,
    folder = unname(group_folder_names[group_values]),
    seasonality = unname(group_seasonality_resolved[group_values]),
    data_type = unname(group_data_type_resolved[group_values])
  )
  readr::write_csv(manifest, file.path(output_dir, "retrospective_group_folders.csv"))
  invisible(manifest)
}

#' Persist the resolved scoring reference for every group of a grouped run.
#' Unlike the run settings/group manifest above, this genuinely can change
#' after the initial run (the scoring-reference selector and
#' add_retrospective_run_configs() can both update it per group), so this is
#' called both at initial-run time (combine_retrospective_group_results())
#' and every time [rewrite_retrospective_output_files()] runs -- mirroring
#' how retrospective_analysis_metadata.csv's (collapsed, display-only)
#' scoring_reference field is already kept in sync on every rewrite. This
#' file is what lets a *loaded* grouped run recover the real per-group
#' named-vector shape that retrospective_analysis_metadata.csv's
#' semicolon-joined summary can't represent.
write_retrospective_group_scoring_reference <- function(output_dir, scoring_reference, group_col) {
  groups <- names(scoring_reference)
  if (is.null(groups)) {
    return(invisible(NULL))
  }
  tbl <- tibble::tibble(
    group = groups,
    scoring_reference = unname(scoring_reference)
  )
  readr::write_csv(tbl, file.path(output_dir, "retrospective_group_scoring_reference.csv"))
  invisible(tbl)
}

#' Inverse of [retrospective_run_config_metadata()]: turn a long-format
#' run_id/model_id/model_label/run_label/parameter/value table (as read back
#' from a persisted retrospective_run_configs.csv) into the wide run_configs
#' tibble shape (`params` as a list-column) every engine function expects.
retrospective_params_list_from_rows <- function(parameter, value) {
  keep <- !is.na(parameter)
  if (!any(keep)) {
    return(list())
  }
  params <- as.list(value[keep])
  names(params) <- parameter[keep]
  params
}

retrospective_run_configs_from_metadata <- function(metadata) {
  empty <- tibble::tibble(
    run_id = character(), model_id = character(), model_label = character(),
    run_label = character(), params = list()
  )
  if (is.null(metadata) || nrow(metadata) == 0) {
    return(empty)
  }

  # distinct() keeps first-occurrence order, so configs come back in the
  # same order they were originally added rather than being re-sorted.
  keys <- metadata |> dplyr::distinct(run_id, model_id, model_label, run_label)
  keys$params <- purrr::map(keys$run_id, function(id) {
    rows <- metadata[metadata$run_id == id, ]
    retrospective_params_list_from_rows(rows$parameter, rows$value)
  })
  keys
}

#' Read and row-bind every per-reference-date forecast CSV in a retrospective
#' output directory (or one group's subfolder of one) -- the inverse of the
#' per-date `retrospective_<date>.csv` files [run_retrospective_forecasts_
#' single()] writes. Ignores every other `retrospective_*.csv` roll-up file
#' in the same directory by matching the strict `retrospective_YYYY-MM-DD.csv`
#' filename pattern.
read_retrospective_weekly_files <- function(dir) {
  files <- list.files(dir, pattern = "^retrospective_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    return(tibble::tibble(
      model = character(),
      reference_date = as.Date(character()),
      horizon = integer(),
      target_end_date = as.Date(character()),
      target_group = character(),
      output_type = character(),
      output_type_id = character(),
      value = numeric()
    ))
  }
  # output_type_id holds values like "0.1"/"0.5" for quantile output --
  # readr's column-type guessing parses those back as double, but every
  # in-memory forecasts tibble (format_retrospective_forecasts() always
  # coerces it via as.character()) expects it as character. Pin it
  # explicitly so a loaded run's forecasts tibble has the same column types
  # a live run's does -- otherwise merging the two in
  # add_retrospective_run_configs() via dplyr::bind_rows() errors.
  purrr::map_dfr(
    files,
    readr::read_csv,
    col_types = readr::cols(output_type_id = readr::col_character(), .default = readr::col_guess()),
    show_col_types = FALSE
  )
}

#' Read a previously-run retrospective output folder back into the same
#' in-memory `result` list shape [run_retrospective_forecasts()] produces
#' (see [combine_retrospective_group_results()] /
#' [run_retrospective_forecasts_single()]'s return value), so a run from an
#' earlier session can be resumed -- viewed exactly as it was, and (via
#' [add_retrospective_run_configs()]) extended with more models -- without
#' starting over. `source_dir` is the top-level folder from a previously
#' downloaded and extracted retrospective ZIP (i.e. `output_dir` from the
#' original run).
#'
#' Deliberately does NOT itself re-derive `raw_data` from
#' retrospective_source_data.csv -- it returns that file's path instead, so
#' the caller (server/retrospective.R) can run it through the exact same
#' validate_data()/read_raw_data() pipeline a live upload goes through. That
#' guarantees a loaded run's `raw_data` is validated (and shaped -- date
#' parsing, retrospective_group trimming, etc.) identically to a fresh
#' upload, rather than this function quietly reimplementing that pipeline a
#' second time and risking it drifting out of sync.
load_retrospective_run <- function(source_dir) {
  require_file <- function(name) {
    path <- file.path(source_dir, name)
    if (!file.exists(path)) {
      stop(
        "This doesn't look like a retrospective output folder -- missing '", name, "'. ",
        "Pick the top-level folder from a previously downloaded and extracted retrospective ZIP.",
        call. = FALSE
      )
    }
    path
  }

  source_data_path <- require_file("retrospective_source_data.csv")
  run_configs_path <- require_file("retrospective_run_configs.csv")

  run_settings <- read_retrospective_metadata_file(file.path(source_dir, "retrospective_run_settings.csv"))
  horizon <- suppressWarnings(as.integer(retrospective_null_coalesce(run_settings$horizon, NA)))
  if (is.na(horizon)) {
    stop("This retrospective output folder is missing its run settings (horizon). Cannot load.", call. = FALSE)
  }
  seasonality <- retrospective_null_coalesce(run_settings$seasonality, NA_character_)
  group_col <- retrospective_null_coalesce(run_settings$group_col, NA_character_)
  if (is.na(group_col)) group_col <- NULL
  if (is.na(seasonality)) seasonality <- NULL
  # Absent on a run written before Data Type existed -- defaults to "count",
  # matching every other data_type default in the app.
  data_type <- retrospective_null_coalesce(run_settings$data_type, "count")
  # Absent on a run written before Run Name existed -- defaults to "" (no
  # name), same as a run where the user just left the field blank.
  run_name <- retrospective_null_coalesce(run_settings$run_name, "")

  # Every column here is an identifier or a stringified parameter value, never
  # something that should be read back as numeric/logical -- readr's
  # column-type guessing would otherwise silently mangle a purely
  # numeric-looking run_id/model_label/run_label (precision loss above ~15-16
  # digits) or a parameter value that happens to look like TRUE/FALSE/T/F
  # (permanently coerced to logical on reload). Mirrors the output_type_id
  # fix already applied in read_retrospective_weekly_files().
  run_configs <- retrospective_run_configs_from_metadata(
    readr::read_csv(
      run_configs_path,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    )
  )

  analysis_metadata <- read_retrospective_metadata_file(
    file.path(source_dir, "retrospective_analysis_metadata.csv")
  )
  ensemble_models <- {
    raw <- retrospective_null_coalesce(analysis_metadata$ensemble_models, NA_character_)
    if (is.na(raw) || !nzchar(raw)) character() else strsplit(raw, ";", fixed = TRUE)[[1]]
  }
  ensemble_method <- retrospective_null_coalesce(analysis_metadata$ensemble_method, "median")

  # A missing file (e.g. no failures at all -- retrospective_failures.csv is
  # only ever written when nrow(failures) > 0, see rewrite_retrospective_
  # output_files()) must still come back with the RIGHT columns, not a bare
  # 0x0 tibble -- downstream code (add_retrospective_run_configs() filtering
  # by `run_id`, group_scoped() selecting by group_col, etc.) expects those
  # columns to exist even on an empty result, exactly as a live in-memory
  # run's empty tibbles already do (see run_retrospective_forecasts_single()).
  empty_shapes <- list(
    "retrospective_failures.csv" = tibble::tibble(
      reference_date = as.Date(character()), run_id = character(),
      model_id = character(), model = character(), message = character()
    ),
    "retrospective_successes.csv" = tibble::tibble(
      reference_date = as.Date(character()), run_id = character(),
      model_id = character(), model = character(), rows = integer()
    ),
    "retrospective_score_rows.csv" = tibble::tibble(
      model = character(), reference_date = as.Date(character()), horizon = integer(),
      target_end_date = as.Date(character()), target_group = character(), actual = numeric(),
      wis = numeric(), relative_wis = numeric(), log_wis = numeric(), relative_log_wis = numeric(),
      weighted_interval_score_50 = numeric(), covered_50 = logical(),
      weighted_interval_score_95 = numeric(), covered_95 = logical()
    )
  )
  # Identifier columns that can appear across the various roll-up CSVs
  # read_optional_csv() is used for (successes/failures/score_rows/overall/
  # by_target_group/by_forecast_date) -- pinned to character so a
  # numeric-looking id (e.g. a FIPS/location code) doesn't lose precision
  # past ~15-16 digits, and a value like "TRUE"/"FALSE"/"T"/"F" isn't
  # permanently coerced to logical, exactly the risk output_type_id was
  # already fixed for in read_retrospective_weekly_files(). `group_col`'s
  # actual (dynamic) name is included too, when this is a grouped run --
  # readr silently ignores a col_types entry for a column name that isn't
  # actually present in a given file, so it's safe to always list every one
  # of these regardless of which specific file is being read.
  optional_csv_id_columns <- unique(c("run_id", "model_id", "model", "target_group", "group", "folder", group_col))
  read_optional_csv <- function(name) {
    path <- file.path(source_dir, name)
    if (file.exists(path)) {
      suppressWarnings(readr::read_csv(
        path,
        show_col_types = FALSE,
        col_types = do.call(
          readr::cols,
          c(
            list(.default = readr::col_guess()),
            stats::setNames(
              replicate(length(optional_csv_id_columns), readr::col_character(), simplify = FALSE),
              optional_csv_id_columns
            )
          )
        )
      ))
    } else {
      retrospective_null_coalesce(empty_shapes[[name]], tibble::tibble())
    }
  }

  build_files_manifest <- function(forecasts_slice, dir, group_value = NULL) {
    if (nrow(forecasts_slice) == 0) {
      return(tibble::tibble(reference_date = as.Date(character()), file = character(), rows = integer()))
    }
    manifest <- forecasts_slice |>
      dplyr::count(reference_date, name = "rows") |>
      dplyr::mutate(
        file = file.path(dir, paste0("retrospective_", format(reference_date, "%Y-%m-%d"), ".csv"))
      ) |>
      dplyr::select(reference_date, file, rows)
    manifest
  }

  if (is.null(group_col)) {
    forecasts <- read_retrospective_weekly_files(source_dir)
    scores <- list(
      rows = read_optional_csv("retrospective_score_rows.csv"),
      overall = read_optional_csv("retrospective_score_overall.csv"),
      by_target_group = read_optional_csv("retrospective_score_by_target_group.csv"),
      by_forecast_date = read_optional_csv("retrospective_score_by_forecast_date.csv")
    )

    result <- list(
      output_dir = source_dir,
      zip_path = if (file.exists(paste0(source_dir, ".zip"))) paste0(source_dir, ".zip") else NULL,
      files = build_files_manifest(forecasts, source_dir),
      forecasts = forecasts,
      scores = scores,
      run_configs = run_configs,
      ensemble_models = ensemble_models,
      ensemble_method = ensemble_method,
      scoring_reference = retrospective_null_coalesce(analysis_metadata$scoring_reference, NA_character_),
      successes = read_optional_csv("retrospective_successes.csv"),
      failures = read_optional_csv("retrospective_failures.csv")
    )
  } else {
    # `group`/`folder` are identifiers (a numeric-looking location code, or
    # "TRUE"/"FALSE"/"T"/"F"-like group values, would otherwise be silently
    # mangled by readr's type guessing) -- `seasonality` is always one of the
    # fixed "A".."E" letter codes, so it's safe to leave guessed.
    manifest <- readr::read_csv(
      require_file("retrospective_group_folders.csv"),
      show_col_types = FALSE,
      col_types = readr::cols(group = readr::col_character(), folder = readr::col_character(), .default = readr::col_guess())
    )
    # Absent on a manifest written before per-group Data Type existed --
    # default every group to "count", same fallback as the old scalar field.
    if (!"data_type" %in% names(manifest)) {
      manifest$data_type <- "count"
    }

    forecasts <- purrr::map_dfr(seq_len(nrow(manifest)), function(i) {
      group_dir <- file.path(source_dir, manifest$folder[[i]])
      read_retrospective_weekly_files(group_dir) |>
        dplyr::mutate(!!group_col := manifest$group[[i]]) |>
        dplyr::relocate(dplyr::all_of(group_col))
    })

    scoring_reference_tbl <- read_optional_csv("retrospective_group_scoring_reference.csv")
    scoring_reference <- if (nrow(scoring_reference_tbl) > 0) {
      stats::setNames(scoring_reference_tbl$scoring_reference, scoring_reference_tbl$group)
    } else {
      stats::setNames(rep(NA_character_, nrow(manifest)), manifest$group)
    }

    scores <- list(
      rows = read_optional_csv("retrospective_score_rows.csv"),
      overall = read_optional_csv("retrospective_score_overall.csv"),
      by_target_group = read_optional_csv("retrospective_score_by_target_group.csv"),
      by_forecast_date = read_optional_csv("retrospective_score_by_forecast_date.csv")
    )

    result <- list(
      output_dir = source_dir,
      zip_path = if (file.exists(paste0(source_dir, ".zip"))) paste0(source_dir, ".zip") else NULL,
      files = build_files_manifest(forecasts, source_dir),
      forecasts = forecasts,
      scores = scores,
      run_configs = run_configs,
      ensemble_models = ensemble_models,
      ensemble_method = ensemble_method,
      scoring_reference = scoring_reference,
      successes = read_optional_csv("retrospective_successes.csv"),
      failures = read_optional_csv("retrospective_failures.csv"),
      group_col = group_col,
      groups = manifest$group
    )
  }

  list(
    result = result,
    source_data_path = source_data_path,
    neighbor_graph = load_retrospective_neighbor_graph(source_dir),
    season_groups = load_retrospective_season_groups(source_dir),
    horizon = horizon,
    seasonality = seasonality,
    group_col = group_col,
    group_seasonality = if (!is.null(group_col)) stats::setNames(as.list(manifest$seasonality), manifest$group) else NULL,
    group_data_type = if (!is.null(group_col)) stats::setNames(as.list(manifest$data_type), manifest$group) else NULL,
    data_type = data_type,
    run_name = run_name
  )
}

#' Which data_type a "Load Previous Run" reload should validate its saved
#' source CSV against: the run's OWN persisted data_type (or "count" for a
#' grouped run, mirroring the has_group_col-forces-"count" convention a fresh
#' upload already uses) -- never whatever data_type the app's Data Type radio
#' button currently happens to show, which reflects leftover UI state from
#' before "Load Previous Run" was clicked and has nothing to do with the file
#' being reloaded. Take `loaded` exactly as [load_retrospective_run()]
#' returns it.
retrospective_load_validation_data_type <- function(loaded) {
  if (!is.null(loaded$group_col)) {
    "count"
  } else {
    retrospective_null_coalesce(loaded$data_type, "count")
  }
}

available_retrospective_reference_dates <- function(data) {
  data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::distinct(date) |>
    dplyr::arrange(date) |>
    dplyr::pull(date) |>
    {\(x) x[-1]}()
}

retrospective_reference_range <- function(data, start_date, end_date) {
  dates <- available_retrospective_reference_dates(data)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  dates[dates >= start_date & dates <= end_date]
}

format_retrospective_forecasts <- function(forecast_df, model_name, reference_date, data_type = "count") {
  reference_date <- as.Date(reference_date)

  forecast_df |>
    dplyr::mutate(
      model = model_name,
      reference_date = reference_date,
      horizon = as.integer(horizon) - 1L,
      target_end_date = reference_date + lubridate::weeks(horizon),
      output_type_id = as.character(output_type_id)
    ) |>
    dplyr::select(
      model,
      reference_date,
      horizon,
      target_end_date,
      target_group,
      output_type,
      output_type_id,
      value
    ) |>
    dplyr::mutate(value = finalize_forecast_value(value, data_type))
}

is_retrospective_baseline_model <- function(model_name) {
  grepl("Baseline", model_name, ignore.case = TRUE)
}

build_retrospective_ensemble <- function(forecasts,
                                         ensemble_members,
                                         ensemble_label = "Ensemble",
                                         method = "median",
                                         data_type = "count") {
  if (is.null(forecasts) || nrow(forecasts) == 0 || length(ensemble_members) < 2) {
    return(NULL)
  }

  # Baseline models are eligible members here if -- and only if -- the caller
  # explicitly asked for them via `ensemble_members` (mirrors the live
  # "Run Ensemble" tab: selectable, just not the default). Callers that want
  # an automatic non-baseline-only ensemble should compute that member list
  # themselves, as build_retrospective_nonbaseline_ensemble() does below.
  member_forecasts <- forecasts |>
    dplyr::filter(model %in% ensemble_members)

  if (dplyr::n_distinct(member_forecasts$model) < 2) {
    return(NULL)
  }

  # Combination math lives in R/ensemble.R (build_ensemble()), shared with the
  # live "Run Ensemble" action, so retrospective backtests combine models
  # identically to a real-time run.
  build_ensemble(
    forecasts   = member_forecasts,
    members     = unique(member_forecasts$model),
    method      = method,
    model_label = ensemble_label,
    data_type   = data_type
  )
}

build_retrospective_nonbaseline_ensemble <- function(weekly_results, ensemble_method = "median", data_type = "count") {
  weekly_output <- dplyr::bind_rows(weekly_results)

  if (nrow(weekly_output) == 0) {
    return(NULL)
  }

  nonbaseline_forecasts <- weekly_output |>
    dplyr::filter(
      !is_retrospective_baseline_model(model),
      model != "Ensemble"
    )

  if (dplyr::n_distinct(nonbaseline_forecasts$model) < 2) {
    return(NULL)
  }

  build_retrospective_ensemble(
    weekly_output,
    ensemble_members = unique(nonbaseline_forecasts$model),
    method = ensemble_method,
    data_type = data_type
  )
}

retrospective_ensemble_plot_data <- function(forecasts,
                                             actual_data,
                                             forecast_stride = 3L,
                                             selected_model = NULL) {
  empty <- list(
    actual = tibble::tibble(
      date = as.Date(character()),
      target_group = character(),
      value = numeric()
    ),
    forecast = tibble::tibble(
      reference_date = as.Date(character()),
      target_group = character(),
      target_end_date = as.Date(character()),
      q0.025 = numeric(),
      q0.25 = numeric(),
      q0.5 = numeric(),
      q0.75 = numeric(),
      q0.975 = numeric()
    ),
    model = NA_character_,
    is_ensemble = FALSE
  )

  if (is.null(forecasts) || nrow(forecasts) == 0) {
    return(empty)
  }

  quantile_forecasts <- forecasts |>
    dplyr::mutate(
      reference_date = as.Date(reference_date),
      target_end_date = as.Date(target_end_date),
      output_type_id = as.character(output_type_id)
    ) |>
    dplyr::filter(
      output_type == "quantile",
      output_type_id %in% c("0.025", "0.25", "0.5", "0.75", "0.975")
    )

  if (nrow(quantile_forecasts) == 0) {
    return(empty)
  }

  available_models <- quantile_forecasts |>
    dplyr::distinct(model) |>
    dplyr::arrange(model) |>
    dplyr::pull(model)
  nonbaseline_models <- available_models[
    !vapply(available_models, is_retrospective_baseline_model, logical(1))
  ]
  nonensemble_nonbaseline_models <- setdiff(nonbaseline_models, "Ensemble")
  plot_model <- if (!is.null(selected_model) &&
                    length(selected_model) == 1 &&
                    selected_model %in% available_models) {
    selected_model
  } else if (length(nonensemble_nonbaseline_models) == 1) {
    nonensemble_nonbaseline_models[[1]]
  } else if ("Ensemble" %in% available_models) {
    "Ensemble"
  } else if (length(nonensemble_nonbaseline_models) > 0) {
    nonensemble_nonbaseline_models[[1]]
  } else {
    available_models[[1]]
  }

  plot_forecasts <- quantile_forecasts |>
    dplyr::filter(model == plot_model)

  reference_dates <- plot_forecasts |>
    dplyr::distinct(reference_date) |>
    dplyr::arrange(reference_date) |>
    dplyr::pull(reference_date)

  stride <- max(1L, as.integer(forecast_stride)[[1]])
  kept_reference_dates <- reference_dates[seq(1L, length(reference_dates), by = stride)]
  if (!utils::tail(reference_dates, 1) %in% kept_reference_dates) {
    kept_reference_dates <- c(kept_reference_dates, utils::tail(reference_dates, 1))
  }

  forecast_plot <- plot_forecasts |>
    dplyr::filter(reference_date %in% kept_reference_dates) |>
    dplyr::select(
      reference_date,
      target_group,
      target_end_date,
      output_type_id,
      value
    ) |>
    tidyr::pivot_wider(
      names_from = output_type_id,
      values_from = value,
      names_prefix = "q"
    ) |>
    dplyr::arrange(target_group, reference_date, target_end_date)

  missing_quantile_cols <- setdiff(
    c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975"),
    names(forecast_plot)
  )
  forecast_plot[missing_quantile_cols] <- NA_real_

  target_groups <- forecast_plot |>
    dplyr::distinct(target_group) |>
    dplyr::pull(target_group)
  target_dates <- forecast_plot |>
    dplyr::distinct(target_end_date) |>
    dplyr::pull(target_end_date)

  actual_plot <- actual_data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(
      target_group %in% target_groups,
      date >= min(target_dates, na.rm = TRUE),
      date <= max(target_dates, na.rm = TRUE)
    ) |>
    dplyr::select(date, target_group, value) |>
    dplyr::arrange(target_group, date)

  list(
    actual = actual_plot,
    forecast = forecast_plot |>
      dplyr::select(
        reference_date,
        target_group,
        target_end_date,
        q0.025,
        q0.25,
        q0.5,
        q0.75,
        q0.975
      ),
    model = plot_model,
    is_ensemble = identical(plot_model, "Ensemble")
  )
}

plot_retrospective_ensemble_forecasts <- function(forecasts,
                                                  actual_data,
                                                  forecast_stride = 3L,
                                                  selected_model = NULL) {
  plot_data <- retrospective_ensemble_plot_data(
    forecasts = forecasts,
    actual_data = actual_data,
    forecast_stride = forecast_stride,
    selected_model = selected_model
  )

  shiny::req(nrow(plot_data$forecast) > 0)

  has_95 <- any(!is.na(plot_data$forecast$`q0.025`)) &&
    any(!is.na(plot_data$forecast$`q0.975`))
  has_50 <- any(!is.na(plot_data$forecast$`q0.25`)) &&
    any(!is.na(plot_data$forecast$`q0.75`))
  has_median <- any(!is.na(plot_data$forecast$`q0.5`))

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = plot_data$actual,
      ggplot2::aes(date, value),
      color = "#1F2937",
      linewidth = 0.65
    ) +
    ggplot2::geom_point(
      data = plot_data$actual,
      ggplot2::aes(date, value),
      color = "#1F2937",
      size = 1.2,
      alpha = 0.8
    )

  if (has_95) {
    p <- p +
      ggplot2::geom_ribbon(
        data = plot_data$forecast,
        ggplot2::aes(
          target_end_date,
          ymin = `q0.025`,
          ymax = `q0.975`,
          group = interaction(reference_date, target_group)
        ),
        fill = "#4EA3C8",
        alpha = 0.13
      )
  }

  if (has_50) {
    p <- p +
      ggplot2::geom_ribbon(
        data = plot_data$forecast,
        ggplot2::aes(
          target_end_date,
          ymin = `q0.25`,
          ymax = `q0.75`,
          group = interaction(reference_date, target_group)
        ),
        fill = "#1B7FA7",
        alpha = 0.22
      )
  }

  if (has_median) {
    p <- p +
      ggplot2::geom_line(
        data = plot_data$forecast,
        ggplot2::aes(
          target_end_date,
          `q0.5`,
          group = interaction(reference_date, target_group)
        ),
        color = "#0B6E99",
        linewidth = 0.8,
        alpha = 0.8
      )
  }

  p +
    ggplot2::facet_wrap(~target_group, scales = "free_y") +
    ggplot2::labs(
      x = NULL,
      y = "Observed value",
      caption = paste0(
        "Black line: observed target data. Blue lines and ribbons: ",
        plot_data$model,
        " forecasts, showing every ",
        forecast_stride,
        "rd forecast origin and always including the final origin."
      )
    ) +
    cowplot::background_grid(major = "xy", minor = "y") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0, color = "#5E6C80")
    )
}

retrospective_score_metric <- function(score_tbl) {
  if (any(!is.na(score_tbl$mean_relative_wis))) {
    return(list(
      column = "mean_relative_wis",
      label = "Relative WIS",
      uses_relative = TRUE
    ))
  }

  list(
    column = "mean_wis",
    label = "WIS",
    uses_relative = FALSE
  )
}

retrospective_wrap_labels <- function(labels, width = 14L) {
  vapply(
    labels,
    function(label) paste(strwrap(label, width = width), collapse = "\n"),
    character(1)
  )
}

plot_retrospective_target_group_scores <- function(score_tbl,
                                                   reference_model = "Regular Baseline",
                                                   data_type = "count") {
  metric <- retrospective_score_metric(score_tbl)
  # Raw WIS (metric$column == "mean_wis", used when no relative-WIS
  # reference is available) lives on the same scale as `value` -- a
  # proportion-scale (0-1) series needs more decimal places than count
  # scale or its heatmap labels all read "0.00". Relative WIS is already a
  # scale-invariant ratio and doesn't need this.
  score_digits <- if (!metric$uses_relative && identical(data_type, "proportion")) 4 else 2
  plot_tbl <- score_tbl |>
    dplyr::filter(model != reference_model) |>
    dplyr::mutate(
      score_value = .data[[metric$column]],
      score_label = ifelse(is.na(score_value), "", sprintf(paste0("%.", score_digits, "f"), score_value))
    )

  if (nrow(plot_tbl) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No non-baseline model scores available.") +
        ggplot2::theme_void(base_size = 15)
    )
  }

  order_category <- if ("Overall" %in% plot_tbl$target_group) {
    "Overall"
  } else {
    plot_tbl |>
      dplyr::distinct(target_group) |>
      dplyr::arrange(target_group) |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(target_group)
  }

  model_levels <- plot_tbl |>
    dplyr::filter(target_group == order_category) |>
    dplyr::arrange(score_value, model) |>
    dplyr::pull(model) |>
    unique()
  model_levels <- c(
    model_levels,
    sort(setdiff(unique(as.character(plot_tbl$model)), model_levels))
  )

  plot_tbl <- plot_tbl |>
    dplyr::mutate(model = factor(model, levels = rev(model_levels)))

  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = target_group, y = model, fill = score_value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = score_label), size = 4.6) +
    ggplot2::scale_x_discrete(labels = retrospective_wrap_labels) +
    ggplot2::labs(
      title = "Retrospective performance by target group",
      subtitle = if (metric$uses_relative) {
        paste0(
          "Cells show relative WIS by model and target group. Lower is better; values below 1 improve on ",
          reference_model,
          "."
        )
      } else {
        "Cells show WIS by model and target group. Lower is better."
      },
      x = "Target group",
      y = NULL,
      fill = metric$label
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 13),
      axis.text.y = ggplot2::element_text(size = 13),
      axis.title.x = ggplot2::element_text(size = 15, margin = ggplot2::margin(t = 10)),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 13),
      plot.title = ggplot2::element_text(size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 14),
      panel.grid = ggplot2::element_blank()
    )

  if (metric$uses_relative) {
    p +
      ggplot2::scale_fill_gradient2(
        low = "#2E8B57",
        mid = "#F7F7F7",
        high = "#C0392B",
        midpoint = 1,
        na.value = "#E5E7EB"
      )
  } else {
    p +
      ggplot2::scale_fill_gradient(
        low = "#E8F3EC",
        high = "#C0392B",
        na.value = "#E5E7EB"
      )
  }
}

plot_retrospective_forecast_date_scores <- function(score_tbl,
                                                    reference_model = "Regular Baseline") {
  metric <- retrospective_score_metric(score_tbl)
  plot_tbl <- score_tbl |>
    dplyr::filter(model != reference_model) |>
    dplyr::mutate(
      reference_date = as.Date(reference_date),
      score_value = .data[[metric$column]],
      is_baseline = is_retrospective_baseline_model(model)
    )

  if (nrow(plot_tbl) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No non-baseline model scores available.") +
        ggplot2::theme_void(base_size = 15)
    )
  }

  baseline_models <- plot_tbl |>
    dplyr::filter(is_baseline) |>
    dplyr::distinct(model) |>
    dplyr::arrange(model) |>
    dplyr::pull(model)
  nonbaseline_models <- plot_tbl |>
    dplyr::filter(!is_baseline) |>
    dplyr::distinct(model) |>
    dplyr::arrange(model) |>
    dplyr::pull(model)

  baseline_colors <- if (length(baseline_models) > 0) {
    stats::setNames(
      grDevices::grey.colors(length(baseline_models), start = 0.35, end = 0.7),
      baseline_models
    )
  } else {
    character()
  }
  model_colors <- c(
    baseline_colors,
    stats::setNames(scales::hue_pal()(length(nonbaseline_models)), nonbaseline_models)
  )

  p <- ggplot2::ggplot(
    plot_tbl,
    ggplot2::aes(x = reference_date, y = score_value, color = model, group = model)
  ) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.95) +
    ggplot2::geom_point(size = 2.8, alpha = 0.95) +
    ggplot2::scale_color_manual(values = model_colors) +
    ggplot2::labs(
      title = "Retrospective performance by forecast date",
      x = "Forecast date",
      y = metric$label,
      color = "Model"
    ) +
    cowplot::background_grid(major = "xy", minor = "y") +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 13),
      axis.text.x = ggplot2::element_text(size = 13),
      axis.text.y = ggplot2::element_text(size = 13),
      axis.title.x = ggplot2::element_text(size = 15, margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(size = 15, margin = ggplot2::margin(r = 10)),
      plot.title = ggplot2::element_text(size = 18, face = "bold")
    )

  if (metric$uses_relative) {
    p + ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "#5E6C80")
  } else {
    p
  }
}

as_scoringutils_scores <- function(score_rows, metrics) {
  class(score_rows) <- c("scores", class(score_rows))
  attr(score_rows, "metrics") <- metrics
  score_rows
}

add_direct_baseline_relative_skill <- function(score_rows,
                                               metric,
                                               output_col,
                                               reference_model) {
  score_rows[[output_col]] <- NA_real_

  if (nrow(score_rows) == 0 || !reference_model %in% score_rows$model) {
    return(score_rows)
  }

  baseline_scores <- score_rows |>
    dplyr::filter(model == reference_model) |>
    dplyr::select(
      reference_date,
      horizon,
      target_end_date,
      target_group,
      baseline_metric = dplyr::all_of(metric)
    )

  score_rows |>
    dplyr::select(-dplyr::all_of(output_col)) |>
    dplyr::left_join(
      baseline_scores,
      by = c("reference_date", "horizon", "target_end_date", "target_group")
    ) |>
    dplyr::mutate(
      !!output_col := dplyr::if_else(
        baseline_metric > 0,
        .data[[metric]] / baseline_metric,
        NA_real_
      )
    ) |>
    dplyr::select(-baseline_metric)
}

add_scoringutils_relative_skill <- function(score_rows,
                                            group_vars,
                                            metric,
                                            output_col,
                                            reference_model) {
  score_rows[[output_col]] <- NA_real_

  if (nrow(score_rows) == 0 ||
      !reference_model %in% score_rows$model) {
    return(score_rows)
  }

  if (length(setdiff(unique(score_rows$model), reference_model)) < 2) {
    return(add_direct_baseline_relative_skill(
      score_rows = score_rows,
      metric = metric,
      output_col = output_col,
      reference_model = reference_model
    ))
  }

  scored <- tryCatch(
    {
      by_arg <- if (length(group_vars) == 0) NULL else group_vars
      skill_input <- score_rows |>
        dplyr::select(
          dplyr::any_of(c(
            "model",
            group_vars,
            "reference_date",
            "horizon",
            "target_end_date",
            "target_group",
            metric
          ))
        )

      suppressWarnings(
        scoringutils::add_relative_skill(
          scores = as_scoringutils_scores(skill_input, metric),
          compare = "model",
          by = by_arg,
          metric = metric,
          baseline = reference_model
        )
      )
    },
    error = function(e) score_rows
  )

  scaled_col <- paste0(metric, "_scaled_relative_skill")
  if (scaled_col %in% names(scored)) {
    skill_lookup <- scored |>
      dplyr::select(dplyr::any_of(c("model", group_vars, scaled_col))) |>
      dplyr::distinct()

    score_rows <- score_rows |>
      dplyr::select(-dplyr::all_of(output_col)) |>
      dplyr::left_join(skill_lookup, by = c("model", group_vars)) |>
      dplyr::rename(!!output_col := dplyr::all_of(scaled_col))
  }

  score_rows
}

add_summary_relative_score <- function(summary_tbl,
                                       group_vars,
                                       metric_col,
                                       output_col,
                                       reference_model) {
  summary_tbl[[output_col]] <- NA_real_

  if (nrow(summary_tbl) == 0 || !reference_model %in% summary_tbl$model) {
    return(summary_tbl)
  }

  baseline_summary <- summary_tbl |>
    dplyr::filter(model == reference_model) |>
    dplyr::select(
      dplyr::any_of(group_vars),
      baseline_metric = dplyr::all_of(metric_col)
    )

  join_by <- group_vars
  if (length(join_by) == 0) {
    baseline_metric <- baseline_summary$baseline_metric[[1]]
    relative_value <- if (is.finite(baseline_metric) && baseline_metric > 0) {
      summary_tbl[[metric_col]] / baseline_metric
    } else {
      rep(NA_real_, nrow(summary_tbl))
    }
    return(summary_tbl |>
      dplyr::mutate(!!output_col := relative_value))
  }

  summary_tbl |>
    dplyr::select(-dplyr::all_of(output_col)) |>
    dplyr::left_join(baseline_summary, by = join_by) |>
    dplyr::mutate(
      !!output_col := dplyr::if_else(
        is.finite(baseline_metric) & baseline_metric > 0,
        .data[[metric_col]] / baseline_metric,
        NA_real_
      )
    ) |>
    dplyr::select(-baseline_metric)
}

summarize_retrospective_score_rows <- function(score_rows,
                                               group_vars = character(),
                                               reference_model = "Regular Baseline") {
  if (nrow(score_rows) == 0) {
    return(tibble::tibble())
  }

  mean_or_na <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }

    mean(x, na.rm = TRUE)
  }

  score_rows <- score_rows |>
    add_scoringutils_relative_skill(
      group_vars = group_vars,
      metric = "wis",
      output_col = "relative_wis",
      reference_model = reference_model
    ) |>
    add_scoringutils_relative_skill(
      group_vars = group_vars,
      metric = "log_wis",
      output_col = "relative_log_wis",
      reference_model = reference_model
    )

  summary_tbl <- score_rows |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "model")))) |>
    dplyr::summarize(
      mean_wis = mean_or_na(wis),
      mean_relative_wis = NA_real_,
      mean_log_wis = mean_or_na(log_wis),
      mean_relative_log_wis = NA_real_,
      coverage_50 = mean_or_na(covered_50),
      coverage_95 = mean_or_na(covered_95),
      n_forecast_targets = dplyr::n(),
      .groups = "drop"
    )

  summary_tbl |>
    add_summary_relative_score(
      group_vars = group_vars,
      metric_col = "mean_wis",
      output_col = "mean_relative_wis",
      reference_model = reference_model
    ) |>
    add_summary_relative_score(
      group_vars = group_vars,
      metric_col = "mean_log_wis",
      output_col = "mean_relative_log_wis",
      reference_model = reference_model
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_vars)), mean_wis, model)
}

retrospective_hubevals_inputs <- function(formatted_forecasts, actual_data) {
  model_out_tbl <- formatted_forecasts |>
    dplyr::mutate(
      reference_date = as.Date(reference_date),
      target_end_date = as.Date(target_end_date),
      output_type_id = as.character(output_type_id)
    ) |>
    dplyr::filter(output_type == "quantile") |>
    dplyr::transmute(
      model_id = model,
      reference_date,
      horizon = as.integer(horizon),
      target_end_date,
      target_group,
      output_type,
      output_type_id,
      value
    )

  oracle_output <- actual_data |>
    dplyr::transmute(
      target_end_date = as.Date(date),
      target_group,
      oracle_value = value
    ) |>
    dplyr::filter(!is.na(oracle_value)) |>
    dplyr::distinct(target_end_date, target_group, .keep_all = TRUE)

  list(model_out_tbl = model_out_tbl, oracle_output = oracle_output)
}

score_retrospective_rows_with_hubevals <- function(model_out_tbl,
                                                   oracle_output,
                                                   reference_model) {
  natural_scores <- suppressWarnings(hubEvals::score_model_out(
    model_out_tbl = model_out_tbl,
    oracle_output = oracle_output,
    metrics = c("wis", "interval_coverage_50", "interval_coverage_95"),
    summarize = FALSE
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(model = model_id)

  if (!"interval_coverage_50" %in% names(natural_scores)) {
    natural_scores$interval_coverage_50 <- NA
  }
  if (!"interval_coverage_95" %in% names(natural_scores)) {
    natural_scores$interval_coverage_95 <- NA
  }
  natural_scores <- natural_scores |>
    dplyr::rename(
      covered_50 = interval_coverage_50,
      covered_95 = interval_coverage_95
    )

  log_scores <- suppressWarnings(hubEvals::score_model_out(
    model_out_tbl = model_out_tbl,
    oracle_output = oracle_output,
    metrics = "wis",
    summarize = FALSE,
    transform = log1p,
    transform_label = "log1p"
  )) |>
    tibble::as_tibble() |>
    dplyr::rename(
      model = model_id,
      log_wis = wis
    )

  score_rows <- natural_scores |>
    dplyr::left_join(
      log_scores,
      by = c("model", "reference_date", "horizon", "target_end_date", "target_group")
    )

  actual_tbl <- oracle_output |>
    dplyr::rename(actual = oracle_value)

  score_rows <- score_rows |>
    dplyr::left_join(actual_tbl, by = c("target_end_date", "target_group")) |>
    dplyr::mutate(
      relative_wis = NA_real_,
      relative_log_wis = NA_real_,
      weighted_interval_score_50 = NA_real_,
      weighted_interval_score_95 = NA_real_
    ) |>
    add_direct_baseline_relative_skill(
      metric = "wis",
      output_col = "relative_wis",
      reference_model = reference_model
    ) |>
    add_direct_baseline_relative_skill(
      metric = "log_wis",
      output_col = "relative_log_wis",
      reference_model = reference_model
    )

  score_rows |>
    dplyr::select(
      model,
      reference_date,
      horizon,
      target_end_date,
      target_group,
      actual,
      wis,
      relative_wis,
      log_wis,
      relative_log_wis,
      weighted_interval_score_50,
      covered_50,
      weighted_interval_score_95,
      covered_95
    )
}

summarize_retrospective_with_hubevals <- function(model_out_tbl,
                                                  oracle_output,
                                                  group_vars = character(),
                                                  reference_model = "Regular Baseline") {
  if (nrow(model_out_tbl) == 0) {
    return(tibble::tibble())
  }

  by <- c("model_id", group_vars)
  has_reference <- reference_model %in% model_out_tbl$model_id
  has_pairwise_comparison <- length(setdiff(unique(model_out_tbl$model_id), reference_model)) >= 2
  relative_metrics <- if (has_reference && has_pairwise_comparison) "wis" else NULL
  baseline <- if (has_reference && has_pairwise_comparison) reference_model else NULL

  natural_summary <- suppressWarnings(hubEvals::score_model_out(
    model_out_tbl = model_out_tbl,
    oracle_output = oracle_output,
    metrics = c("wis", "interval_coverage_50", "interval_coverage_95"),
    relative_metrics = relative_metrics,
    baseline = baseline,
    by = by,
    include_count = TRUE
  )) |>
    tibble::as_tibble()

  if (!"wis_scaled_relative_skill" %in% names(natural_summary)) {
    natural_summary$wis_scaled_relative_skill <- NA_real_
  }
  if (!"interval_coverage_50" %in% names(natural_summary)) {
    natural_summary$interval_coverage_50 <- NA_real_
  }
  if (!"interval_coverage_95" %in% names(natural_summary)) {
    natural_summary$interval_coverage_95 <- NA_real_
  }

  log_summary <- suppressWarnings(hubEvals::score_model_out(
    model_out_tbl = model_out_tbl,
    oracle_output = oracle_output,
    metrics = "wis",
    relative_metrics = relative_metrics,
    baseline = baseline,
    by = by,
    include_count = FALSE,
    transform = log1p,
    transform_label = "log1p"
  )) |>
    tibble::as_tibble()

  if (!"wis_scaled_relative_skill" %in% names(log_summary)) {
    log_summary$wis_scaled_relative_skill <- NA_real_
  }

  summary_tbl <- natural_summary |>
    dplyr::rename(
      model = model_id,
      mean_wis = wis,
      mean_relative_wis = wis_scaled_relative_skill,
      coverage_50 = interval_coverage_50,
      coverage_95 = interval_coverage_95,
      n_forecast_targets = count
    ) |>
    dplyr::select(
      dplyr::any_of(c(group_vars, "model")),
      mean_wis,
      mean_relative_wis,
      coverage_50,
      coverage_95,
      n_forecast_targets
    ) |>
    dplyr::left_join(
      log_summary |>
        dplyr::rename(
          model = model_id,
          mean_log_wis = wis,
          mean_relative_log_wis = wis_scaled_relative_skill
        ) |>
        dplyr::select(dplyr::any_of(c(group_vars, "model")), mean_log_wis, mean_relative_log_wis),
      by = c(group_vars, "model")
    ) |>
    dplyr::select(
      dplyr::any_of(group_vars),
      model,
      mean_wis,
      mean_relative_wis,
      mean_log_wis,
      mean_relative_log_wis,
      coverage_50,
      coverage_95,
      n_forecast_targets
    )

  if (has_reference) {
    summary_tbl <- summary_tbl |>
      add_summary_relative_score(
        group_vars = group_vars,
        metric_col = "mean_wis",
        output_col = "mean_relative_wis",
        reference_model = reference_model
      ) |>
      add_summary_relative_score(
        group_vars = group_vars,
        metric_col = "mean_log_wis",
        output_col = "mean_relative_log_wis",
        reference_model = reference_model
      )
  }

  summary_tbl |>
    dplyr::mutate(
      mean_relative_wis = dplyr::if_else(
        model == reference_model & is.nan(mean_relative_wis),
        1,
        mean_relative_wis
      ),
      mean_relative_log_wis = dplyr::if_else(
        model == reference_model & is.nan(mean_relative_log_wis),
        1,
        mean_relative_log_wis
      )
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_vars)), mean_wis, model)
}

score_retrospective_forecasts <- function(formatted_forecasts,
                                          actual_data,
                                          reference_model = "Regular Baseline") {
  empty_score_rows <- tibble::tibble(
    model = character(),
    reference_date = as.Date(character()),
    horizon = integer(),
    target_end_date = as.Date(character()),
    target_group = character(),
    actual = numeric(),
    wis = numeric(),
    relative_wis = numeric(),
    log_wis = numeric(),
    relative_log_wis = numeric(),
    weighted_interval_score_50 = numeric(),
    covered_50 = logical(),
    weighted_interval_score_95 = numeric(),
    covered_95 = logical()
  )

  if (is.null(formatted_forecasts) || nrow(formatted_forecasts) == 0) {
    return(list(
      rows = empty_score_rows,
      overall = tibble::tibble(),
      by_target_group = tibble::tibble(),
      by_forecast_date = tibble::tibble()
    ))
  }

  if (!requireNamespace("hubEvals", quietly = TRUE)) {
    stop(
      "Install the hubEvals R package to score retrospective forecasts.",
      call. = FALSE
    )
  }

  hub_inputs <- retrospective_hubevals_inputs(formatted_forecasts, actual_data)
  model_out_tbl <- hub_inputs$model_out_tbl
  oracle_output <- hub_inputs$oracle_output

  if (nrow(model_out_tbl) == 0 || nrow(oracle_output) == 0) {
    return(list(
      rows = empty_score_rows,
      overall = tibble::tibble(),
      by_target_group = tibble::tibble(),
      by_forecast_date = tibble::tibble()
    ))
  }

  score_rows <- score_retrospective_rows_with_hubevals(
    model_out_tbl = model_out_tbl,
    oracle_output = oracle_output,
    reference_model = reference_model
  )

  list(
    rows = score_rows,
    overall = summarize_retrospective_with_hubevals(
      model_out_tbl,
      oracle_output,
      reference_model = reference_model
    ),
    by_target_group = summarize_retrospective_with_hubevals(
      model_out_tbl,
      oracle_output,
      group_vars = "target_group",
      reference_model = reference_model
    ),
    by_forecast_date = summarize_retrospective_with_hubevals(
      model_out_tbl,
      oracle_output,
      group_vars = "reference_date",
      reference_model = reference_model
    )
  )
}

#' Pool an already-scored, multi-group `score_rows` table (the `rows`
#' element of [score_retrospective_forecasts()]'s return value, combined
#' across every retrospective group by
#' [combine_retrospective_group_results()]) into ONE row per model --
#' "how did each model do across every group at once".
#'
#' Deliberately works from the row-level score table rather than re-running
#' hubEvals scoring across every group's forecasts at once:
#' `retrospective_hubevals_inputs()`'s `oracle_output` only ever joins
#' actuals on `(target_end_date, target_group)`, with NO location/group
#' dimension, and de-duplicates on that same key -- pooling raw forecasts
#' and actuals from multiple groups into one hubEvals call would silently
#' score every group's forecasts against just ONE group's actual values
#' (since reference dates and target_group values are typically identical
#' across groups). Each group MUST stay scored in isolation; only the
#' resulting per-row numbers are pooled here.
#'
#' Mirrors `summarize_retrospective_with_hubevals()`'s own math: a ratio of
#' pooled means (via `add_summary_relative_score()`), not a mean of
#' per-group summaries or of per-row ratios -- so a group that contributed
#' more forecast targets naturally carries proportionally more weight,
#' rather than every group counting equally regardless of size.
summarize_retrospective_scores_pooled_across_groups <- function(score_rows, reference_model) {
  if (is.null(score_rows) || nrow(score_rows) == 0) {
    return(tibble::tibble())
  }

  summary_tbl <- score_rows |>
    dplyr::group_by(model) |>
    dplyr::summarize(
      mean_wis = mean(wis, na.rm = TRUE),
      mean_log_wis = mean(log_wis, na.rm = TRUE),
      coverage_50 = mean(covered_50, na.rm = TRUE),
      coverage_95 = mean(covered_95, na.rm = TRUE),
      n_forecast_targets = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(mean_relative_wis = NA_real_, mean_relative_log_wis = NA_real_)

  has_reference <- reference_model %in% summary_tbl$model

  if (has_reference) {
    summary_tbl <- summary_tbl |>
      add_summary_relative_score(
        group_vars = character(),
        metric_col = "mean_wis",
        output_col = "mean_relative_wis",
        reference_model = reference_model
      ) |>
      add_summary_relative_score(
        group_vars = character(),
        metric_col = "mean_log_wis",
        output_col = "mean_relative_log_wis",
        reference_model = reference_model
      )
  }

  summary_tbl |>
    dplyr::mutate(
      mean_relative_wis = dplyr::if_else(
        model == reference_model & is.nan(mean_relative_wis), 1, mean_relative_wis
      ),
      mean_relative_log_wis = dplyr::if_else(
        model == reference_model & is.nan(mean_relative_log_wis), 1, mean_relative_log_wis
      )
    ) |>
    dplyr::select(
      model, mean_wis, mean_relative_wis, mean_log_wis, mean_relative_log_wis,
      coverage_50, coverage_95, n_forecast_targets
    ) |>
    dplyr::arrange(mean_wis, model)
}

# `neighbor_graph` is captured by the closures rather than passed through
# `params`, because params is serialised into run ids and run labels
# (retrospective_make_run_id()) and an edge-list data frame has no sensible
# string form. What belongs in params is the *choice* of structure.
retrospective_model_runners <- function(settings, data_type = "count", neighbor_graph = NULL,
                                        season_groups = NULL) {
  list(
    baseline_regular = list(
      label = "Regular Baseline",
      run = function(train_data, horizon, quantiles_needed, seasonality, params = list()) {
        fit_process_baseline_flat(
          df = train_data,
          weeks_ahead = horizon,
          quantiles_needed = quantiles_needed,
          data_type = data_type
        )
      }
    ),
    baseline_seasonal = list(
      label = "Seasonal Baseline",
      run = function(train_data, horizon, quantiles_needed, seasonality, params = list()) {
        fit_process_baseline_seasonal(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          seasonality = seasonality,
          data_type = data_type
        )
      }
    ),
    baseline_opt = list(
      label = "Opt Baseline",
      run = function(train_data, horizon, quantiles_needed, seasonality, params = list()) {
        fit_process_baseline_flat(
          df = train_data,
          weeks_ahead = horizon,
          quantiles_needed = quantiles_needed,
          window_size = 8,
          data_type = data_type
        )
      }
    ),
    inla = list(
      label = "INFLAenza",
      run = function(train_data, horizon, quantiles_needed, seasonality, params = list()) {
        params <- utils::modifyList(settings$inla, params)
        fit_process_inla(
          df = train_data,
          weeks_ahead = horizon,
          quantiles_needed = quantiles_needed,
          forecast_uncertainty = params$forecast_uncertainty,
          use_offset = params$use_offset,
          interaction = params$interaction,
          neighbor_graph = neighbor_graph,
          seasonal = params$seasonal,
          season_groups = season_groups,
          data_type = data_type
        )
      }
    ),
    copycat = list(
      label = "Copycat",
      run = function(train_data, horizon, quantiles_needed, seasonality, params = list()) {
        params <- utils::modifyList(settings$copycat, params)
        fit_process_copycat(
          df = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          recent_weeks_touse = params$recent_weeks_touse,
          resp_week_range = params$resp_week_range,
          seasonality = seasonality,
          share_groups = params$share_groups,
          weight_exponent = params$weight_exponent,
          add_poisson_noise = params$add_poisson_noise,
          points_per_knot = params$points_per_knot,
          max_matches = if (is.na(params$max_matches)) Inf else params$max_matches,
          data_type = data_type
        )
      }
    ),
    calcopycat = list(
      label = "CalCopycat",
      run = function(train_data, horizon, quantiles_needed, seasonality, params = list()) {
        params <- utils::modifyList(settings$calcopycat, params)
        fit_process_calcopycat(
          df = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          recent_weeks_touse = params$recent_weeks_touse,
          resp_week_range = params$resp_week_range,
          share_groups = params$share_groups,
          data_type = data_type
        )
      }
    ),
    newgbqr = list(
      label = "newGBQR",
      run = function(train_data, horizon, quantiles_needed, seasonality, params = list()) {
        params <- utils::modifyList(settings$newgbqr, params)
        fit_process_newgbqr(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          num_bags = params$num_bags,
          bag_frac_samples = 0.7,
          nrounds = params$nrounds,
          num_leaves = params$num_leaves,
          seasonality = seasonality,
          model_type = params$model_type,
          data_type = data_type
        )
      }
    ),
    pargbqr = list(
      label = "parGBQR",
      run = function(train_data, horizon, quantiles_needed, seasonality, params = list()) {
        params <- utils::modifyList(settings$pargbqr, params)
        fit_process_pargbqr(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          num_bags = params$num_bags,
          bag_frac_samples = 0.7,
          nrounds = params$nrounds,
          num_leaves = params$num_leaves,
          seasonality = seasonality,
          model_type = params$model_type,
          data_type = data_type
        )
      }
    ),
    fourcat = list(
      label = "FourCAT",
      run = function(train_data, horizon, quantiles_needed, seasonality, params = list()) {
        fit_process_fourcat(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          zone = seasonality,
          data_type = data_type
        )
      }
    ),
    starima = list(
      label = "STArima",
      run = function(train_data, horizon, quantiles_needed, seasonality, params = list()) {
        fit_process_starima(
          clean_data = train_data,
          fcast_horizon = horizon,
          quantiles_needed = quantiles_needed,
          origin_date = max(as.Date(train_data$date), na.rm = TRUE) + 7L,
          lambda_data = NULL,
          data_type = data_type
        )
      }
    )
  )
}

write_retrospective_zip <- function(output_dir) {
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  zip_path <- paste0(output_dir, ".zip")
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dir.create(dirname(zip_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(zip_path)) {
    unlink(zip_path)
  }
  setwd(output_dir)
  utils::zip(zipfile = zip_path, files = list.files(".", recursive = TRUE), flags = "-q")
  zip_path
}

call_retrospective_runner <- function(runner,
                                      train_data,
                                      horizon,
                                      quantiles_needed,
                                      seasonality,
                                      params) {
  runner_args <- list(
    train_data = train_data,
    horizon = horizon,
    quantiles_needed = quantiles_needed,
    seasonality = seasonality
  )

  runner_formals <- names(formals(runner$run))
  if ("params" %in% runner_formals || "..." %in% runner_formals) {
    runner_args$params <- params
  }

  do.call(runner$run, runner_args)
}

#' Run retrospective forecasts for a single, already-filtered dataset.
#'
#' This is the original (pre-grouping) retrospective engine: it fits every
#' selected model/config across every reference date using the *entire*
#' `data` argument at once. Every retrospective model implementation
#' internally groups by `target_group` only, so `data` must contain rows for
#' a single location/group -- see `run_retrospective_forecasts()` below for
#' the wrapper that splits a multi-group upload before calling this.
run_retrospective_forecasts_single <- function(data,
                                        reference_dates,
                                        models = NULL,
                                        horizon,
                                        seasonality,
                                        quantiles_needed,
                                        output_dir,
                                        run_configs = NULL,
                                        ensemble_models = NULL,
                                        auto_ensemble = NULL,
                                        ensemble_method = "median",
                                        reference_model = NULL,
                                        runners = NULL,
                                        progress_callback = NULL,
                                        write_zip = TRUE,
                                        neighbor_graph = NULL,
                                        season_groups = NULL,
                                        data_type = "count") {
  legacy_model_selection <- is.null(run_configs)
  if (is.null(auto_ensemble)) {
    auto_ensemble <- legacy_model_selection
  }

  data <- data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::arrange(date)
  reference_dates <- sort(as.Date(reference_dates))
  horizon <- as.integer(horizon)

  if (length(reference_dates) == 0) {
    stop("Select at least one retrospective reference week.")
  }
  if (length(horizon) != 1 || is.na(horizon) || horizon < 1) {
    stop("Forecast horizon must be a single positive integer.")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  settings <- retrospective_default_settings(has_population = "population" %in% names(data))
  if (is.null(runners)) {
    runners <- retrospective_model_runners(
      settings,
      data_type = data_type,
      neighbor_graph = neighbor_graph,
      season_groups = season_groups
    )
  }

  if (is.null(run_configs)) {
    if (length(models) == 0) {
      stop("Select at least one model.", call. = FALSE)
    }
    run_configs <- purrr::map_dfr(models, function(model_id) {
      runner_label <- if (!is.null(runners[[model_id]]$label)) {
        runners[[model_id]]$label
      } else {
        retrospective_model_label(model_id)
      }

      tibble::tibble(
        run_id = model_id,
        model_id = model_id,
        model_label = runner_label,
        run_label = runner_label,
        params = list(retrospective_null_coalesce(settings[[model_id]], list()))
      )
    })
  } else {
    run_configs <- retrospective_validate_run_configs(
      run_configs,
      has_population = "population" %in% names(data)
    )
  }

  unknown_models <- setdiff(run_configs$model_id, names(runners))
  if (length(unknown_models) > 0) {
    stop("Unknown retrospective model(s): ", paste(unknown_models, collapse = ", "))
  }

  success_rows <- list()
  failure_rows <- list()
  file_rows <- list()
  forecast_rows <- list()

  for (reference_date_index in seq_along(reference_dates)) {
    reference_date <- reference_dates[[reference_date_index]]
    if (is.function(progress_callback)) {
      progress_callback(reference_date_index, length(reference_dates), reference_date)
    }

    train_data <- data |>
      dplyr::filter(date < reference_date)
    weekly_results <- list()

    for (config_index in seq_len(nrow(run_configs))) {
      run_config <- run_configs[config_index, ]
      model_id <- run_config$model_id[[1]]
      runner <- runners[[model_id]]
      run_label <- run_config$run_label[[1]]
      run_id <- run_config$run_id[[1]]
      params <- run_config$params[[1]]

      result <- tryCatch(
        {
          raw_forecast <- call_retrospective_runner(
            runner = runner,
            train_data = train_data,
            horizon = horizon,
            quantiles_needed = quantiles_needed,
            seasonality = seasonality,
            params = params
          )

          formatted <- format_retrospective_forecasts(
            forecast_df = raw_forecast,
            model_name = run_label,
            reference_date = reference_date,
            data_type = data_type
          )

          list(ok = TRUE, data = formatted, message = "")
        },
        error = function(e) {
          list(ok = FALSE, data = NULL, message = conditionMessage(e))
        }
      )

      if (isTRUE(result$ok)) {
        weekly_results[[run_id]] <- result$data
        success_rows[[length(success_rows) + 1]] <- tibble::tibble(
          reference_date = reference_date,
          run_id = run_id,
          model_id = model_id,
          model = run_label,
          rows = nrow(result$data)
        )
      } else {
        failure_rows[[length(failure_rows) + 1]] <- tibble::tibble(
          reference_date = reference_date,
          run_id = run_id,
          model_id = model_id,
          model = run_label,
          message = result$message
        )
      }
    }

    weekly_output <- dplyr::bind_rows(weekly_results)
    ensemble_result <- if (isTRUE(auto_ensemble)) {
      build_retrospective_nonbaseline_ensemble(weekly_results, ensemble_method = ensemble_method, data_type = data_type)
    } else {
      build_retrospective_ensemble(
        weekly_output,
        ensemble_members = ensemble_models,
        method = ensemble_method,
        data_type = data_type
      )
    }
    if (!is.null(ensemble_result) && nrow(ensemble_result) > 0) {
      weekly_results[["ensemble"]] <- ensemble_result
      success_rows[[length(success_rows) + 1]] <- tibble::tibble(
        reference_date = reference_date,
        run_id = "ensemble",
        model_id = "ensemble",
        model = "Ensemble",
        rows = nrow(ensemble_result)
      )
    }

    weekly_output <- dplyr::bind_rows(weekly_results)
    if (nrow(weekly_output) > 0) {
      forecast_rows[[length(forecast_rows) + 1]] <- weekly_output

      csv_path <- file.path(
        output_dir,
        paste0("retrospective_", format(reference_date, "%Y-%m-%d"), ".csv")
      )
      readr::write_csv(weekly_output, csv_path)
      file_rows[[length(file_rows) + 1]] <- tibble::tibble(
        reference_date = reference_date,
        file = csv_path,
        rows = nrow(weekly_output)
      )
    }
  }

  failures <- dplyr::bind_rows(failure_rows)
  successes <- dplyr::bind_rows(success_rows)
  files <- dplyr::bind_rows(file_rows)
  forecasts <- dplyr::bind_rows(forecast_rows)
  if (ncol(successes) == 0) {
    successes <- tibble::tibble(
      reference_date = as.Date(character()),
      run_id = character(),
      model_id = character(),
      model = character(),
      rows = integer()
    )
  }
  if (ncol(failures) == 0) {
    failures <- tibble::tibble(
      reference_date = as.Date(character()),
      run_id = character(),
      model_id = character(),
      model = character(),
      message = character()
    )
  }
  if (ncol(files) == 0) {
    files <- tibble::tibble(
      reference_date = as.Date(character()),
      file = character(),
      rows = integer()
    )
  }
  if (ncol(forecasts) == 0) {
    forecasts <- tibble::tibble(
      model = character(),
      reference_date = as.Date(character()),
      horizon = integer(),
      target_end_date = as.Date(character()),
      target_group = character(),
      output_type = character(),
      output_type_id = character(),
      value = numeric()
    )
  }

  if (nrow(failures) > 0) {
    readr::write_csv(failures, file.path(output_dir, "retrospective_failures.csv"))
  }
  if (nrow(successes) > 0) {
    readr::write_csv(successes, file.path(output_dir, "retrospective_successes.csv"))
  }

  config_metadata <- retrospective_run_config_metadata(run_configs)
  readr::write_csv(config_metadata, file.path(output_dir, "retrospective_run_configs.csv"))

  ensemble_metadata <- tibble::tibble(
    model = retrospective_null_coalesce(ensemble_models, character())
  )
  if (nrow(ensemble_metadata) > 0) {
    readr::write_csv(ensemble_metadata, file.path(output_dir, "retrospective_ensemble_members.csv"))
  }

  scoring_reference <- retrospective_resolve_reference_model(
    forecasts = forecasts,
    run_configs = run_configs,
    requested_reference_model = reference_model
  )
  scores <- score_retrospective_forecasts(
    forecasts,
    data,
    reference_model = scoring_reference
  )

  if (nrow(scores$rows) > 0) {
    readr::write_csv(scores$rows, file.path(output_dir, "retrospective_score_rows.csv"))
  }
  if (nrow(scores$overall) > 0) {
    readr::write_csv(scores$overall, file.path(output_dir, "retrospective_score_overall.csv"))
  }
  if (nrow(scores$by_target_group) > 0) {
    readr::write_csv(scores$by_target_group, file.path(output_dir, "retrospective_score_by_target_group.csv"))
  }
  if (nrow(scores$by_forecast_date) > 0) {
    readr::write_csv(scores$by_forecast_date, file.path(output_dir, "retrospective_score_by_forecast_date.csv"))
  }

  write_retrospective_analysis_metadata(
    output_dir,
    scoring_reference = scoring_reference,
    ensemble_models = retrospective_null_coalesce(ensemble_models, character()),
    ensemble_method = ensemble_method
  )

  zip_path <- if (isTRUE(write_zip)) write_retrospective_zip(output_dir) else NULL

  list(
    output_dir = output_dir,
    zip_path = zip_path,
    files = files,
    forecasts = forecasts,
    scores = scores,
    run_configs = run_configs,
    ensemble_models = retrospective_null_coalesce(ensemble_models, character()),
    ensemble_method = ensemble_method,
    scoring_reference = scoring_reference,
    successes = successes,
    failures = failures
  )
}

#' Run retrospective forecasts, splitting by an optional grouping column.
#'
#' When `data` contains a `retrospective_group` column (added by the upload
#' validation as an optional extra column, e.g. "country"), each distinct
#' group value is forecast completely independently: `data` is filtered down
#' to that group's rows (with the grouping column dropped) and run through
#' [run_retrospective_forecasts_single()] exactly as a single-country upload
#' would be. This keeps every model, the ensemble step, and scoring fully
#' isolated per group -- none of them ever see another group's rows -- since
#' every model implementation and the ensemble/scoring helpers key only on
#' `target_group`, and would otherwise silently mix unrelated series that
#' happen to share a target_group name and dates.
#'
#' When `data` has no `retrospective_group` column, this is a thin pass
#' through to [run_retrospective_forecasts_single()] with no behavior change.
#'
#' @param group_col Name of the optional grouping column. Defaults to
#'   `"retrospective_group"`.
#' @param group_seasonality Optional named list/character vector mapping
#'   group value -> seasonality zone (e.g. `c(Argentina = "C", Brazil =
#'   "B")`). A group missing from this map falls back to the top-level
#'   `seasonality` argument.
#' @param ... All other arguments are passed through to
#'   [run_retrospective_forecasts_single()] for every group.
run_retrospective_forecasts <- function(data,
                                        reference_dates,
                                        models = NULL,
                                        horizon,
                                        seasonality,
                                        quantiles_needed,
                                        output_dir,
                                        run_configs = NULL,
                                        ensemble_models = NULL,
                                        auto_ensemble = NULL,
                                        ensemble_method = "median",
                                        reference_model = NULL,
                                        runners = NULL,
                                        progress_callback = NULL,
                                        group_col = "retrospective_group",
                                        group_seasonality = NULL,
                                        group_data_type = NULL,
                                        neighbor_graph = NULL,
                                        season_groups = NULL,
                                        data_type = "count",
                                        run_name = NULL) {
  # Saved before any group-splitting, alongside the source data, so a loaded run
  # carries the graph it was actually fit with.
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write_retrospective_neighbor_graph(output_dir, neighbor_graph)
  write_retrospective_season_groups(output_dir, season_groups)
  has_groups <- !is.null(group_col) &&
    group_col %in% names(data) &&
    dplyr::n_distinct(data[[group_col]], na.rm = TRUE) > 0

  # A single uploaded neighbor graph cannot describe several retrospective
  # groups at once -- each one has its own target groups, so the graph would
  # cover at most one of them and every other group would silently fall back.
  # Refuse up front rather than produce a run whose rows are labelled
  # "besagproper" but were mostly fit as "exchangeable".
  if (has_groups && !is.null(run_configs) && nrow(run_configs) > 0) {
    wants_spatial <- vapply(
      run_configs$params,
      function(p) identical(p$interaction, "besagproper"),
      logical(1)
    )
    if (any(wants_spatial)) {
      stop(
        "The spatial (neighbor graph) structure is not supported for ",
        "retrospective runs that use a retrospective_group column, because one ",
        "neighbor graph cannot describe several groups' target groups at once. ",
        "Remove the spatial configuration(s), or run each group separately.",
        call. = FALSE
      )
    }
  }

  # Persisted once, up front, for a later "Load Previous Run" to read back --
  # see load_retrospective_run(). Must happen here (rather than inside
  # run_retrospective_forecasts_single()) because this is the only place
  # that ever sees BOTH the full, not-yet-group-split `data` and the
  # top-level `output_dir` -- run_retrospective_forecasts_single() is called
  # once per group, each time with only that group's own slice. Unlike
  # seasonality/group_col/data_type, `run_name` is a whole-run label, never
  # per-group, so it's always written as-is regardless of has_groups.
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write_retrospective_source_data(output_dir, data)
  write_retrospective_run_settings(
    output_dir,
    horizon = horizon,
    seasonality = if (has_groups) NA_character_ else seasonality,
    group_col = if (has_groups) group_col else NA_character_,
    data_type = if (has_groups) NA_character_ else data_type,
    run_name = run_name
  )

  if (!has_groups) {
    return(run_retrospective_forecasts_single(
      neighbor_graph = neighbor_graph,
      season_groups = season_groups,
      data = data,
      reference_dates = reference_dates,
      models = models,
      horizon = horizon,
      seasonality = seasonality,
      quantiles_needed = quantiles_needed,
      output_dir = output_dir,
      run_configs = run_configs,
      ensemble_models = ensemble_models,
      auto_ensemble = auto_ensemble,
      ensemble_method = ensemble_method,
      reference_model = reference_model,
      runners = runners,
      progress_callback = progress_callback,
      write_zip = TRUE,
      data_type = data_type
    ))
  }

  group_values <- data |>
    dplyr::filter(!is.na(.data[[group_col]])) |>
    dplyr::distinct(.data[[group_col]]) |>
    dplyr::arrange(.data[[group_col]]) |>
    dplyr::pull(1) |>
    as.character()

  # The Shiny upload validator (validate_data()) normally guarantees every
  # group covers the same set of observed weeks before this function is ever
  # called -- the reference_dates argument below is shared across every
  # group's run, so a group missing some of those weeks would otherwise
  # silently fit models against training windows it doesn't actually have
  # data for. Check independently here too, since this function can be
  # (and already is, e.g. by retrospective batch scripts) called directly,
  # bypassing that validator.
  assert_retrospective_groups_share_week_coverage(data, group_col, group_values)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  group_folder_names <- make_unique_retrospective_group_folder_names(group_values)

  # Resolve once, up front, so the persisted manifest and every group's
  # actual run always agree on which zone was used.
  group_seasonality_resolved <- stats::setNames(
    vapply(group_values, function(group_value) {
      if (!is.null(group_seasonality) && group_value %in% names(group_seasonality)) {
        as.character(group_seasonality[[group_value]])
      } else {
        as.character(seasonality)
      }
    }, character(1)),
    group_values
  )
  # Same resolution pattern as seasonality above: a group with no explicit
  # entry in group_data_type falls back to the run-level `data_type` default
  # ("count" unless the caller says otherwise).
  group_data_type_resolved <- stats::setNames(
    vapply(group_values, function(group_value) {
      if (!is.null(group_data_type) && group_value %in% names(group_data_type)) {
        as.character(group_data_type[[group_value]])
      } else {
        as.character(data_type)
      }
    }, character(1)),
    group_values
  )
  write_retrospective_group_manifest(output_dir, group_values, group_folder_names, group_seasonality_resolved, group_data_type_resolved)

  group_results <- purrr::map(group_values, function(group_value) {
    group_data <- data |>
      dplyr::filter(as.character(.data[[group_col]]) == group_value) |>
      dplyr::select(-dplyr::all_of(group_col))

    group_seasonality_value <- group_seasonality_resolved[[group_value]]
    group_data_type_value <- group_data_type_resolved[[group_value]]

    group_progress <- if (is.function(progress_callback)) {
      function(reference_date_index, total_reference_dates, reference_date) {
        progress_callback(
          reference_date_index,
          total_reference_dates,
          reference_date,
          group = group_value,
          group_index = match(group_value, group_values),
          total_groups = length(group_values)
        )
      }
    } else {
      NULL
    }

    # A structural failure for one group (e.g. too little data before the
    # earliest reference week) must not take down every other group's
    # results -- per-model-per-date failures are already isolated inside
    # run_retrospective_forecasts_single(), but a hard stop() there (as
    # opposed to a per-model error) would otherwise propagate all the way
    # up and abort the whole multi-group run.
    tryCatch(
      run_retrospective_forecasts_single(
        neighbor_graph = neighbor_graph,
        season_groups = season_groups,
        data = group_data,
        reference_dates = reference_dates,
        models = models,
        horizon = horizon,
        seasonality = group_seasonality_value,
        quantiles_needed = quantiles_needed,
        output_dir = file.path(output_dir, group_folder_names[[group_value]]),
        run_configs = run_configs,
        ensemble_models = ensemble_models,
        auto_ensemble = auto_ensemble,
        ensemble_method = ensemble_method,
        reference_model = reference_model,
        runners = runners,
        progress_callback = group_progress,
        write_zip = FALSE,
        data_type = group_data_type_value
      ),
      error = function(e) {
        retrospective_group_failure_result(
          group_value = group_value,
          error_message = conditionMessage(e),
          run_configs = run_configs,
          ensemble_models = ensemble_models,
          ensemble_method = ensemble_method
        )
      }
    )
  })
  names(group_results) <- group_values

  combine_retrospective_group_results(group_results, output_dir = output_dir, group_col = group_col)
}

#' Defensive, engine-level version of the identical-week-coverage check that
#' validate_data() already runs at upload time. Stops with a clear error
#' (rather than silently forecasting a group over training windows it
#' doesn't have data for) if any group's own set of observed weeks differs
#' from another's.
assert_retrospective_groups_share_week_coverage <- function(data, group_col, group_values) {
  if (length(group_values) < 2) {
    return(invisible(NULL))
  }

  group_date_sets <- purrr::map(group_values, function(group_value) {
    data |>
      dplyr::filter(as.character(.data[[group_col]]) == group_value) |>
      dplyr::pull(date) |>
      as.Date() |>
      unique() |>
      sort()
  })
  names(group_date_sets) <- group_values

  reference_group <- group_values[[1]]
  reference_dates_set <- group_date_sets[[reference_group]]
  mismatched <- group_values[
    vapply(group_date_sets, function(x) !identical(x, reference_dates_set), logical(1))
  ]

  if (length(mismatched) > 0) {
    stop(
      "Every value of '", group_col, "' must cover the same set of observed weeks to run a ",
      "retrospective forecast together (reference_dates is shared across every group). ",
      "Group(s) with different week coverage than '", reference_group, "': ",
      paste(mismatched, collapse = ", "), ".",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Turn an arbitrary group value into a safe folder/file name component.
sanitize_retrospective_group_name <- function(group_value) {
  cleaned <- gsub("[^A-Za-z0-9 _.-]+", "_", trimws(as.character(group_value)))
  if (!nzchar(cleaned)) "group" else cleaned
}

#' Turn the user's free-text "Run Name" into a short, filesystem-safe slug:
#' unlike sanitize_retrospective_group_name() (which always returns SOME
#' name, falling back to "group"), an empty/NA/whitespace-only run_name is
#' meaningful here -- "the user didn't name this run" -- and returns "",
#' letting retrospective_run_folder_stamp() fall back to a bare timestamp
#' exactly like before this feature existed. Spaces become "-" (folder names
#' with literal spaces are easy to mistype/misquote on the command line);
#' capped at 40 characters so one long name can't make the folder name
#' unwieldy.
retrospective_sanitize_run_name <- function(run_name) {
  if (is.null(run_name) || length(run_name) == 0 || is.na(run_name)) return("")
  trimmed <- trimws(as.character(run_name))
  if (!nzchar(trimmed)) return("")
  cleaned <- gsub("[^A-Za-z0-9 _.-]+", "", trimmed)
  cleaned <- gsub("\\s+", "-", cleaned)
  substr(cleaned, 1, 40)
}

#' Build the on-disk folder name for a brand-new retrospective run: the
#' user's own sanitized run_name (if any), prefixed onto the same
#' timestamp-plus-random-suffix scheme this app always used, so a run
#' someone bothered to name is immediately recognizable in the "Load
#' Previous Run" folder browser instead of showing up as an opaque digit
#' string. `now`/`random_suffix` are only ever overridden by tests -- real
#' callers just pass `run_name`.
retrospective_run_folder_stamp <- function(run_name = NULL, now = Sys.time(), random_suffix = NULL) {
  slug <- retrospective_sanitize_run_name(run_name)
  suffix <- retrospective_null_coalesce(random_suffix, sprintf("%04d", sample.int(9999L, 1L)))
  timestamp <- gsub("[^0-9]", "", format(now, "%Y%m%d%H%M%OS3"))
  if (nzchar(slug)) paste0(slug, "__", timestamp, "-", suffix) else paste0(timestamp, "-", suffix)
}

#' Map every distinct group value to a sanitized, GUARANTEED-unique folder
#' name. Two different group values can sanitize to the same string (e.g.
#' punctuation-only differences); when that happens, later duplicates get a
#' "__2", "__3", ... suffix so they never share -- and silently overwrite --
#' the same on-disk subfolder.
make_unique_retrospective_group_folder_names <- function(group_values) {
  used <- character()
  result <- character(length(group_values))
  names(result) <- group_values

  for (group_value in group_values) {
    base_name <- sanitize_retrospective_group_name(group_value)
    candidate <- base_name
    suffix <- 2L
    while (candidate %in% used) {
      candidate <- paste0(base_name, "__", suffix)
      suffix <- suffix + 1L
    }
    used <- c(used, candidate)
    result[[group_value]] <- candidate
  }

  result
}

#' Build a result for one group when its run_retrospective_forecasts_single()
#' call threw a hard error, so combine_retrospective_group_results() can
#' still bind it in alongside every other (successful) group. Shaped
#' identically to a real result, just empty, plus one row in `failures`
#' describing what went wrong -- it shows up in the UI exactly where a
#' per-model failure would, scoped to this group.
retrospective_group_failure_result <- function(group_value,
                                               error_message,
                                               run_configs,
                                               ensemble_models,
                                               ensemble_method) {
  empty_forecasts <- tibble::tibble(
    model = character(),
    reference_date = as.Date(character()),
    horizon = integer(),
    target_end_date = as.Date(character()),
    target_group = character(),
    output_type = character(),
    output_type_id = character(),
    value = numeric()
  )
  empty_score_rows <- tibble::tibble(
    model = character(),
    reference_date = as.Date(character()),
    horizon = integer(),
    target_end_date = as.Date(character()),
    target_group = character(),
    actual = numeric(),
    wis = numeric(),
    relative_wis = numeric(),
    log_wis = numeric(),
    relative_log_wis = numeric(),
    weighted_interval_score_50 = numeric(),
    covered_50 = logical(),
    weighted_interval_score_95 = numeric(),
    covered_95 = logical()
  )

  list(
    output_dir = NULL,
    zip_path = NULL,
    files = tibble::tibble(reference_date = as.Date(character()), file = character(), rows = integer()),
    forecasts = empty_forecasts,
    scores = list(
      rows = empty_score_rows,
      overall = tibble::tibble(),
      by_target_group = tibble::tibble(),
      by_forecast_date = tibble::tibble()
    ),
    run_configs = run_configs,
    ensemble_models = retrospective_null_coalesce(ensemble_models, character()),
    ensemble_method = ensemble_method,
    scoring_reference = NA_character_,
    successes = tibble::tibble(
      reference_date = as.Date(character()), run_id = character(),
      model_id = character(), model = character(), rows = integer()
    ),
    failures = tibble::tibble(
      reference_date = as.Date(NA),
      run_id = NA_character_,
      model_id = NA_character_,
      model = paste0("(group '", group_value, "' failed)"),
      message = error_message
    ),
    group_failed = TRUE
  )
}

#' Combine the per-group results of [run_retrospective_forecasts()] into one
#' result list, tagging every row with the group value, writing combined
#' roll-up CSVs at `output_dir`, and zipping the whole thing (per-group
#' subfolders included) into a single downloadable archive.
combine_retrospective_group_results <- function(group_results, output_dir, group_col) {
  bind_group_field <- function(accessor) {
    purrr::map(group_results, accessor) |>
      dplyr::bind_rows(.id = group_col)
  }

  forecasts <- bind_group_field(function(x) x$forecasts)
  successes <- bind_group_field(function(x) x$successes)
  failures <- bind_group_field(function(x) x$failures)
  files <- bind_group_field(function(x) x$files)

  scores <- list(
    rows = bind_group_field(function(x) x$scores$rows),
    overall = bind_group_field(function(x) x$scores$overall),
    by_target_group = bind_group_field(function(x) x$scores$by_target_group),
    by_forecast_date = bind_group_field(function(x) x$scores$by_forecast_date)
  )

  # The run configuration (models/params) and ensemble definition are shared
  # across every group by construction (the same request drives every
  # group's run) -- including a group whose run_retrospective_forecasts_
  # single() call hard-failed, since retrospective_group_failure_result()
  # echoes back the very same run_configs/ensemble_models/ensemble_method
  # arguments a successful run would have used. So group_results[[1]] is
  # safe to read these from even if group 1 specifically failed. Only the
  # resolved scoring reference can legitimately differ per group, e.g. if a
  # model failed for one group but not another.
  run_configs <- group_results[[1]]$run_configs
  ensemble_models <- group_results[[1]]$ensemble_models
  ensemble_method <- group_results[[1]]$ensemble_method
  scoring_reference <- purrr::map_chr(group_results, function(x) {
    retrospective_null_coalesce(x$scoring_reference, NA_character_)
  })

  if (nrow(failures) > 0) {
    readr::write_csv(failures, file.path(output_dir, "retrospective_failures.csv"))
  }
  if (nrow(successes) > 0) {
    readr::write_csv(successes, file.path(output_dir, "retrospective_successes.csv"))
  }
  readr::write_csv(
    retrospective_run_config_metadata(run_configs),
    file.path(output_dir, "retrospective_run_configs.csv")
  )
  write_retrospective_group_scoring_reference(output_dir, scoring_reference, group_col)
  if (nrow(scores$rows) > 0) {
    readr::write_csv(scores$rows, file.path(output_dir, "retrospective_score_rows.csv"))
  }
  if (nrow(scores$overall) > 0) {
    readr::write_csv(scores$overall, file.path(output_dir, "retrospective_score_overall.csv"))
  }
  if (nrow(scores$by_target_group) > 0) {
    readr::write_csv(scores$by_target_group, file.path(output_dir, "retrospective_score_by_target_group.csv"))
  }
  if (nrow(scores$by_forecast_date) > 0) {
    readr::write_csv(scores$by_forecast_date, file.path(output_dir, "retrospective_score_by_forecast_date.csv"))
  }
  write_retrospective_analysis_metadata(
    output_dir,
    scoring_reference = paste(unique(stats::na.omit(scoring_reference)), collapse = "; "),
    ensemble_models = retrospective_null_coalesce(ensemble_models, character()),
    ensemble_method = ensemble_method
  )

  zip_path <- write_retrospective_zip(output_dir)

  list(
    output_dir = output_dir,
    zip_path = zip_path,
    files = files,
    forecasts = forecasts,
    scores = scores,
    run_configs = run_configs,
    ensemble_models = retrospective_null_coalesce(ensemble_models, character()),
    ensemble_method = ensemble_method,
    scoring_reference = scoring_reference,
    successes = successes,
    failures = failures,
    group_col = group_col,
    groups = names(group_results)
  )
}

#' Replace one group's rows within a combined (group_col-tagged) tibble with
#' new rows for that same group, leaving every other group's rows untouched.
#' Used by the interactive scoring-reference / ensemble-rebuild handlers so
#' each call only ever touches one group's slice.
#'
#' `new_rows` is always exactly one group's replacement slice by contract,
#' so `group_col` is unconditionally forced to `group_value` on every row --
#' not just filled in when the column is missing entirely. Some callers
#' (e.g. rebuilding an Ensemble via build_retrospective_ensemble()) hand in
#' `new_rows` built by `dplyr::bind_rows()`-ing group-tagged individual-model
#' rows together with ensemble rows that never carried `group_col` at all
#' (build_ensemble() only ever returns the fixed hubverse forecast columns);
#' bind_rows() then leaves `group_col` NA for just the ensemble rows rather
#' than dropping the column, so the old "only fill in when the column is
#' completely absent" check silently let those NAs through -- the Ensemble
#' would appear to vanish from that group's view (filtered out by
#' group_scoped()) immediately after being rebuilt. Forcing the column
#' unconditionally fixes that for every column-shape new_rows can arrive in.
#'
#' When `combined` has no `group_col` column at all (an ungrouped run), this
#' just returns `new_rows` unchanged -- there is only ever one "group" and no
#' splicing is needed.
retrospective_replace_group_rows <- function(combined, group_col, group_value, new_rows) {
  if (is.null(group_col) || is.null(group_value) || is.null(combined) || !group_col %in% names(combined)) {
    return(new_rows)
  }

  kept <- combined |> dplyr::filter(.data[[group_col]] != group_value)

  if (!is.null(new_rows) && nrow(new_rows) > 0) {
    new_rows[[group_col]] <- group_value
    new_rows <- new_rows |> dplyr::relocate(dplyr::all_of(group_col))
  }

  dplyr::bind_rows(kept, new_rows)
}

#' Resolve the on-disk folder name for each group being rewritten by
#' [rewrite_retrospective_output_files()]. MUST reuse the exact
#' (collision-disambiguated) folder names [make_unique_retrospective_group_folder_names()]
#' picked at initial-run time -- recomputing [sanitize_retrospective_group_name()]
#' fresh here, with no memory of which sanitized names were already claimed
#' by an earlier group, is what let two groups that sanitize to the same
#' string (e.g. "Cote d'Ivoire" and "Cote d Ivoire") silently overwrite each
#' other's files on every interactive update (add model / change scoring
#' reference / rebuild ensemble). Falls back to recomputing only if the
#' manifest is missing or doesn't cover every current group -- should not
#' happen in practice, since the set of groups never changes after the
#' initial run, but this keeps a rewrite from hard-failing if it ever does.
#' `col_types` forces both columns to character on read so a numeric-looking
#' group value (e.g. a location code) matches the character group values
#' `result$forecasts[[group_col]]` always carries, the same way
#' `output_type_id` is already pinned to character elsewhere.
retrospective_group_folder_names_for_rewrite <- function(output_dir, group_values) {
  manifest_path <- file.path(output_dir, "retrospective_group_folders.csv")
  if (file.exists(manifest_path)) {
    manifest <- readr::read_csv(
      manifest_path,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )
    folder_map <- stats::setNames(manifest$folder, manifest$group)
    if (all(group_values %in% names(folder_map))) {
      return(folder_map[group_values])
    }
  }
  make_unique_retrospective_group_folder_names(group_values)
}

#' Rewrite a retrospective result's on-disk CSVs (per-group weekly forecast
#' files plus the combined roll-up CSVs) and zip, after an interactive
#' change (scoring reference, ensemble rebuild) has mutated `result` in
#' place. Safe to call whether or not the result is grouped.
rewrite_retrospective_output_files <- function(result) {
  output_dir <- result$output_dir
  if (is.null(output_dir) || !dir.exists(output_dir)) {
    return(result)
  }

  write_weekly_files <- function(forecasts_slice, dir) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    reference_dates <- forecasts_slice |>
      dplyr::distinct(reference_date) |>
      dplyr::pull(reference_date)

    for (reference_date_value in reference_dates) {
      reference_date_value <- as.Date(reference_date_value, origin = "1970-01-01")
      weekly <- forecasts_slice |>
        dplyr::filter(.data$reference_date == .env$reference_date_value)
      readr::write_csv(
        weekly,
        file.path(dir, paste0("retrospective_", format(reference_date_value, "%Y-%m-%d"), ".csv"))
      )
    }
  }

  group_col <- result$group_col
  if (!is.null(group_col) && group_col %in% names(result$forecasts)) {
    group_values <- unique(as.character(result$forecasts[[group_col]]))
    group_folder_names <- retrospective_group_folder_names_for_rewrite(output_dir, group_values)
    for (group_value in group_values) {
      slice <- result$forecasts |>
        dplyr::filter(as.character(.data[[group_col]]) == group_value) |>
        dplyr::select(-dplyr::all_of(group_col))
      write_weekly_files(slice, file.path(output_dir, group_folder_names[[group_value]]))
    }
  } else {
    write_weekly_files(result$forecasts, output_dir)
  }

  # Keep the on-disk config listing and failure log in sync with `result` --
  # both can legitimately change out from under the original run (a model
  # added or removed via add_retrospective_run_configs() below), so they're
  # rewritten unconditionally here rather than only at initial-run time.
  readr::write_csv(
    retrospective_run_config_metadata(result$run_configs),
    file.path(output_dir, "retrospective_run_configs.csv")
  )
  failures_path <- file.path(output_dir, "retrospective_failures.csv")
  if (!is.null(result$failures) && nrow(result$failures) > 0) {
    readr::write_csv(result$failures, failures_path)
  } else if (file.exists(failures_path)) {
    unlink(failures_path)
  }
  successes_path <- file.path(output_dir, "retrospective_successes.csv")
  if (!is.null(result$successes) && nrow(result$successes) > 0) {
    readr::write_csv(result$successes, successes_path)
  } else if (file.exists(successes_path)) {
    unlink(successes_path)
  }
  if (!is.null(group_col) && group_col %in% names(result$forecasts)) {
    write_retrospective_group_scoring_reference(output_dir, result$scoring_reference, group_col)
  }

  scores <- result$scores
  if (nrow(scores$rows) > 0) {
    readr::write_csv(scores$rows, file.path(output_dir, "retrospective_score_rows.csv"))
  }
  if (nrow(scores$overall) > 0) {
    readr::write_csv(scores$overall, file.path(output_dir, "retrospective_score_overall.csv"))
  }
  if (nrow(scores$by_target_group) > 0) {
    readr::write_csv(scores$by_target_group, file.path(output_dir, "retrospective_score_by_target_group.csv"))
  }
  if (nrow(scores$by_forecast_date) > 0) {
    readr::write_csv(scores$by_forecast_date, file.path(output_dir, "retrospective_score_by_forecast_date.csv"))
  }
  write_retrospective_analysis_metadata(
    output_dir,
    scoring_reference = paste(unique(stats::na.omit(result$scoring_reference)), collapse = "; "),
    ensemble_models = retrospective_null_coalesce(result$ensemble_models, character()),
    ensemble_method = result$ensemble_method
  )

  result$zip_path <- write_retrospective_zip(output_dir)
  result
}

#' Add newly-configured model(s) to an existing retrospective result without
#' re-running (or losing) anything that already completed -- the engine half
#' of "run an additional model without clearing all the data, as long as
#' it's run on the same time period". The caller (server/retrospective.R) is
#' responsible for deciding *when* this applies: it's only valid when the
#' reference dates / horizon / seasonality haven't changed since
#' `existing_result` was produced (tracked today via the existing
#' `retrospective$result_stale` flag) -- if anything about the run itself
#' changed, a fresh, full [run_retrospective_forecasts()] call is required
#' instead, since every reference date's training window would differ.
#'
#' Only the rows of `run_configs` not already present (by `run_id`) in
#' `existing_result$run_configs` are actually fit; everything else -- other
#' models' forecasts, successes, failures, and any already-built Ensemble --
#' is carried over untouched. A config present in `existing_result` but no
#' longer in `run_configs` (removed by the user before re-running) has its
#' rows dropped from the merged result. Ensemble rows are always carried
#' over as-is: the Ensemble is a derived, separately-managed result (built
#' via the "Run Ensemble" control, see server/retrospective.R), not a
#' `run_configs` row, so adding or removing a model here never rebuilds it --
#' only an explicit ensemble rebuild does that.
#'
#' Scores are always recomputed from scratch on the fully-merged forecasts
#' (never patched incrementally), mirroring the pattern already used by the
#' scoring-reference-change and ensemble-rebuild handlers -- scoring is cheap
#' relative to actually fitting a model, so there's no need to try to reuse
#' old score rows. For a grouped result this happens group by group, since
#' [score_retrospective_forecasts()] itself only ever looks at one location's
#' forecasts against that location's own data (see
#' `run_retrospective_forecasts_single()`), even though the merge above is
#' global (a single "Run Retrospective" click always re-runs every group at
#' once, so there's no notion of a per-group added/removed model to track).
#'
#' Newly-run models are fit into a scratch directory (never directly into
#' `existing_result$output_dir`) because `run_retrospective_forecasts_single()`
#' unconditionally overwrites each reference date's CSV with exactly the
#' models it just ran -- fitting straight into the real output directory
#' would silently wipe the previously-run models' on-disk rows for every
#' date until [rewrite_retrospective_output_files()] ran again. The merged
#' result is persisted back to `existing_result$output_dir` (never a new
#' run/timestamp directory), which is what gives "start a session" its
#' meaning: the zip and folder a user already downloaded keep accumulating
#' in place across multiple "Run Retrospective" clicks instead of each click
#' producing a brand-new, disconnected output.
add_retrospective_run_configs <- function(existing_result,
                                          data,
                                          reference_dates,
                                          horizon,
                                          seasonality,
                                          quantiles_needed,
                                          run_configs,
                                          runners = NULL,
                                          group_col = "retrospective_group",
                                          group_seasonality = NULL,
                                          group_data_type = NULL,
                                          progress_callback = NULL,
                                          neighbor_graph = NULL,
                                          season_groups = NULL,
                                          data_type = "count") {
  has_population <- "population" %in% names(data)
  run_configs <- retrospective_validate_run_configs(run_configs, has_population = has_population)

  existing_configs <- existing_result$run_configs
  existing_run_ids <- if (!is.null(existing_configs) && nrow(existing_configs) > 0) {
    existing_configs$run_id
  } else {
    character()
  }
  new_run_ids <- setdiff(run_configs$run_id, existing_run_ids)
  kept_run_ids <- intersect(run_configs$run_id, existing_run_ids)
  # `existing_result$forecasts$model` was stamped with the OLD run_label at
  # the time those rows were fit (see format_retrospective_forecasts()) --
  # forecasts carry no run_id column of their own. Looking up the kept
  # labels in the NEW `run_configs` (as opposed to `existing_configs`) would
  # silently drop a kept run_id's entire forecast history the moment its
  # label differs between the old and new config table (a rename), even
  # though the row itself is still meant to be retained by run_id. Matching
  # against `existing_configs` here finds the label these rows are actually
  # stored under; any rename is then reconciled below by relabeling the kept
  # rows to the current label, so forecasts/scores/ensemble config all agree
  # on the SAME (new) label going forward.
  old_labels_by_run_id <- stats::setNames(existing_configs$run_label, existing_configs$run_id)
  new_labels_by_run_id <- stats::setNames(run_configs$run_label, run_configs$run_id)
  kept_labels <- unname(old_labels_by_run_id[kept_run_ids])
  # "Ensemble" is never itself a run_configs row/run_id -- always carry its
  # rows over, per the "derived, separately managed" note above.
  keep_model_labels <- c(kept_labels, "Ensemble")
  keep_success_ids <- c(kept_run_ids, "ensemble")

  has_groups <- !is.null(group_col) && group_col %in% names(data) &&
    dplyr::n_distinct(data[[group_col]], na.rm = TRUE) > 0

  if (length(new_run_ids) > 0) {
    new_configs <- run_configs |> dplyr::filter(run_id %in% new_run_ids)
    scratch_dir <- file.path(
      tempdir(),
      paste0(
        "retrospective_add_models_",
        gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS6")), "-",
        sprintf("%04d", sample.int(9999L, 1L))
      )
    )
    on.exit(unlink(scratch_dir, recursive = TRUE, force = TRUE), add = TRUE)

    new_run <- run_retrospective_forecasts(
      data = data,
      reference_dates = reference_dates,
      horizon = horizon,
      seasonality = seasonality,
      quantiles_needed = quantiles_needed,
      output_dir = scratch_dir,
      run_configs = new_configs,
      ensemble_models = NULL,
      auto_ensemble = FALSE,
      runners = runners,
      progress_callback = progress_callback,
      group_col = group_col,
      group_seasonality = group_seasonality,
      group_data_type = group_data_type,
      neighbor_graph = neighbor_graph,
      season_groups = season_groups,
      data_type = data_type
    )

    new_forecasts <- new_run$forecasts
    new_successes <- new_run$successes
    new_failures <- new_run$failures
  } else {
    new_forecasts <- existing_result$forecasts[0, ]
    new_successes <- existing_result$successes[0, ]
    new_failures <- existing_result$failures[0, ]
  }

  kept_forecasts <- existing_result$forecasts |> dplyr::filter(model %in% keep_model_labels)

  # Reconcile a run_label rename for any kept run_id: `kept_forecasts$model`
  # still holds the OLD label above (that's how those rows are found), but
  # `run_configs` (and therefore retrospective_run_config_metadata() /
  # successes / any future ensemble rebuild) will refer to this run_id by
  # its NEW label from here on. Relabel in place so every downstream table
  # agrees on one current name per run_id instead of the forecasts table
  # quietly keeping the stale one.
  renamed_run_ids <- kept_run_ids[
    !is.na(new_labels_by_run_id[kept_run_ids]) &
      new_labels_by_run_id[kept_run_ids] != old_labels_by_run_id[kept_run_ids]
  ]
  if (length(renamed_run_ids) > 0 && nrow(kept_forecasts) > 0) {
    old_to_new <- stats::setNames(
      unname(new_labels_by_run_id[renamed_run_ids]),
      unname(old_labels_by_run_id[renamed_run_ids])
    )
    kept_forecasts <- kept_forecasts |>
      dplyr::mutate(
        model = dplyr::if_else(model %in% names(old_to_new), unname(old_to_new[model]), model)
      )
  }

  merged_forecasts <- dplyr::bind_rows(kept_forecasts, new_forecasts)

  kept_successes <- existing_result$successes |> dplyr::filter(run_id %in% keep_success_ids)
  merged_successes <- dplyr::bind_rows(kept_successes, new_successes)

  # A group-failure marker row (see retrospective_group_failure_result()) has
  # run_id = NA -- keep those regardless, they describe the group itself,
  # not any one model.
  kept_failures <- existing_result$failures |>
    dplyr::filter(is.na(run_id) | run_id %in% keep_success_ids)

  # A structural marker describes the group as of the run that produced it --
  # it does NOT automatically mean the group is still failing. Whenever this
  # call actually re-attempts every group (new_run_ids non-empty), drop any
  # kept marker for a group that no longer has a fresh marker in
  # `new_failures`: that group's run_retrospective_forecasts_single() call
  # didn't throw this time, so it evidently isn't structurally broken
  # anymore. Without this, a group that failed once (e.g. because of a
  # model config that has since been swapped out) would show a permanent
  # "group X failed" banner even after a later incremental run succeeds for
  # it. Left untouched when new_run_ids is empty (only removing/reordering
  # existing models re-runs nothing, so no group's health can be confirmed
  # either way this call).
  if (has_groups && length(new_run_ids) > 0 && group_col %in% names(kept_failures)) {
    new_group_markers <- if (group_col %in% names(new_failures)) {
      new_failures |>
        dplyr::filter(is.na(run_id)) |>
        dplyr::pull(.data[[group_col]]) |>
        as.character()
    } else {
      character()
    }
    all_group_values <- as.character(unique(data[[group_col]][!is.na(data[[group_col]])]))
    stale_marker_groups <- setdiff(all_group_values, new_group_markers)

    kept_failures <- kept_failures |>
      dplyr::filter(
        !(is.na(run_id) & as.character(.data[[group_col]]) %in% stale_marker_groups)
      )
  }

  merged_failures <- dplyr::bind_rows(kept_failures, new_failures)

  existing_scoring_reference <- existing_result$scoring_reference

  if (has_groups) {
    group_values <- data |>
      dplyr::filter(!is.na(.data[[group_col]])) |>
      dplyr::distinct(.data[[group_col]]) |>
      dplyr::arrange(.data[[group_col]]) |>
      dplyr::pull(1) |>
      as.character()

    group_score_parts <- purrr::map(group_values, function(group_value) {
      group_forecasts <- merged_forecasts |>
        dplyr::filter(as.character(.data[[group_col]]) == group_value) |>
        dplyr::select(-dplyr::all_of(group_col))
      group_data <- data |>
        dplyr::filter(as.character(.data[[group_col]]) == group_value) |>
        dplyr::select(-dplyr::all_of(group_col))
      requested <- if (!is.null(names(existing_scoring_reference))) {
        existing_scoring_reference[[group_value]]
      } else {
        existing_scoring_reference
      }
      resolved <- retrospective_resolve_reference_model(
        forecasts = group_forecasts,
        run_configs = run_configs,
        requested_reference_model = requested
      )
      list(
        scores = score_retrospective_forecasts(group_forecasts, group_data, reference_model = resolved),
        scoring_reference = resolved
      )
    })
    names(group_score_parts) <- group_values

    scores <- list(
      rows = dplyr::bind_rows(purrr::map(group_score_parts, function(x) x$scores$rows), .id = group_col),
      overall = dplyr::bind_rows(purrr::map(group_score_parts, function(x) x$scores$overall), .id = group_col),
      by_target_group = dplyr::bind_rows(
        purrr::map(group_score_parts, function(x) x$scores$by_target_group), .id = group_col
      ),
      by_forecast_date = dplyr::bind_rows(
        purrr::map(group_score_parts, function(x) x$scores$by_forecast_date), .id = group_col
      )
    )
    scoring_reference <- purrr::map_chr(
      group_score_parts, function(x) retrospective_null_coalesce(x$scoring_reference, NA_character_)
    )
  } else {
    resolved <- retrospective_resolve_reference_model(
      forecasts = merged_forecasts,
      run_configs = run_configs,
      requested_reference_model = existing_scoring_reference
    )
    scores <- score_retrospective_forecasts(merged_forecasts, data, reference_model = resolved)
    scoring_reference <- resolved
  }

  result <- existing_result
  result$forecasts <- merged_forecasts
  result$successes <- merged_successes
  result$failures <- merged_failures
  result$scores <- scores
  result$scoring_reference <- scoring_reference
  result$run_configs <- run_configs

  rewrite_retrospective_output_files(result)
}
