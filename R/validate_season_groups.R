## Validation logic for a user-uploaded seasonal grouping.
##
## A two-column CSV assigning each target group to a seasonal group. Target
## groups sharing a season group share one estimated seasonal curve:
##
##   target_group,season_group
##   Alaska,AK
##   Hawaii,Tropical
##   Puerto Rico,Tropical
##
## Season group labels are arbitrary strings -- only which groups share a label
## matters, never the label's value or ordering. Any target group NOT listed
## falls into one shared default group, so a file only has to name the
## exceptions: with the example above, all 49 remaining states share one curve.
##
## Why this is declared separately from the neighbor graph: spatial adjacency
## and seasonal regime are different things that merely correlate. Alaska is
## spatially isolated but its seasonal shape is temperate; south Florida borders
## Georgia but is subtropical. Deriving one from the other would make it
## impossible to say "Hawaii and Puerto Rico share a tropical curve" (which
## pools two data-poor series) or to give a connected region its own curve.
##
## Returns a list with:
##   $errors   — named list of blocking error strings (not stored if any exist)
##   $warnings — named list of non-blocking warning strings (still stored)
##   $data     — cleaned assignment data frame (target_group, season_group), or NULL
##
## File reading is split from validation so the logic can be unit-tested without
## a file or a Shiny session. readr::read_csv is used for reading because it
## strips a UTF-8 BOM, which the bundled templates carry.

SEASON_GROUP_DEFAULT_LABEL <- "(shared)"

validate_season_groups <- function(file, target_groups, agg_group = "Overall") {
  df <- tryCatch(
    readr::read_csv(file, show_col_types = FALSE),
    error = function(e) NULL
  )

  if (is.null(df) || nrow(df) == 0) {
    return(list(
      errors = list(
        read = "Could not read the uploaded file, or the file is empty. Ensure it is a valid CSV."
      ),
      warnings = list(),
      data = NULL
    ))
  }

  validate_season_groups_df(
    df = as.data.frame(df, stringsAsFactors = FALSE),
    target_groups = target_groups,
    agg_group = agg_group
  )
}


## All validation logic. Takes an already-read data frame. Base R only.
validate_season_groups_df <- function(df, target_groups, agg_group = "Overall") {
  error_list   <- list()
  warning_list <- list()

  fail <- function(errors) {
    list(errors = errors, warnings = warning_list, data = NULL)
  }

  # ── Check 1: Required columns ─────────────────────────────────────────────
  req_cols     <- c("target_group", "season_group")
  missing_cols <- setdiff(req_cols, colnames(df))
  if (length(missing_cols) > 0) {
    error_list$cols <- paste0(
      "Missing required column(s): ", paste(missing_cols, collapse = ", "),
      ". The seasonal grouping file needs exactly two columns, 'target_group' ",
      "and 'season_group'. Download the template to see the expected format."
    )
    return(fail(error_list))
  }

  df$target_group <- trimws(as.character(df$target_group))
  df$season_group <- trimws(as.character(df$season_group))

  # ── Check 2: No blank or missing values ───────────────────────────────────
  blank_rows <- which(
    is.na(df$target_group) | is.na(df$season_group) |
      df$target_group == "" | df$season_group == ""
  )
  if (length(blank_rows) > 0) {
    error_list$blank <- paste0(
      "Blank or missing value(s) on row(s): ",
      paste(utils::head(blank_rows, 10), collapse = ", "),
      if (length(blank_rows) > 10) ", ..." else "",
      ". Every row must name a target group and a season group."
    )
    return(fail(error_list))
  }

  # ── Check 3: A target group cannot be in two seasonal groups ──────────────
  dup_targets <- unique(df$target_group[duplicated(df$target_group)])
  if (length(dup_targets) > 0) {
    conflicting <- vapply(dup_targets, function(g) {
      labels <- unique(df$season_group[df$target_group == g])
      paste0(g, " (", paste(labels, collapse = ", "), ")")
    }, character(1))

    error_list$duplicate_target <- paste0(
      "A target group can belong to only one season group. Conflicting ",
      "assignment(s): ", paste(utils::head(conflicting, 10), collapse = "; "),
      if (length(conflicting) > 10) ", ..." else "", "."
    )
  }

  # ── Check 4: Target groups must exist in the uploaded data ────────────────
  expected_groups <- sort(unique(as.character(target_groups)))
  found_groups    <- sort(unique(df$target_group))
  extra_groups    <- setdiff(found_groups, expected_groups)

  if (length(extra_groups) > 0) {
    error_list$extra_groups <- paste0(
      "Unrecognized target group(s) in the seasonal grouping file: ",
      paste(extra_groups, collapse = ", "), ". ",
      "Expected: ", paste(expected_groups, collapse = ", "), "."
    )
  }

  # ── Check 5: The default label is reserved ────────────────────────────────
  # Unlisted groups are assigned this label internally, so a user-supplied one
  # would silently merge those groups with the unlisted ones.
  if (any(df$season_group == SEASON_GROUP_DEFAULT_LABEL)) {
    error_list$reserved_label <- paste0(
      "\"", SEASON_GROUP_DEFAULT_LABEL, "\" is a reserved season group name ",
      "(it is used internally for target groups you don't list). Please use a ",
      "different label."
    )
  }

  if (length(error_list) > 0) {
    return(fail(error_list))
  }

  # ── Warning: unlisted groups fall into the shared default ─────────────────
  modelled_groups <- setdiff(expected_groups, agg_group)
  unlisted <- setdiff(modelled_groups, found_groups)
  if (length(unlisted) > 0) {
    warning_list$unlisted <- paste0(
      length(unlisted), " target group(s) are not listed and will share one ",
      "common seasonal curve: ",
      paste(utils::head(unlisted, 10), collapse = ", "),
      if (length(unlisted) > 10) ", ..." else "", ". ",
      "This is usually what you want -- list only the groups whose seasonality ",
      "differs from the majority."
    )
  }

  # ── Warning: the aggregate group is listed ────────────────────────────────
  if (!is.null(agg_group) && agg_group %in% found_groups) {
    warning_list$agg_group <- paste0(
      "The aggregate group \"", agg_group, "\" appears in the seasonal ",
      "grouping file. It is derived from the other groups rather than fit ",
      "directly, so its assignment will be ignored."
    )
  }

  # ── Warning: degenerate groupings ─────────────────────────────────────────
  # Both are legal, but each is equivalent to a setting the user could have
  # picked without uploading anything, so it's likely a mistake.
  n_labels <- length(unique(df$season_group))
  if (n_labels == 1 && length(unlisted) == 0) {
    warning_list$single_group <- paste0(
      "Every target group is assigned to the same season group, which is ",
      "identical to the \"Shared\" seasonality setting and needs no file."
    )
  }
  if (n_labels == length(modelled_groups) && length(unlisted) == 0 && n_labels > 1) {
    warning_list$all_separate <- paste0(
      "Every target group has its own season group, which is identical to the ",
      "\"Per target group\" seasonality setting and needs no file. Seasonal ",
      "curves estimated from a single group each are far less well identified ",
      "than pooled ones."
    )
  }

  list(
    errors = error_list,
    warnings = warning_list,
    data = df[, c("target_group", "season_group"), drop = FALSE]
  )
}


## Integer season-group index aligned to `levels`, for INLA's
## f(epiweek, ..., group=season_idx, ...) term.
##
## `levels` MUST be the same character vector used to derive group_idx -- see
## prep_data_inla(), which derives both from one call. Target groups absent from
## `assignments` all share one default group, so an uploaded file only needs to
## name the exceptions.
##
## Returns integers starting at 1, as INLA requires. The mapping from label to
## integer is arbitrary but deterministic (sorted label order), since only which
## groups share an index carries meaning.
season_group_index <- function(assignments, levels) {
  levels <- as.character(levels)
  n <- length(levels)

  if (n == 0) {
    return(integer(0))
  }

  label <- rep(SEASON_GROUP_DEFAULT_LABEL, n)

  if (!is.null(assignments) && nrow(assignments) > 0) {
    idx <- match(levels, as.character(assignments$target_group))
    assigned <- !is.na(idx)
    label[assigned] <- as.character(assignments$season_group)[idx[assigned]]
  }

  as.integer(factor(label, levels = sort(unique(label))))
}


## TRUE when an uploaded seasonal grouping actually distinguishes anything among
## `levels` -- i.e. it yields more than one season group. A file naming only
## groups that aren't being fit collapses to a single group, in which case the
## grouped seasonal model is identical to the shared one and should say so
## rather than pretend it did something.
season_groups_are_usable <- function(assignments, levels) {
  if (is.null(assignments) || nrow(assignments) == 0 || length(levels) == 0) {
    return(FALSE)
  }
  length(unique(season_group_index(assignments, levels))) > 1
}
