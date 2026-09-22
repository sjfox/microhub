## Validation logic for a user-uploaded neighbor (adjacency) graph.
##
## The file is a two-column edge list. Both columns hold values from the
## `target_group` column of the uploaded time series -- nothing else:
##
##   target_group,neighbor
##   Region A,Region B
##   Region B,Region C
##
## Each row declares an undirected neighbor relationship, so one row per pair is
## enough; the reverse direction is filled in when the matrix is built. There is
## deliberately no weight column: INLA's besag/besagproper models read the
## neighborhood STRUCTURE of `graph=` and not the values in it, so a weight
## would have no effect on the fit.
##
## Why an edge list rather than an N x N matrix: it scales to large sparse
## panels (a 255-country graph is a few hundred rows instead of 65,025 cells),
## and it has no row/column ORDER for a caller to get wrong. INLA's `graph=`
## takes a bare matrix indexed by the integer group index with no names in it,
## so the binding between "row 3" and a particular region exists only through
## that integer. Building the matrix from names in build_neighbor_matrix() makes
## the name lookup the only way to construct it at all.
##
## Returns a list with:
##   $errors   — named list of blocking error strings (graph not stored if any exist)
##   $warnings — named list of non-blocking warning strings (graph still stored)
##   $data     — cleaned edge-list data frame (target_group, neighbor), or NULL
##
## Note this deliberately returns the EDGE LIST, not a matrix. The matrix has to
## be built against the exact set of groups the model actually fits, which is
## not always the full uploaded set -- fit_process_inla() drops the aggregate
## group before fitting when it derives it by aggregation. See
## build_neighbor_matrix().
##
## File reading is split from validation so the logic below can be unit-tested
## without a file or a Shiny session (same reason retrospective_parameter_input_widget()
## is factored out in R/retrospective.R). readr::read_csv is used for reading
## because it strips a UTF-8 BOM, which the bundled templates carry and which
## base read.csv would fold into the first column name.

validate_neighbor_graph <- function(file, target_groups, agg_group = "Overall") {
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

  validate_neighbor_graph_df(
    df = as.data.frame(df, stringsAsFactors = FALSE),
    target_groups = target_groups,
    agg_group = agg_group
  )
}


## All validation logic. Takes an already-read data frame. Base R only.
validate_neighbor_graph_df <- function(df, target_groups, agg_group = "Overall") {
  error_list   <- list()
  warning_list <- list()

  fail <- function(errors) {
    list(errors = errors, warnings = warning_list, data = NULL)
  }

  # ── Check 1: Required columns ─────────────────────────────────────────────
  req_cols     <- c("target_group", "neighbor")
  missing_cols <- setdiff(req_cols, colnames(df))
  if (length(missing_cols) > 0) {
    error_list$cols <- paste0(
      "Missing required column(s): ", paste(missing_cols, collapse = ", "),
      ". The neighbor file needs exactly two columns, 'target_group' and ",
      "'neighbor', both holding target group names from your time series data. ",
      "Download the template to see the expected format."
    )
    # Cannot run further checks without required columns
    return(fail(error_list))
  }

  df$target_group <- trimws(as.character(df$target_group))
  df$neighbor     <- trimws(as.character(df$neighbor))

  # ── Check 2: No blank or missing group names ──────────────────────────────
  blank_rows <- which(
    is.na(df$target_group) | is.na(df$neighbor) |
      df$target_group == "" | df$neighbor == ""
  )
  if (length(blank_rows) > 0) {
    error_list$blank <- paste0(
      "Blank or missing group name(s) on row(s): ",
      paste(utils::head(blank_rows, 10), collapse = ", "),
      if (length(blank_rows) > 10) ", ..." else "",
      ". Every row must name two target groups."
    )
    return(fail(error_list))
  }

  # ── Check 3: No self-loops ────────────────────────────────────────────────
  self_loops <- which(df$target_group == df$neighbor)
  if (length(self_loops) > 0) {
    error_list$self_loop <- paste0(
      "A group cannot be its own neighbor. Self-referencing row(s) found for: ",
      paste(unique(df$target_group[self_loops]), collapse = ", "), "."
    )
  }

  # ── Check 4: Group names must match the uploaded data ─────────────────────
  # Only one direction is an error. Groups in the data with no edges are
  # legitimate (a genuine island), so that is a warning further down.
  expected_groups <- sort(unique(as.character(target_groups)))
  found_groups    <- sort(unique(c(df$target_group, df$neighbor)))
  extra_groups    <- setdiff(found_groups, expected_groups)

  if (length(extra_groups) > 0) {
    error_list$extra_groups <- paste0(
      "Unrecognized target group(s) in the neighbor file: ",
      paste(extra_groups, collapse = ", "), ". ",
      "Expected: ", paste(expected_groups, collapse = ", "), "."
    )
  }

  # ── Bail out before the warning checks if anything is already broken ──────
  if (length(error_list) > 0) {
    return(fail(error_list))
  }

  # ── Warning: duplicate edges ──────────────────────────────────────────────
  # Undirected, so (A,B) and (B,A) are the same edge.
  pair_key <- paste(
    pmin(df$target_group, df$neighbor),
    pmax(df$target_group, df$neighbor),
    sep = "\r"
  )
  dup_idx <- which(duplicated(pair_key))
  if (length(dup_idx) > 0) {
    dup_shown <- unique(paste(df$target_group[dup_idx], "-", df$neighbor[dup_idx]))
    warning_list$duplicates <- paste0(
      length(dup_idx), " duplicate edge(s) found and ignored (an edge is ",
      "undirected, so A-B and B-A are the same): ",
      paste(utils::head(dup_shown, 5), collapse = "; "),
      if (length(dup_shown) > 5) ", ..." else "", "."
    )
    df <- df[!duplicated(pair_key), , drop = FALSE]
  }

  # ── Warning: the aggregate group should not be a node ─────────────────────
  # "Overall" is derived by aggregating the other groups, so it has no place in
  # a spatial neighborhood structure.
  if (!is.null(agg_group) && agg_group %in% found_groups) {
    warning_list$agg_group <- paste0(
      "The aggregate group \"", agg_group, "\" appears in the neighbor file. ",
      "It is derived from the other groups rather than fit spatially, so its ",
      "edges will be ignored."
    )
  }

  # ── Warning: groups with no neighbors ─────────────────────────────────────
  modelled_groups <- setdiff(expected_groups, agg_group)
  isolated <- setdiff(modelled_groups, found_groups)
  if (length(isolated) > 0) {
    warning_list$isolated <- paste0(
      length(isolated), " target group(s) have no neighbors listed: ",
      paste(utils::head(isolated, 10), collapse = ", "),
      if (length(isolated) > 10) ", ..." else "", ". ",
      "This is fine for a genuine island, but is often a typo or an omission."
    )
  }

  # ── Warning: disconnected components ──────────────────────────────────────
  n_components <- neighbor_graph_n_components(df, modelled_groups)
  if (n_components > 1) {
    warning_list$components <- paste0(
      "The neighbor graph has ", n_components, " disconnected components. ",
      "This is expected when the regions genuinely form separate clusters, ",
      "but check it is not an omission."
    )
  }

  list(
    errors = error_list,
    warnings = warning_list,
    data = df[, c("target_group", "neighbor"), drop = FALSE]
  )
}


## Count connected components of the undirected graph, treating every group in
## `all_groups` as a node (so a group with no edges counts as its own
## component). Plain iterative union-find; no extra dependencies.
neighbor_graph_n_components <- function(edges, all_groups) {
  all_groups <- unique(as.character(all_groups))
  n <- length(all_groups)
  if (n == 0) {
    return(0L)
  }

  # Unnamed integer vector: parent[i] is the index of i's parent. Kept unnamed
  # deliberately so root indices stay plain integers.
  parent <- seq_len(n)

  find_root <- function(i, parent) {
    while (parent[[i]] != i) {
      i <- parent[[i]]
    }
    i
  }

  if (!is.null(edges) && nrow(edges) > 0) {
    ia <- match(as.character(edges$target_group), all_groups)
    ib <- match(as.character(edges$neighbor), all_groups)

    for (k in seq_len(nrow(edges))) {
      if (is.na(ia[[k]]) || is.na(ib[[k]])) next
      ra <- find_root(ia[[k]], parent)
      rb <- find_root(ib[[k]], parent)
      if (ra != rb) {
        parent[[ra]] <- rb
      }
    }
  }

  roots <- vapply(
    seq_len(n),
    function(i) find_root(i, parent),
    integer(1),
    USE.NAMES = FALSE
  )

  length(unique(roots))
}


## Build the symmetric 0/1 adjacency matrix INLA needs, indexed in the order of
## `levels`.
##
## `levels` MUST be the same character vector used to derive the integer group
## index passed to INLA's f(group_idx, ...) term -- see prep_data_inla(), which
## derives both from one call so they cannot drift apart. Edges naming a group
## outside `levels` are dropped silently, which is what makes it safe to fit a
## subset of groups (e.g. with the aggregate group removed) against a graph
## covering all of them.
##
## The returned matrix is symmetric with a zero diagonal.
build_neighbor_matrix <- function(edges, levels) {
  levels <- as.character(levels)
  n <- length(levels)

  W <- matrix(0, nrow = n, ncol = n, dimnames = list(levels, levels))

  if (is.null(edges) || nrow(edges) == 0 || n == 0) {
    return(W)
  }

  i <- match(as.character(edges$target_group), levels)
  j <- match(as.character(edges$neighbor), levels)

  keep <- !is.na(i) & !is.na(j) & i != j
  i <- i[keep]
  j <- j[keep]

  if (length(i) == 0) {
    return(W)
  }

  W[cbind(i, j)] <- 1
  W[cbind(j, i)] <- 1

  W
}


## Which of `levels` actually have at least one neighbor inside `levels`.
##
## Used to re-check a graph at fit time rather than trusting the check made when
## it was uploaded: the groups being fit are not always the groups the graph was
## validated against. `missing` groups are not an error -- a genuine island has
## no neighbors -- but they are modelled as isolated, which the user should know
## about rather than discover in the forecasts.
neighbor_graph_coverage <- function(edges, levels) {
  levels <- as.character(levels)
  w <- build_neighbor_matrix(edges, levels)

  if (length(levels) == 0) {
    return(list(covered = character(0), missing = character(0), n_edges = 0L))
  }

  has_neighbor <- rowSums(w) > 0

  list(
    covered = levels[has_neighbor],
    missing = levels[!has_neighbor],
    n_edges = as.integer(sum(w) / 2)
  )
}


## TRUE when a stored neighbor graph can actually be used for `levels` -- i.e.
## it produces at least one edge among the groups being fit. Used to decide
## whether a besagproper request is honourable, so the fallback is explicit
## rather than a silently mis-specified model.
neighbor_graph_is_usable <- function(edges, levels) {
  if (is.null(edges) || nrow(edges) == 0) {
    return(FALSE)
  }
  W <- build_neighbor_matrix(edges, levels)
  any(W > 0)
}
