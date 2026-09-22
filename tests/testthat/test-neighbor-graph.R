## Tests for R/validate_neighbor_graph.R
##
## The central property under test is that the adjacency matrix handed to INLA
## is keyed entirely by target group NAME. INLA's f(group_idx, ..., graph=W)
## sees only an integer index and an unnamed matrix, so if W's row order ever
## disagrees with the order that produced group_idx, the model fits cleanly and
## forecasts wrongly with no error. build_neighbor_matrix() is the only place
## that binding is made, so it carries the bulk of these tests.

edge_df <- function(a, b) {
  data.frame(target_group = a, neighbor = b, stringsAsFactors = FALSE)
}

test_groups <- c("Adult", "Pediatric", "Senior", "Island", "Overall")


# ── Matrix construction: the order-safety property ──────────────────────────

test_that("build_neighbor_matrix keys edges by name, not by row order", {
  edges <- edge_df(c("Senior", "Adult"), c("Adult", "Pediatric"))

  levels_a <- c("Pediatric", "Adult", "Senior", "Island")
  levels_b <- c("Island", "Senior", "Adult", "Pediatric")

  w_a <- build_neighbor_matrix(edges, levels_a)
  w_b <- build_neighbor_matrix(edges, levels_b)

  # Same logical graph under two different level orders must agree cell for
  # cell when addressed by name.
  for (i in levels_a) {
    for (j in levels_a) {
      expect_equal(
        w_a[i, j], w_b[i, j],
        info = paste("cell", i, j, "disagrees between level orderings")
      )
    }
  }
})

test_that("build_neighbor_matrix is symmetric with a zero diagonal", {
  edges <- edge_df(c("Senior", "Adult"), c("Adult", "Pediatric"))
  w <- build_neighbor_matrix(edges, c("Pediatric", "Adult", "Senior", "Island"))

  expect_equal(w, t(w))
  expect_true(all(diag(w) == 0))
})

test_that("build_neighbor_matrix treats edges as undirected", {
  levels <- c("Pediatric", "Adult", "Senior", "Island")

  forward <- build_neighbor_matrix(edge_df(c("Senior", "Adult"), c("Adult", "Pediatric")), levels)
  reverse <- build_neighbor_matrix(edge_df(c("Adult", "Pediatric"), c("Senior", "Adult")), levels)

  expect_equal(forward, reverse)
})

test_that("build_neighbor_matrix records exactly the declared edges", {
  edges <- edge_df(c("Senior", "Adult"), c("Adult", "Pediatric"))
  w <- build_neighbor_matrix(edges, c("Pediatric", "Adult", "Senior", "Island"))

  expect_equal(w["Adult", "Pediatric"], 1)
  expect_equal(w["Pediatric", "Adult"], 1)
  expect_equal(w["Adult", "Senior"], 1)
  expect_equal(w["Pediatric", "Senior"], 0)
  expect_true(all(w["Island", ] == 0))
})

test_that("build_neighbor_matrix ignores edges naming groups outside levels", {
  # fit_process_inla() drops the aggregate group before fitting, so the graph
  # routinely covers more groups than the model fits.
  edges <- edge_df(c("Adult", "Overall"), c("Pediatric", "Adult"))
  w <- build_neighbor_matrix(edges, c("Adult", "Pediatric"))

  expect_equal(dim(w), c(2L, 2L))
  expect_equal(w["Adult", "Pediatric"], 1)
})

test_that("build_neighbor_matrix handles empty input", {
  expect_equal(dim(build_neighbor_matrix(NULL, c("A", "B"))), c(2L, 2L))
  expect_true(all(build_neighbor_matrix(NULL, c("A", "B")) == 0))
  expect_equal(dim(build_neighbor_matrix(edge_df("A", "B"), character(0))), c(0L, 0L))
})


# ── Blocking errors ─────────────────────────────────────────────────────────

test_that("validate_neighbor_graph_df requires both columns", {
  result <- validate_neighbor_graph_df(data.frame(a = 1, b = 2), test_groups)

  expect_true("cols" %in% names(result$errors))
  expect_null(result$data)
})

test_that("validate_neighbor_graph_df rejects unrecognized target groups", {
  result <- validate_neighbor_graph_df(edge_df("Adult", "Nowhere"), test_groups)

  expect_true("extra_groups" %in% names(result$errors))
  expect_match(result$errors$extra_groups, "Nowhere")
  expect_null(result$data)
})

test_that("validate_neighbor_graph_df rejects self-referencing edges", {
  result <- validate_neighbor_graph_df(edge_df("Adult", "Adult"), test_groups)

  expect_true("self_loop" %in% names(result$errors))
  expect_null(result$data)
})

test_that("validate_neighbor_graph_df rejects blank group names", {
  result <- validate_neighbor_graph_df(edge_df("Adult", ""), test_groups)

  expect_true("blank" %in% names(result$errors))
  expect_null(result$data)
})


# ── Non-blocking warnings ───────────────────────────────────────────────────

test_that("validate_neighbor_graph_df accepts a clean edge list", {
  result <- validate_neighbor_graph_df(
    edge_df(c("Adult", "Pediatric"), c("Pediatric", "Senior")),
    c("Adult", "Pediatric", "Senior")
  )

  expect_length(result$errors, 0)
  expect_equal(nrow(result$data), 2)
  expect_named(result$data, c("target_group", "neighbor"))
})

test_that("validate_neighbor_graph_df warns about reversed duplicate edges", {
  result <- validate_neighbor_graph_df(
    edge_df(c("Adult", "Pediatric"), c("Pediatric", "Adult")),
    test_groups
  )

  expect_length(result$errors, 0)
  expect_true("duplicates" %in% names(result$warnings))
  expect_equal(nrow(result$data), 1)
})

test_that("validate_neighbor_graph_df warns when the aggregate group is a node", {
  result <- validate_neighbor_graph_df(
    edge_df(c("Adult", "Overall"), c("Pediatric", "Adult")),
    test_groups
  )

  expect_length(result$errors, 0)
  expect_true("agg_group" %in% names(result$warnings))
})

test_that("validate_neighbor_graph_df warns about groups with no neighbors", {
  result <- validate_neighbor_graph_df(
    edge_df(c("Adult"), c("Pediatric")),
    c("Adult", "Pediatric", "Island")
  )

  expect_true("isolated" %in% names(result$warnings))
  expect_match(result$warnings$isolated, "Island")
})

test_that("validate_neighbor_graph_df warns about disconnected components", {
  result <- validate_neighbor_graph_df(
    edge_df(c("Adult", "Senior"), c("Pediatric", "Island")),
    c("Adult", "Pediatric", "Senior", "Island")
  )

  expect_length(result$errors, 0)
  expect_true("components" %in% names(result$warnings))
})


# ── Component counting ──────────────────────────────────────────────────────

test_that("neighbor_graph_n_components counts connected components", {
  groups <- c("A", "B", "C", "D")

  expect_equal(neighbor_graph_n_components(edge_df(c("A", "C"), c("B", "D")), groups), 2)
  expect_equal(neighbor_graph_n_components(edge_df(c("A", "B", "C"), c("B", "C", "D")), groups), 1)
  expect_equal(neighbor_graph_n_components(NULL, groups), 4)
  expect_equal(neighbor_graph_n_components(NULL, character(0)), 0)
})


# ── Usability guard ─────────────────────────────────────────────────────────

test_that("neighbor_graph_is_usable reports whether any edge is in scope", {
  edges <- edge_df("Adult", "Pediatric")

  expect_true(neighbor_graph_is_usable(edges, c("Adult", "Pediatric")))
  # A graph that covers none of the groups being fit is not usable -- this is
  # what makes a besagproper fallback explicit rather than a silently
  # mis-specified model.
  expect_false(neighbor_graph_is_usable(edges, c("Senior", "Island")))
  expect_false(neighbor_graph_is_usable(NULL, c("Adult", "Pediatric")))
})


# ── Model formula generation ────────────────────────────────────────────────
#
# R/inla.R can't be source()d here: its first lines call inla.setOption(), which
# needs INLA installed. Following the pattern of extract_pure_server_functions()
# in helper-load-app.R, evaluate just the two top-level definitions under test
# straight from the real file, so these assertions run against shipped code
# rather than a copy.
local({
  wanted <- c("INLA_INTERACTION_CHOICES", "inla_model_formula")
  for (e in as.list(parse(test_path("../../R/inla.R")))) {
    if (is.call(e) && length(e) >= 3 &&
        is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") &&
        is.name(e[[2]]) && as.character(e[[2]]) %in% wanted) {
      eval(e, envir = globalenv())
    }
  }
})

# The exact strings R/inla.R produced before the interaction option existed.
# If either of these assertions fails, every existing INFLAenza forecast has
# silently changed -- that is the point of pinning them here.
PRE_EXISTING_SINGLE_GROUP_FORMULA <-
  'value ~ 1 + f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model=TRUE) +
   f(t, model="ar1", hyper=hyper_wk)'

PRE_EXISTING_MULTI_GROUP_FORMULA <-
  'value ~ 1 + target_group +
   f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model=TRUE) +
   f(t, model="ar1", hyper=hyper_wk, group=group_idx, control.group=list(model="exchangeable"))'


test_that("the default formulas are unchanged from before the interaction option", {
  expect_identical(inla_model_formula(TRUE), PRE_EXISTING_SINGLE_GROUP_FORMULA)
  expect_identical(inla_model_formula(FALSE), PRE_EXISTING_MULTI_GROUP_FORMULA)
  expect_identical(inla_model_formula(FALSE, "exchangeable"), PRE_EXISTING_MULTI_GROUP_FORMULA)
})

test_that("a single target group ignores the interaction entirely", {
  for (choice in INLA_INTERACTION_CHOICES) {
    expect_identical(
      inla_model_formula(TRUE, choice),
      PRE_EXISTING_SINGLE_GROUP_FORMULA
    )
  }
})

test_that("every interaction choice produces a parseable formula", {
  for (choice in INLA_INTERACTION_CHOICES) {
    expect_s3_class(as.formula(inla_model_formula(FALSE, choice)), "formula")
  }
})

test_that("interaction choices produce the intended group structure", {
  expect_match(inla_model_formula(FALSE, "iid"), 'control.group=list(model="iid")', fixed = TRUE)
  # "none" pools completely: one shared trend, no group= term at all.
  expect_false(grepl("group=group_idx", inla_model_formula(FALSE, "none"), fixed = TRUE))
})

test_that("besagproper carries both a main temporal term and a spatial interaction", {
  formula <- inla_model_formula(FALSE, "besagproper")

  expect_match(formula, "graph=graph", fixed = TRUE)
  expect_match(formula, 'model="besagproper"', fixed = TRUE)
  # The spatial term is grouped over t2, so the main f(t, ...) effect must also
  # be present -- INLA needs a distinct index column per f() term.
  expect_match(formula, "group=t2", fixed = TRUE)
  expect_match(formula, 'f(t, model="ar1", hyper=hyper_wk) +', fixed = TRUE)
})

test_that("an unrecognized interaction is rejected rather than silently ignored", {
  expect_error(inla_model_formula(FALSE, "not-a-structure"))
})


# ── Coverage re-validation ──────────────────────────────────────────────────
#
# A graph is validated against whatever target groups were loaded at upload
# time; the groups actually fit can differ (a different dataset, or one
# retrospective group's slice). neighbor_graph_coverage() is the fit-time
# re-check that keeps a partially-covering graph from being used in silence.

test_that("neighbor_graph_coverage separates covered from isolated groups", {
  edges <- edge_df(c("Adult"), c("Pediatric"))
  coverage <- neighbor_graph_coverage(edges, c("Adult", "Pediatric", "Island"))

  expect_setequal(coverage$covered, c("Adult", "Pediatric"))
  expect_equal(coverage$missing, "Island")
  expect_equal(coverage$n_edges, 1)
})

test_that("neighbor_graph_coverage reports full coverage when every group has an edge", {
  edges <- edge_df(c("Adult", "Pediatric"), c("Pediatric", "Senior"))
  coverage <- neighbor_graph_coverage(edges, c("Adult", "Pediatric", "Senior"))

  expect_length(coverage$missing, 0)
  expect_equal(coverage$n_edges, 2)
})

test_that("neighbor_graph_coverage handles a graph covering none of the fitted groups", {
  coverage <- neighbor_graph_coverage(edge_df("Adult", "Pediatric"), c("Senior", "Island"))

  expect_setequal(coverage$missing, c("Senior", "Island"))
  expect_equal(coverage$n_edges, 0)
})

test_that("neighbor_graph_coverage handles empty inputs", {
  expect_equal(neighbor_graph_coverage(NULL, c("A", "B"))$missing, c("A", "B"))
  expect_length(neighbor_graph_coverage(NULL, character(0))$missing, 0)
})


# ── Main-term variants ──────────────────────────────────────────────────────
#
# besagproper necessarily carries a separate main temporal effect. Comparing it
# against plain "exchangeable" (which does not) would vary two things at once,
# so the "_main" variants exist to hold the main term constant.

test_that("the main-term variants are offered", {
  expect_true(all(c("exchangeable_main", "iid_main") %in% INLA_INTERACTION_CHOICES))
})

test_that("main-term variants carry both a main effect and a grouped interaction", {
  for (choice in c("exchangeable_main", "iid_main")) {
    formula <- inla_model_formula(FALSE, choice)

    expect_match(formula, 'f(t, model="ar1", hyper=hyper_wk) +', fixed = TRUE)
    # The interaction goes on t2, since INLA needs a distinct index per f() term.
    expect_match(formula, "f(t2, model=\"ar1\"", fixed = TRUE)
    expect_match(formula, "group=group_idx", fixed = TRUE)
    expect_s3_class(as.formula(formula), "formula")
  }

  expect_match(inla_model_formula(FALSE, "exchangeable_main"),
               'control.group=list(model="exchangeable")', fixed = TRUE)
  expect_match(inla_model_formula(FALSE, "iid_main"),
               'control.group=list(model="iid")', fixed = TRUE)
})

test_that("exchangeable_main differs from besagproper only in the group structure", {
  # This is the property that makes the comparison interpretable: both carry the
  # same main temporal term, so a scoring difference is attributable to the
  # group structure alone.
  main_term <- 'f(t, model="ar1", hyper=hyper_wk) +'

  expect_match(inla_model_formula(FALSE, "exchangeable_main"), main_term, fixed = TRUE)
  expect_match(inla_model_formula(FALSE, "besagproper"), main_term, fixed = TRUE)
  # ...whereas the legacy default has no separate main term at all.
  expect_false(grepl(main_term, inla_model_formula(FALSE, "exchangeable"), fixed = TRUE))
})

test_that("the legacy variants are untouched by the new options", {
  expect_identical(inla_model_formula(FALSE), PRE_EXISTING_MULTI_GROUP_FORMULA)
  expect_identical(inla_model_formula(TRUE), PRE_EXISTING_SINGLE_GROUP_FORMULA)
})
