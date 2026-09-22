## Tests for R/validate_season_groups.R
##
## Seasonal grouping is declared separately from the neighbor graph on purpose:
## spatial adjacency and seasonal regime are different things that merely
## correlate. The property that matters most here is that season_group_index()
## keys off target group NAME and yields the same PARTITION regardless of the
## order of `levels` -- the same order-safety requirement the adjacency matrix
## has, since INLA sees only integers.

season_df <- function(target, season) {
  data.frame(target_group = target, season_group = season, stringsAsFactors = FALSE)
}

season_test_groups <- c("Alaska", "Hawaii", "Puerto Rico", "Texas", "Maine", "Overall")

# The headline use case, matching the reference's flusight grouping: name only
# the exceptions and let everything else share one curve.
exceptions_only <- season_df(
  c("Alaska", "Hawaii", "Puerto Rico"),
  c("Alaska", "Tropical", "Tropical")
)


# ── Index construction ──────────────────────────────────────────────────────

test_that("season_group_index groups by shared label, not by label value", {
  levels <- c("Texas", "Maine", "Alaska", "Hawaii", "Puerto Rico")
  idx <- season_group_index(exceptions_only, levels)
  names(idx) <- levels

  # Hawaii and Puerto Rico were given the same label, so they share a curve.
  expect_equal(idx[["Hawaii"]], idx[["Puerto Rico"]])
  # Alaska was given its own.
  expect_false(idx[["Alaska"]] == idx[["Hawaii"]])
  # Unlisted groups fall into one shared default, distinct from the named ones.
  expect_equal(idx[["Texas"]], idx[["Maine"]])
  expect_false(idx[["Texas"]] == idx[["Alaska"]])

  expect_equal(length(unique(idx)), 3)
})

test_that("season_group_index returns contiguous integers starting at 1", {
  # INLA requires group indices to be 1..k with no gaps.
  idx <- season_group_index(exceptions_only, c("Texas", "Alaska", "Hawaii"))
  expect_equal(sort(unique(as.integer(idx))), seq_len(length(unique(idx))))
  expect_true(min(idx) == 1)
})

test_that("season_group_index produces the same partition under any level order", {
  levels_a <- c("Texas", "Maine", "Alaska", "Hawaii", "Puerto Rico")
  levels_b <- rev(levels_a)

  idx_a <- season_group_index(exceptions_only, levels_a); names(idx_a) <- levels_a
  idx_b <- season_group_index(exceptions_only, levels_b); names(idx_b) <- levels_b

  # The integers themselves may differ; what must not differ is which groups
  # share a curve.
  for (i in levels_a) {
    for (j in levels_a) {
      expect_equal(
        idx_a[[i]] == idx_a[[j]],
        idx_b[[i]] == idx_b[[j]],
        info = paste("partition disagrees for", i, "and", j)
      )
    }
  }
})

test_that("season_group_index with no assignment puts everything in one group", {
  expect_equal(length(unique(season_group_index(NULL, c("A", "B", "C")))), 1)
  expect_length(season_group_index(exceptions_only, character(0)), 0)
})

test_that("season_groups_are_usable reports whether anything is distinguished", {
  expect_true(season_groups_are_usable(exceptions_only, c("Texas", "Alaska")))
  # Naming only groups that aren't being fit collapses to one group, which is
  # identical to "shared" -- the model should say so rather than pretend.
  expect_false(season_groups_are_usable(season_df("Alaska", "Alaska"), c("Texas", "Maine")))
  expect_false(season_groups_are_usable(NULL, c("Texas", "Maine")))
})


# ── Blocking errors ─────────────────────────────────────────────────────────

test_that("validate_season_groups_df requires both columns", {
  result <- validate_season_groups_df(data.frame(a = 1, b = 2), season_test_groups)

  expect_true("cols" %in% names(result$errors))
  expect_null(result$data)
})

test_that("validate_season_groups_df rejects unrecognized target groups", {
  result <- validate_season_groups_df(season_df("Atlantis", "Tropical"), season_test_groups)

  expect_true("extra_groups" %in% names(result$errors))
  expect_match(result$errors$extra_groups, "Atlantis")
  expect_null(result$data)
})

test_that("validate_season_groups_df rejects a target group in two season groups", {
  result <- validate_season_groups_df(
    season_df(c("Alaska", "Alaska"), c("Alaska", "Tropical")),
    season_test_groups
  )

  expect_true("duplicate_target" %in% names(result$errors))
  expect_match(result$errors$duplicate_target, "Alaska")
  expect_null(result$data)
})

test_that("validate_season_groups_df rejects blank values", {
  result <- validate_season_groups_df(season_df("Alaska", ""), season_test_groups)

  expect_true("blank" %in% names(result$errors))
  expect_null(result$data)
})

test_that("validate_season_groups_df reserves the internal default label", {
  # Unlisted groups are assigned this label internally, so allowing a user to
  # supply it would silently merge their group with the unlisted ones.
  result <- validate_season_groups_df(
    season_df("Alaska", SEASON_GROUP_DEFAULT_LABEL),
    season_test_groups
  )

  expect_true("reserved_label" %in% names(result$errors))
  expect_null(result$data)
})


# ── Non-blocking warnings ───────────────────────────────────────────────────

test_that("validate_season_groups_df accepts an exceptions-only file", {
  result <- validate_season_groups_df(exceptions_only, season_test_groups)

  expect_length(result$errors, 0)
  expect_equal(nrow(result$data), 3)
  expect_named(result$data, c("target_group", "season_group"))
})

test_that("validate_season_groups_df explains what happens to unlisted groups", {
  result <- validate_season_groups_df(exceptions_only, season_test_groups)

  expect_true("unlisted" %in% names(result$warnings))
  expect_match(result$warnings$unlisted, "Texas|Maine")
})

test_that("validate_season_groups_df warns when the aggregate group is listed", {
  result <- validate_season_groups_df(
    season_df(c("Alaska", "Overall"), c("Alaska", "Tropical")),
    season_test_groups
  )

  expect_length(result$errors, 0)
  expect_true("agg_group" %in% names(result$warnings))
})

test_that("validate_season_groups_df warns about degenerate groupings", {
  modelled <- setdiff(season_test_groups, "Overall")

  # Everything in one group is identical to the "shared" setting.
  all_same <- validate_season_groups_df(
    season_df(modelled, rep("Everything", length(modelled))),
    season_test_groups
  )
  expect_true("single_group" %in% names(all_same$warnings))

  # Everything in its own group is identical to the "target_group" setting.
  all_separate <- validate_season_groups_df(
    season_df(modelled, modelled),
    season_test_groups
  )
  expect_true("all_separate" %in% names(all_separate$warnings))
})


# ── Formula generation ──────────────────────────────────────────────────────
#
# Same technique as test-neighbor-graph.R: R/inla.R can't be source()d because
# it calls inla.setOption() at the top, so evaluate only the definitions under
# test straight from the shipped file.
local({
  wanted <- c("INLA_INTERACTION_CHOICES", "INLA_SEASONAL_CHOICES", "inla_model_formula")
  for (e in as.list(parse(test_path("../../R/inla.R")))) {
    if (is.call(e) && length(e) >= 3 &&
        is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") &&
        is.name(e[[2]]) && as.character(e[[2]]) %in% wanted) {
      eval(e, envir = globalenv())
    }
  }
})

test_that("the seasonal default leaves existing formulas untouched", {
  # Same pins as test-neighbor-graph.R. If these fail, every existing INFLAenza
  # forecast has silently changed.
  expect_identical(
    inla_model_formula(FALSE, "exchangeable", "shared"),
    inla_model_formula(FALSE)
  )
  expect_identical(
    inla_model_formula(TRUE, "exchangeable", "shared"),
    inla_model_formula(TRUE)
  )
})

test_that("a single target group ignores the seasonal grouping", {
  # One group means one curve; there is nothing to group over.
  for (choice in INLA_SEASONAL_CHOICES) {
    expect_identical(inla_model_formula(TRUE, "exchangeable", choice),
                     inla_model_formula(TRUE))
  }
})

test_that("each seasonal choice groups over the intended index", {
  shared <- inla_model_formula(FALSE, "exchangeable", "shared")
  by_season <- inla_model_formula(FALSE, "exchangeable", "season_group")
  by_target <- inla_model_formula(FALSE, "exchangeable", "target_group")

  expect_false(grepl("f(epiweek, model=\"rw2\", cyclic=TRUE, hyper=hyper_epwk, scale.model=TRUE, group=",
                     shared, fixed = TRUE))
  expect_match(by_season, "group=season_idx, control.group=list(model=\"iid\")", fixed = TRUE)
  expect_match(by_target, "group=group_idx, control.group=list(model=\"iid\")", fixed = TRUE)
})

test_that("every seasonal x interaction combination parses", {
  for (s in INLA_SEASONAL_CHOICES) {
    for (i in INLA_INTERACTION_CHOICES) {
      expect_s3_class(as.formula(inla_model_formula(FALSE, i, s)), "formula")
    }
  }
})

test_that("an unrecognized seasonal choice is rejected", {
  expect_error(inla_model_formula(FALSE, "exchangeable", "not-a-choice"))
})
