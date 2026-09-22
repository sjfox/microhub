## Tests for the Beta censoring helpers in R/inla.R
##
## The Beta density is defined on the open interval (0, 1), so an observation of
## exactly 0 has no likelihood. microhub used to clamp those to a hardcoded
## 1e-4 -- rewriting a zero as a value that was never observed, with a constant
## that knows nothing about the data's scale. On an RSV ED-visits series whose
## median is 8e-4, that constant sits at roughly the 15th percentile and
## collides with the smallest genuine positive value.
##
## Censoring replaces it: the threshold is derived from the data, the observed
## values are left untouched, and INLA integrates the likelihood over (0, c) for
## anything at or below it. This mirrors the upstream production pipelines
## (inla-forecasting-paper, scripts/flusight-25-26 and scripts/metrocast-25-26).
##
## R/inla.R can't be source()d here -- it calls inla.setOption() at the top --
## so evaluate just the helpers under test, as test-neighbor-graph.R does.
local({
  wanted <- c(
    "beta_censor_value",
    "warn_if_beta_censor_touches_upper_tail",
    "beta_precision_mean"
  )
  for (e in as.list(parse(test_path("../../R/inla.R")))) {
    if (is.call(e) && length(e) >= 3 &&
        is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") &&
        is.name(e[[2]]) && as.character(e[[2]]) %in% wanted) {
      eval(e, envir = globalenv())
    }
  }
})


# ── The defining property ───────────────────────────────────────────────────

test_that("no genuine positive observation is ever censored", {
  # This is what makes the threshold safe: it is strictly below every positive
  # value, so only true zeros fall at or under it.
  set.seed(20260922)

  for (i in seq_len(100)) {
    values <- c(
      rep(0, sample(0:20, 1)),
      round(runif(sample(5:60, 1), 0, 1), sample(2:6, 1))
    )
    cens <- beta_censor_value(values)
    positive <- values[values > 0]

    if (length(positive) > 0) {
      expect_true(
        all(positive > cens),
        info = paste("iteration", i, "censored a positive value at cens =", cens)
      )
    }
  }
})

test_that("the threshold is bounded by the upper tail as well as the lower", {
  # INLA censors symmetrically: y <= c is left-censored, y >= 1 - c is
  # right-censored. Reasoning only about the lower tail gives c = 0.25 here,
  # which would swallow the genuine 0.999.
  values <- c(0, 0.5, 0.999)
  cens <- beta_censor_value(values)

  expect_equal(cens, (1 - 0.999) / 2)
  expect_true(all(values[values > 0] > cens))   # lower tail safe
  expect_true(all(values < 1 - cens))           # upper tail safe
})

test_that("a series that reaches 1 is left right-censorable", {
  # Saturation at 1 SHOULD be right-censored, and the upper bound would be
  # degenerate there, so it is deliberately not applied.
  cens <- beta_censor_value(c(0, 0.4, 1))

  expect_true(cens > 0)
  expect_equal(cens, 0.2)
})


# ── Scale ───────────────────────────────────────────────────────────────────

test_that("the threshold scales with the data rather than being fixed", {
  small <- beta_censor_value(c(0, 0.0001, 0.04))
  large <- beta_censor_value(c(0, 0.30, 0.60))

  expect_true(small < large)
  # The old hardcoded clamp was 1e-4, which on the small series is ABOVE the
  # smallest real observation -- the failure this change exists to fix.
  expect_true(small < 1e-4)
})


# ── Degenerate input ────────────────────────────────────────────────────────

test_that("beta_censor_value returns a usable threshold for degenerate input", {
  # A threshold of 0 or NA would be rejected by INLA, so every path must return
  # something strictly positive.
  expect_true(beta_censor_value(c(0, 0, 0)) > 0)
  expect_true(beta_censor_value(c(NA, NA)) > 0)
  expect_true(beta_censor_value(numeric(0)) > 0)
  expect_true(beta_censor_value(NULL) > 0)
})

test_that("beta_censor_value ignores NA and non-finite values", {
  expect_equal(beta_censor_value(c(NA, Inf, 0.4, 0)), 0.2)
  expect_equal(beta_censor_value(c(0, 0.2)), 0.1)
})

test_that("censoring is a no-op when the data contains no zeros", {
  # Nothing is at or below the threshold, so the likelihood is unchanged.
  values <- c(0.3, 0.6, 0.9)
  cens <- beta_censor_value(values)

  expect_true(all(values > cens))
})


# ── Upper-tail guard ────────────────────────────────────────────────────────

test_that("the upper-tail warning fires only when it should", {
  expect_silent(
    warn_if_beta_censor_touches_upper_tail(c(0, 0.5, 0.999), beta_censor_value(c(0, 0.5, 0.999)))
  )
  # Forced: a threshold large enough to reach real observations.
  expect_warning(warn_if_beta_censor_touches_upper_tail(c(0.4, 0.8), 0.3))
})


# ── Hyperparameter lookup ───────────────────────────────────────────────────

test_that("the Beta precision is found by name, not by position", {
  # It was read as summary.hyperpar[1, ], which assumes the family
  # hyperparameter is listed first.
  fit <- list(summary.hyperpar = data.frame(
    mean = c(0.5, 42, 7),
    row.names = c(
      "Precision for epiweek",
      "precision parameter for the beta observations",
      "Rho for t"
    )
  ))

  expect_equal(beta_precision_mean(fit), 42)
})

test_that("the Beta precision lookup falls back to the first row", {
  fit <- list(summary.hyperpar = data.frame(
    mean = c(99, 1),
    row.names = c("something", "else")
  ))

  expect_equal(beta_precision_mean(fit), 99)
})
