test_that("build_ensemble returns NULL when fewer than two members are available", {
  df <- tibble(
    model = c("A", "B"),
    reference_date = as.Date("2026-01-10"),
    horizon = 0L,
    target_end_date = as.Date("2026-01-10"),
    target_group = "Overall",
    output_type = "quantile",
    output_type_id = "0.5",
    value = c(10, 20)
  )

  expect_null(build_ensemble(df, members = "A"))
  expect_null(build_ensemble(df, members = c("A", "C")))
})

test_that("build_ensemble median method reproduces MicroHub's original per-quantile median", {
  df <- tibble(
    model = rep(c("A", "B", "C"), each = 2),
    reference_date = as.Date("2026-01-10"),
    horizon = 1L,
    target_end_date = as.Date("2026-01-17"),
    target_group = "Overall",
    output_type = "quantile",
    output_type_id = rep(c("0.25", "0.75"), times = 3),
    value = c(5, 15, 10, 20, 30, 40)
  )

  result <- build_ensemble(df, members = c("A", "B", "C"), method = "median")

  expect_equal(
    names(result),
    c("model", "reference_date", "horizon", "target_end_date",
      "target_group", "output_type", "output_type_id", "value")
  )
  expect_true(all(result$model == "Ensemble"))
  expect_equal(nrow(result), 2)

  q25 <- result$value[result$output_type_id == "0.25"]
  q75 <- result$value[result$output_type_id == "0.75"]
  expect_equal(q25, round(median(c(5, 10, 30)), 0))
  expect_equal(q75, round(median(c(15, 20, 40)), 0))
})

test_that("build_ensemble mean method averages quantile-by-quantile", {
  df <- tibble(
    model = c("A", "B"),
    reference_date = as.Date("2026-01-10"),
    horizon = 0L,
    target_end_date = as.Date("2026-01-10"),
    target_group = "Overall",
    output_type = "quantile",
    output_type_id = "0.5",
    value = c(10, 21)
  )

  result <- build_ensemble(df, members = c("A", "B"), method = "mean")
  expect_equal(result$value, round(mean(c(10, 21)), 0))
})

test_that("build_ensemble respects a custom model_label", {
  df <- tibble(
    model = c("A", "B"),
    reference_date = as.Date("2026-01-10"),
    horizon = 0L,
    target_end_date = as.Date("2026-01-10"),
    target_group = "Overall",
    output_type = "quantile",
    output_type_id = "0.5",
    value = c(10, 20)
  )

  result <- build_ensemble(df, members = c("A", "B"), model_label = "Test Ensemble")
  expect_equal(unique(result$model), "Test Ensemble")
})

test_that("build_ensemble linear_pool combines full predictive distributions", {
  testthat::skip_if_not_installed("hubEnsembles")

  quantile_levels <- c("0.1", "0.25", "0.5", "0.75", "0.9")
  make_member <- function(model_name, center, spread) {
    tibble::tibble(
      model = model_name,
      reference_date = as.Date("2026-01-10"),
      horizon = 1L,
      target_end_date = as.Date("2026-01-17"),
      target_group = "Overall",
      output_type = "quantile",
      output_type_id = quantile_levels,
      value = round(stats::qnorm(as.numeric(quantile_levels), mean = center, sd = spread), 1)
    )
  }

  df <- dplyr::bind_rows(
    make_member("A", 100, 10),
    make_member("B", 120, 15)
  )

  result <- build_ensemble(df, members = c("A", "B"), method = "linear_pool")

  expect_equal(nrow(result), length(quantile_levels))
  expect_true(all(is.finite(result$value)))
  expect_equal(sort(result$output_type_id), sort(quantile_levels))

  pooled_median <- result$value[result$output_type_id == "0.5"]
  expect_true(pooled_median > 95 && pooled_median < 125)

  ordered <- result[order(as.numeric(result$output_type_id)), ]
  expect_true(all(diff(ordered$value) >= 0))
})

test_that("ensemble_method_label gives readable names", {
  expect_equal(ensemble_method_label("median"), "Median")
  expect_equal(ensemble_method_label("mean"), "Mean")
  expect_equal(ensemble_method_label("linear_pool"), "Linear pool")
})
