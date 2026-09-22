## Tests for detect_data_type() / detect_data_type_by_group() in R/data_utils.R
##
## These pick the DEFAULT position of the Data Type control on upload; the user
## can always override. The interesting cases are the ambiguous ones, since a
## naive "everything in [0, 1] means proportion" rule misclassifies genuine
## count data: a rare outcome in a small jurisdiction, or any respiratory target
## early in the season, produces a column of nothing but 0s and 1s. Integrality
## is what breaks that tie.

test_that("values above 1 are counts", {
  expect_equal(detect_data_type(c(0, 17, 15924)), "count")
  expect_equal(detect_data_type(c(0.5, 1.0001)), "count")
  # 0-100 percentages are NOT microhub proportions, which are strictly 0-1.
  expect_equal(detect_data_type(c(0.5, 42.3, 97)), "count")
})

test_that("non-integer values within [0, 1] are proportions", {
  expect_equal(detect_data_type(c(0.001, 0.04, 0.0132)), "proportion")
  expect_equal(detect_data_type(c(0.3, 0.7)), "proportion")
  # 0.5 is worth pinning: an integrality test written with round() rather than
  # floor() would call this an integer, since R rounds halves to even.
  expect_equal(detect_data_type(0.5), "proportion")
})

test_that("all-integer values within [0, 1] stay counts", {
  # The case the naive range-only rule gets wrong.
  expect_equal(detect_data_type(c(0, 0, 1, 0, 1)), "count")
  expect_equal(detect_data_type(c(0, 0, 0)), "count")
  expect_equal(detect_data_type(c(1, 1, 1)), "count")
  expect_equal(detect_data_type(c(0, 1)), "count")
})

test_that("detect_data_type falls back to the default when it cannot tell", {
  expect_equal(detect_data_type(c(NA, NA)), "count")
  expect_equal(detect_data_type(numeric(0)), "count")
  expect_equal(detect_data_type(NULL), "count")
  # An explicit default is honoured, so a caller can choose the other fallback.
  expect_equal(detect_data_type(numeric(0), default = "proportion"), "proportion")
})

test_that("detect_data_type steers invalid data toward the lenient type", {
  # Negatives are invalid for both types. "count" produces the clearer
  # validate_data() message, and every valid proportion row is also a valid
  # count row, so a wrong guess surfaces as a control to flip rather than as a
  # rejected upload.
  expect_equal(detect_data_type(c(-1, 0.5)), "count")
})

test_that("detect_data_type ignores non-finite values and coerces input", {
  expect_equal(detect_data_type(c(0.4, Inf)), "proportion")
  expect_equal(detect_data_type(c(0.3, NA, 0.7)), "proportion")
  expect_equal(detect_data_type(c("0.3", "0.7")), "proportion")
})

test_that("detect_data_type_by_group classifies each group independently", {
  result <- detect_data_type_by_group(
    values = c(0.1, 0.2, 5, 900, 0, 1),
    groups = c("A", "A", "B", "B", "C", "C")
  )

  expect_type(result, "list")
  expect_setequal(names(result), c("A", "B", "C"))
  expect_equal(result$A, "proportion")
  expect_equal(result$B, "count")
  # Group C is all-integer within [0, 1] -- counts, not proportions.
  expect_equal(result$C, "count")
})

test_that("detect_data_type_by_group handles a single group", {
  result <- detect_data_type_by_group(c(0.2, 0.4), c("only", "only"))

  expect_named(result, "only")
  expect_equal(result$only, "proportion")
})
