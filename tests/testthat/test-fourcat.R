source(test_path("../../R/FourCAT.R"))

test_that("FourCAT target-group extraction handles raw and delimited grouping keys", {
  expect_equal(
    fourcat_extract_target_group(c("Overall", "target_group||Adults", "Children")),
    c("Overall", "Adults", "Children")
  )
})
