#### TESTS FOR identify_matches ################################################

test_that("the function works", {
  expect_equal(vec_size(test_identify), 6)
  expect_equal(vec_size(identify_matches(test_long_sig)), 6)
  expect_equal(vec_size(identify_matches(test_long_sig, test_long_sig)), 6)
})

test_that("y is NULL if x is matchr_matrix", {
  expect_error(identify_matches(test_match, y = test_match), "a `y` argument")
})

test_that("tibble integration works", {
  expect_s3_class(test_identify, "tbl_df")

  old_fn <- base::requireNamespace
  requireNamespace1 <- function(...) FALSE
  unlockBinding("requireNamespace", as.environment("package:base"))
  assign("requireNamespace", requireNamespace1, "package:base")

  expect(!inherits(identify_matches(test_match), "tbl_df"), "tbl_df class")
  assign("requireNamespace", old_fn, "package:base")
  lockBinding("requireNamespace", as.environment("package:base"))

})
