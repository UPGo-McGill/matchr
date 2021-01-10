#### TESTS FOR confirm_matches #################################################

test_that("the function works", {
  expect_equal(length(test_confirm), 7)
  test_id_2 <- test_identify
  test_id_2[1,]$correlation <- 0.98
  expect_equal(length(confirm_matches(test_id_2, test_long_sig, test_long_sig)), 
               7)
  expect_equal(length(confirm_matches(test_id_2)), 7)
})

test_that("tibble integration works", {
  expect_s3_class(test_confirm, "tbl_df")

  old_fn <- base::requireNamespace
  requireNamespace1 <- function(...) FALSE
  unlockBinding("requireNamespace", as.environment("package:base"))
  assign("requireNamespace", requireNamespace1, "package:base")
  test_id_2 <- test_identify
  test_id_2[1,]$correlation <- 0.98
  expect_s3_class(confirm_matches(as.data.frame(test_id_2), test_long_sig, 
                                  test_long_sig), "data.frame", exact = TRUE)
  assign("requireNamespace", old_fn, "package:base")
  lockBinding("requireNamespace", as.environment("package:base"))

})
