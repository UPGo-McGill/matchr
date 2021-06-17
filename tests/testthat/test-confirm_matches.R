#### TESTS FOR confirm_matches #################################################

test_that("the function works", {
  expect_equal(length(test_confirm), 5)
  test_id_2 <- test_identify
  test_id_2[1,]$correlation <- 0.98
  expect_equal(length(confirm_matches(test_id_2)), 5)
})
