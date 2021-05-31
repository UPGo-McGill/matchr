#### TESTS FOR integrate_changes ###############################################

test_that("the function works", {
  expect_equal(length(test_integrate), 9)
  expect_equal(sum(test_integrate$match == "no match"), 1)
  expect_equal(sum(test_integrate$confirmed), 6)
  expect_equal(test_integrate, test_integrate_2)
})
