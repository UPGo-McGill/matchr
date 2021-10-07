#### TESTS FOR integrate_changes ###############################################

test_that("the function works", {
  expect_equal(length(test_integrate), 6)
  expect_equal(sum(test_integrate$match, na.rm = TRUE), 3)
  expect_equal(sum(test_integrate_2$match, na.rm = TRUE), 
               sum(test_integrate$match, na.rm = TRUE))
})
