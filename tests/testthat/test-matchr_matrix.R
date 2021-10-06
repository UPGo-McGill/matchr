#### TESTS FOR matchr_matrix ###################################################

test_that("matchr_matrix objects are printed properly", {
  expect_output(print(test_match), "15 x 15 in 3 matrices")
  expect_output(print(test_match[1]), "6 x 6 in 1 matrix")
})

test_that("length 0 matchr_matrix objects are printed properly", {
  expect_output(print(test_match[0]), "0 matrices")
})

test_that("other methods work", {
  expect_true(is_matrix(test_match))
  expect_output(str(test_match), "matrix")
  expect_equal(sum(as.matrix(test_match[1])), 9008)
  expect_warning(as.matrix(test_match), "Only the first")
})
