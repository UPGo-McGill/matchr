#### TESTS FOR matchr_img ######################################################

test_that("matchr_img handles NA", {
  expect_true(is.na(test_na))
})

test_that("matchr_img objects are printed properly", {
  expect_output(print(test_img), "640 x 480")
  expect_output(print(test_na[[1]]), "NA")
})
