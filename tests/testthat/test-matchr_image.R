#### TESTS FOR matchr_image ####################################################

test_that("matchr_image handles NA", {
  expect_true(is.na(test_na))
})

test_that("matchr_image objects are printed properly", {
  expect_output(print(test_img), "640 x 480")
  expect_output(print(test_na[[1]]), "NA")
})

test_that("other methods works", {
  expect_true(is_image(test_img))
  expect_output(str(test_img), "image")
})
