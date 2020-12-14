#### TESTS FOR matchr_img ######################################################

### Setup ######################################################################


### Tests ######################################################################

test_that("matchr_img handles NA", {
  expect_true(is.na(test_na))
  # expect_warning(load_image("fdalkj"))
  # expect_true(is.na(suppressWarnings(load_image("fdalkj"))))
  # expect_true(is.na(suppressWarnings(load_image(
  #   "https://upgo.lab.mcgill.ca/resources/img_2_corrupt.jpg"))))
})

test_that("match_img objects are printed properly", {
  expect_output(print(test_img), "Image. Width: 640 pix Height: 480 pix")
  expect_output(print(test_na[[1]]), "NA")
})




