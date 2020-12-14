#### TESTS FOR load_image ######################################################

### Setup ######################################################################

context("load_image tests")


### Tests ######################################################################

test_that("remote images are loaded", {
  expect_true(
    inherits(load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png")[[1]],
             "matchr_img")
    )
  })

test_that("garbage strings produce NA", {
  expect_true(is.na(suppressWarnings(load_image("fdalkj"))))
  expect_warning(load_image("fdalkj"))
})



