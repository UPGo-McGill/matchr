#### TESTS FOR load_image ######################################################

test_that("remote images are loaded", {
  expect_true(
    inherits(load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png")[[1]],
             "matchr_img"))
  expect_true(inherits(test_img, "matchr_img"))
  })

test_that("garbage strings produce NA", {
  expect_warning(load_image("fdalkj"))
  expect_true(is.na(suppressWarnings(load_image("fdalkj"))))
  expect_true(is.na(test_na))
})



