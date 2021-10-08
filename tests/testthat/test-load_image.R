#### TESTS FOR load_image ######################################################

test_that("remote images are loaded", {
  expect_true(inherits(test_img, "matchr_image"))
  skip_on_cran()
  expect_true(
    inherits(load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png"),
             "matchr_image"))
  })

test_that("garbage strings produce NA", {
  expect_warning(load_image("fdalkj"))
  expect_true(is.na(suppressWarnings(load_image("fdalkj"))))
  expect_true(is.na(test_na))
})

test_that("a directory path works", {
  skip_on_os("windows")
  skip_on_cran()
  td <- tempdir()
  td_test <- paste0(td, "/matchr_test")
  dir.create(td_test)
  test_paths <- paste0(td_test, "/", seq_along(example_urls[5:6]), ".jpg")
  mapply(download.file, example_urls[5:6], test_paths, 
         MoreArgs = list(quiet = TRUE))
  expect_equal(sum(!is.na(suppressWarnings(load_image(td_test)))), 2)
  unlink(td_test, recursive = TRUE)
})

test_that("multisession futures work", {
  old_plan <- future::plan(future::multisession, workers = 2)
  expect_message(load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png"),
                 "non-parallel")
  old_opt <- options(matchr.force_parallel = TRUE)
  expect_true(inherits(suppressMessages(
    load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png")),
    "matchr_image"))
  future::plan(old_plan)
  options(old_opt)
})

