#### TESTS FOR load_image ######################################################

test_that("remote images are loaded", {
  expect_true(
    inherits(load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png"),
             "matchr_image"))
  expect_true(inherits(test_img, "matchr_image"))
  })

test_that("garbage strings produce NA", {
  expect_warning(load_image("fdalkj"))
  expect_true(is.na(suppressWarnings(load_image("fdalkj"))))
  expect_true(is.na(test_na))
})

test_that("a directory path works", {
  skip_on_os("windows")
  td <- tempdir()
  td_test <- paste0(td, "/matchr_test")
  dir.create(td_test)
  test_paths <- paste0(td_test, "/", seq_along(test_urls[1:5]), ".jpg")
  mapply(download.file, test_urls[1:5], test_paths, 
         MoreArgs = list(quiet = TRUE))
  expect_equal(sum(!is.na(suppressWarnings(load_image(td_test)))), 4)
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

