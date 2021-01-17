#### TESTS FOR matchr_image ####################################################

save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code
  
  path
}

test_that("matchr_image handles NA", {
  expect_true(is.na(test_na))
})

test_that("matchr_image objects are printed properly", {
  expect_output(print(test_img), "300 x 225")
  expect_output(print(test_na), "NA")
})

test_that("other methods works", {
  expect_true(is_image(test_img))
  expect_output(str(test_img), "image")
})

test_that("plots work", {
  skip_on_cran()
  plot_1 <- save_png(plot(load_image(urls[8])))
  plot_2 <- save_png(plot(load_image(urls[15])))
  expect_snapshot_file(plot_1, "plot_1.png")
  expect_snapshot_file(plot_2, "plot_2.png")
})
