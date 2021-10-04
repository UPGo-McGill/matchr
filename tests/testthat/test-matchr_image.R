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
  expect_message(plot(test_long_img[4:5]), "Only the first")
  expect_message(plot(test_long_img[c(1:4, 6:15)]), "Only the first")
  expect_warning(plot(test_na), "No non-NA")
  skip_on_ci()
  plot_1 <- save_png(plot(test_long_img[8]))
  plot_2 <- save_png(plot(test_long_img[15]))
  plot_3 <- save_png(plot(test_long_img[c(1:3, 5:13)], n_rows = 3))
  expect_snapshot_file(plot_1, "plot_1.png")
  expect_snapshot_file(plot_2, "plot_2.png")
  expect_snapshot_file(plot_3, "plot_3.png")
})
