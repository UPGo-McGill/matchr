#### TESTS FOR plot METHODS ####################################################

save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code
  
  path
}

test_that("matchr_image plots work", {
  print(test_long_sig)
  expect_message(plot(test_long_img[4:5]), "Only the first")
  expect_message(plot(test_long_img[c(1:4, 6:15)]), "Only the first")
  expect_warning(plot(test_na), "No non-NA")
  skip_on_ci()
  plot_img_1 <- save_png(plot(test_long_img[8]))
  plot_img_2 <- save_png(plot(test_long_img[15]))
  plot_img_3 <- save_png(plot(test_long_img[c(1:3, 5:13)], n_rows = 3))
  expect_snapshot_file(plot_img_1, "plot_img_1.png")
  expect_snapshot_file(plot_img_2, "plot_img_2.png")
  expect_snapshot_file(plot_img_3, "plot_img_3.png")
})

test_that("matchr_signature plots work", {
  expect_message(plot(test_long_sig[4:5]), "Only the first")
  expect_message(plot(test_long_sig[c(1:4, 6:15)]), "Only the first")
  expect_warning(plot(create_signature(test_na)), "No non-NA signatures")
  skip_on_ci()
  plot_sig_1 <- save_png(plot(test_long_sig[8]))
  plot_sig_2 <- save_png(plot(test_long_sig[15]))
  plot_sig_3 <- save_png(plot(test_long_sig[5:10], n_rows = 3))
  expect_snapshot_file(plot_sig_1, "plot_sig_1.png")
  expect_snapshot_file(plot_sig_2, "plot_sig_2.png")
  expect_snapshot_file(plot_sig_3, "plot_sig_3.png")
})
