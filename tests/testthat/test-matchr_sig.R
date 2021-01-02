#### TESTS FOR matchr_sig ######################################################

test_that("matchr_img handles NA", {
  expect_true(is.na(create_signature(TRUE)))
})

test_that("match_sig objects are printed properly", {
  expect_output(print(test_matchr_sig_list[[1]]), "Aspect ratio")
  expect_output(print(create_signature(TRUE)), "NA")
  # Disable colour
  expect_output({
    col_var <- options(crayon.enabled = FALSE)
    print(test_matchr_sig_list[[1]])
    options(col_var)
  }, "0.53, 0.49")
  # Enable colour
  expect_true({
    col_var <- getOption("crayon.enabled")
    options(crayon.enabled = TRUE)
    val <-
      crayon::has_style(capture.output(print(test_matchr_sig_list[[1]]))[[2]])
    options(crayon.enabled = col_var)
    val})
})


