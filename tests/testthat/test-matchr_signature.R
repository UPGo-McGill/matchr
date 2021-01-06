#### TESTS FOR matchr_signature ################################################

test_that("matchr_signature handles NA", {
  expect_true(suppressWarnings(is.na(create_signature("flkj"))))
})

test_that("matchr_signature objects are printed properly", {
  expect_output(print(test_sig[1]), "aspect ratio")
  expect_output(print(suppressWarnings(create_signature("fdlkj"))), "NA")
  # Disable colour
  col_var <- options(crayon.enabled = FALSE)
  expect_output(print(test_sig[1]), "0.41, 0.44")
  options(col_var)
  # Enable colour
  col_var <- options(crayon.enabled = TRUE)
  expect_true(crayon::has_style(capture.output(print(test_sig[1]))[[2]]))
  options(crayon.enabled = col_var)
})

test_that("length > 1 matchr_signature objects are printed properly", {
  expect_output(print(test_sig), "2 signatures")
})

test_that("matchr_signature objects can be concatenated", {
  expect_output(print(c(test_sig, test_sig)), "4 signatures")
})

test_that("long matchr_signature objects are truncated", {
  expect_output(print(c(test_sig, test_sig, test_sig, test_sig, test_sig, 
                        test_sig, test_sig, test_sig, test_sig, test_sig, 
                        test_sig, test_sig, test_sig)), 
                "with 16 more signatures")
})

test_that("different displays work", {
  expect_output(print(test_sig), ".41", width = 20)
  expect_output(print(test_sig), "a.r.", width = 40)
  expect_output(print(test_sig), "aspect ratio", width = 45)
  expect_output(print(test_sig), "resources", width = 60)
  expect_output(print(test_sig), "aspect ratio", width = 80)
  expect_output(print(test_sig), "https", width = 100)
  expect_output(print(dplyr::tibble(x = test_sig)), "<sig>")
  col_var <- options(crayon.enabled = TRUE)
  expect_true(crayon::has_style(capture.output(print(
    dplyr::tibble(x = rep(test_sig, 15))))[[4]]))
  options(crayon.enabled = col_var)
})

test_that("other methods works", {
  expect_true(is_signature(test_sig))
  expect_output(str(test_sig), "sig")
})
