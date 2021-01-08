#### TESTS FOR matchr_matrix ###################################################

test_that("matchr_matrix objects are printed properly", {
  expect_output(print(test_match), "15 x 15 in 3 matrices")
  expect_output(print(test_match[1]), "6 x 6 in 1 matrix")
})



# test_that("matchr_signature handles NA", {
#   expect_true(suppressWarnings(is.na(create_signature("flkj"))))
# })
# 

# test_that("length > 1 matchr_signature objects are printed properly", {
#   expect_output(print(test_sig), "2 signatures")
# })
# 
# test_that("matchr_signature objects can be concatenated", {
#   expect_output(print(c(test_sig, test_sig)), "4 signatures")
# })
# 
# test_that("long matchr_signature objects are truncated", {
#   expect_output(print(c(test_sig, test_sig, test_sig, test_sig, test_sig, 
#                         test_sig, test_sig, test_sig, test_sig, test_sig, 
#                         test_sig, test_sig, test_sig)), 
#                 "with 16 more signatures")
# })
# 
# test_that("different displays work", {
#   expect_output(print(test_sig), ".41", width = 20)
#   expect_output(print(test_sig), "a.r.", width = 40)
#   expect_output(print(test_sig), "aspect ratio", width = 45)
#   expect_output(print(test_sig), "ces", width = 60)
#   expect_output(print(test_sig), "aspect ratio", width = 80)
#   expect_output(print(test_sig), "https", width = 100)
#   expect_output(print(dplyr::tibble(x = test_sig)), "<sig>")
#   col_var <- options(crayon.enabled = TRUE)
#   expect_true(crayon::has_style(capture.output(print(
#     dplyr::tibble(x = rep(test_sig, 15))))[[4]]))
#   options(crayon.enabled = col_var)
# })

test_that("other methods works", {
  expect_true(is_matrix(test_match))
  expect_output(str(test_match), "matrix")
})

# test_that("a single NA is printed correctly", {
#   expect_output(print(test_long_sig[4], width = 20), "NA\\\033")
#   expect_output(print(test_long_sig[4], width = 40), "NA\\\033")
#   expect_output(print(test_long_sig[4], width = 45), "NA\\\033")
#   expect_output(print(test_long_sig[4], width = 50), "...ab")
#   expect_output(print(test_long_sig[4], width = 60), "\\(http")
# })
