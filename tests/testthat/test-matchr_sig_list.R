#### TESTS FOR matchr_sig_list #################################################

test_that("matchr_sig_list objects are printed properly", {
  expect_output(print(test_matchr_sig_list), "2 signatures")
})

test_that("matchr_sig_list objects can be concatenated", {
  expect_output(print(c(test_matchr_sig_list, test_matchr_sig_list)),
                "4 signatures")
})
