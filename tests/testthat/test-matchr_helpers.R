#### TESTS FOR matchr_helpers ##################################################

test_that("chunk splits vectors correctly", {
  expect_equal(length(1:3711), sum(lengths(chunk(1:3711, 32))))
})

