#### TESTS FOR match_images ####################################################

test_that("non-compare version works", {
  out <- suppressWarnings(match_images(test_urls, compare = FALSE))
  expect_equal(sum(out$distance), 72)
})
