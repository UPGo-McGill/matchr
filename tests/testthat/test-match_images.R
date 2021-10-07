#### TESTS FOR match_images ####################################################

test_that("non-compare version works", {
  out <- suppressWarnings(match_images(test_urls, compare = FALSE))
  # This gives different results on Windows/Linux (64) and macOS (72)
  expect(sum(out$distance) %in% c(64, 72), "match_images doesn't work")
})
