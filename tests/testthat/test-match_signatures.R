#### TESTS FOR match_signatures ################################################

test_that("a short single matchr_signature with !compare_aspect_ratios works", {
  result <- match_signatures(test_long_sig, compare_aspect_ratios = FALSE)
  expect_equal(attr(result, "x_total"), 14)
  expect_equal(sum(field(result, "matrix")[[1]][1:3, 1:3] > 0.999), 9)
  expect_equal(sum(field(result, "matrix")[[1]][1:3, 5:7] < 0.5), 9)
  expect(is.na(field(result, "matrix")[[1]][1, 4]), "match_signature NA.")
})

# test_that("a long single matchr_signature works", {
#   expect_equal(
#     sum(lengths(
#       match_signatures(rep(test_long_sig[5:12], 5))
#       )), 77)
# })
# 
# test_that("a pair of matchr_signature vectors works", {
#   expect_equal(
#     sum(lengths(match_signatures(test_long_sig[5:12]))), 77)
# })
# 
# 

