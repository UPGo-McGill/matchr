#### TESTS FOR match_signatures ################################################

test_that("a short single matchr_signature with !compare_aspect_ratios works", {
  expect_equal(attr(match_signatures(
    test_long_sig[5:12], compare_aspect_ratios = FALSE), "x_total"), 8)
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
# test_that("NA works", {
#   expect_equal(
#     sum(lengths(match_signatures(test_long_sig[5:12]))), 77)
# })
# 
