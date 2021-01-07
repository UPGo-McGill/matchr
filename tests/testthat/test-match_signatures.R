#### TESTS FOR match_signatures ################################################

test_that("a short single matchr_signature with !compare_aspect_ratios works", {
  r1 <- match_signatures(test_long_sig, compare_aspect_ratios = FALSE)
  expect_equal(attr(r1, "x_total"), sum(sapply(field(r1, "matrix"), nrow), 
                                        length(attr(r1, "x_na"))))
  expect_equal(sum(field(r1, "matrix")[[1]][1:3, 1:3] > 0.999), 9)
  expect_equal(sum(field(r1, "matrix")[[1]][1:3, 4:6] < 0.5), 9)
  expect_equal(sum(length(attr(r1, "x_na")), length(attr(r1, "y_na"))), 4)
})

test_that("a short single matchr_signature with compare_aspect_ratios works", {
  r2 <- match_signatures(test_long_sig)
  expect_equal(attr(r2, "x_total"), sum(sapply(field(r2, "matrix"), nrow), 
                                        length(attr(r2, "x_na"))))
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

