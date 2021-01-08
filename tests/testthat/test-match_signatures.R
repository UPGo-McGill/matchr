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
  expect_equal(attr(test_match, "x_total"), 
               sum(sapply(field(test_match, "matrix"), nrow), 
                   length(attr(test_match, "x_na"))))
})

test_that("colour works", {
  r2 <- match_signatures(test_long_sig[8:9])
  expect_equal(sum(field(r2, "matrix")[[1]] > 0.99), 4)
  r3 <- match_signatures(test_long_sig[8:9], method = "colour")
  expect_equal(sum(field(r3, "matrix")[[1]] > 0.99), 2)
})

test_that("a pair of matchr_signature vectors works", {
  r4 <- match_signatures(test_long_sig, test_long_sig)
  expect_equal(r4, test_match)
})

# test_that("a long single matchr_signature works", {
#   expect_equal(
#     sum(lengths(
#       match_signatures(rep(test_long_sig[5:12], 5))
#       )), 77)
# })
# 
# 

test_that("backups work", {
  assign("sig_hash", digest::digest(rep("test", 1200)), envir = .matchr_env)
  assign("sig_backup", list(rep(list(list(NA, NA)), 600), NULL), 
         envir = .matchr_env)
  expect_error(sum(suppressWarnings(create_signature(rep("test", 1199)))))
  expect_equal(sum(suppressWarnings(suppressMessages(
    create_signature(rep("test", 1200))))), NA_integer_)
})

