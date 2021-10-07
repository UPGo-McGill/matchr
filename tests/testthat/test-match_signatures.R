#### TESTS FOR match_signatures ################################################

test_that("a short single matchr_signature with !compare_ar works", {
  r1 <- match_signatures(test_long_sig, compare_ar = FALSE)
  expect_equal(attr(r1, "x_total"), sum(sapply(get_array(r1), nrow),
                                        length(attr(r1, "x_na"))))
  expect_equal(sum(get_array(r1)[[1]][1:3, 1:3] == 0), 5)
  expect_equal(sum(get_array(r1)[[1]][1:3, 4:6] < 1000), 1)
  expect_equal(sum(length(attr(r1, "x_na")), length(attr(r1, "y_na"))), 4)
})

test_that("a short single matchr_signature with compare_aspect_ratios works", {
  expect_equal(attr(test_match, "x_total"),
               sum(unlist(sapply(get_array(test_match), nrow)),
                   length(attr(test_match, "x_na"))))
})

test_that("a pair of matchr_signature vectors works", {
  r4 <- match_signatures(test_long_sig, test_long_sig)
  expect_equal(r4, test_match)
})

test_that("NA inputs work", {
  expect_equal(attr(match_signatures(suppressWarnings(
    create_signature(c("a", "b")))), "x_na"), c("a", "b"))
})

# test_that("multisession futures work", {
#   old_opt <- options(matchr.blas = FALSE)
#   old_plan <- future::plan(future::multisession, workers = 2)
#   suppressWarnings(rm(par_1, envir = .matchr_env))
#   expect_equal(suppressMessages(match_signatures(test_long_sig)), test_match)
#   # Test identify_matches while we're at it
#   expect_equal(suppressMessages(identify_matches(test_long_sig)), test_confirm)
#   future::plan(old_plan)
#   options(old_opt)
# })

test_that("get_clusters collapses empty vectors", {
  expect_equal(sum(lengths(get_clusters(clust_x, clust_y, max_clust = 17))), 32)
  expect_equal(sum(lengths(get_clusters(test_long_sig[1:2],
                                        test_long_sig[1:2]))), 2)
})

test_that("match_signatures_pairwise works", {
  expect_equal(sum(match_signatures_pairwise(test_long_sig, test_long_sig), 
                   na.rm = TRUE), 0)
  # This gives different results on Windows/Linux (64) and macOS (72)
  expect(sum(match_signatures_pairwise(
    test_identify$x_sig, test_identify$y_sig)) %in% c(64, 72),
    "match_signatures_pairwise doesn't work")
})
