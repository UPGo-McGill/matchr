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

test_that("multisession futures work", {
  old_opt <- options(matchr.force_parallel = TRUE)
  old_plan <- future::plan(future::multisession, workers = 2)
  suppressWarnings(rm(par_1, envir = .matchr_env))
  suppressMessages(expect_message(match_signatures(test_long_sig), "parallel"))
  future::plan(old_plan)
  options(old_opt)
})

test_that("backups work", {
  na_sig <- create_signature(test_na)
  input <- c(test_long_sig, rep(na_sig, 986))
  expect_equal(lengths(field(match_signatures(input), "matrix")), c(16, 64, 1))
  assign("match_hash", 
         digest::digest(list(trim_signature(input[!is.na(input)], 1:40), 
                             trim_signature(input[!is.na(input)], 1:40))), 
         envir = .matchr_env)
  assign("match_backup", list(matrix(1:16, nrow = 4), NULL, NULL), 
         envir = .matchr_env)
  expect_error(match_signatures(c(input, input)))
  expect_message(match_signatures(input), "Backup detected")
})

test_that("get_clusters collapses empty vectors", {
  expect_equal(sum(lengths(get_clusters(clust_x, clust_y))), 22)
})

test_that("match_signatures_pairwise works", {
  expect_equal(sum(match_signatures_pairwise(test_long_sig, test_long_sig, 
                                             "grey"), na.rm = TRUE), 13)
})
