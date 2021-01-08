#### TESTS FOR remove_backups ##################################################

test_that("sig_backup and sig_hash are removed", {
  assign("sig_backup", test_img, envir = .matchr_env)
  assign("sig_hash", "test", envir = .matchr_env)
  expect_equal(suppressMessages(remove_backups()), c("sig_backup", "sig_hash"))
})

test_that("match_backup and match_hash are removed", {
  assign("match_backup", test_img, envir = .matchr_env)
  assign("match_hash", "test", envir = .matchr_env)
  expect_equal(suppressMessages(remove_backups()), 
               c("match_backup", "match_hash"))
})

test_that("no backups are handled", {
  expect_equal(suppressMessages(remove_backups()), character())
})
