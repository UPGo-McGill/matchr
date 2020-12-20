#### TESTS FOR remove_backups ##################################################

test_that("sig_backup is removed", {
  assign("sig_backup", test_img, envir = .matchr_env)
  expect_equal(remove_backups(), "sig_backup")
})

test_that("no backups are handled", {
  expect_equal(remove_backups(), character())
})


test <- remove_backups()