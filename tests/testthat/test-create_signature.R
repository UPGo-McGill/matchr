#### TESTS FOR create_signature ################################################

test_that("a single matchr_image works", {
  expect_equal(ceiling(sum(create_signature(test_img))), 67)
  })

test_that("a vector of matchr_img works", {
  expect_equal(
    ceiling(sum(test_sig)), 133)
})

test_that("a vector of paths works", {
  expect(ceiling(sum(test_long_sig, na.rm = TRUE)) %in% 1129:1132,
         "test_long_sig")
})

test_that("NA works", {
  expect(is.na(create_signature(test_na)), 
         "create_signature(NA) did not return NA.")
})

test_that("rm_black_bars works", {
  expect_output(print(create_signature(urls[13], rm_black_bars = FALSE)),
                "1.00")
  expect_output(print(create_signature(urls[13])), "(2.08)|(2.04)")
  expect(is.na(create_signature(black_image)),
         "create_signature(black_image) did not return NA.")
})

test_that("tiny images return NA", {
  expect(is.na(create_signature(urls[14])), 
         "create_signature(urls[14]) did not return NA.")
})

test_that("backups work", {
  expect_equal(sum(suppressWarnings(create_signature(rep("test", 1200)))), 
               NA_integer_)
  assign("sig_hash", digest::digest(rep("test", 1200)), envir = .matchr_env)
  assign("sig_backup", list(rep(list(list(NA, NA)), 600), NULL), 
         envir = .matchr_env)
  expect_error(sum(suppressWarnings(create_signature(rep("test", 1199)))))
  expect_equal(sum(suppressWarnings(suppressMessages(
    create_signature(rep("test", 1200))))), NA_integer_)
})
