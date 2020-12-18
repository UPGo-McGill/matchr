#### TESTS FOR create_signature ################################################

test_that("a single matchr_img works", {
  expect_equal(
    ceiling(sum(create_signature(test_img))), 67)
  })

test_that("a single cimg works", {
  expect_equal(
    ceiling(sum(create_signature(test_cimg))), 67)
})

test_that("a list of matchr_img works", {
  expect_equal(
    ceiling(sum(sapply(test_matchr_sig_list, sum))), 133)
})

test_that("a list of one matchr_img works", {
  expect_type(create_signature(list(test_img)), "list")
})

test_that("a vector of paths works", {
  expect_equal(
    ceiling(sum(sapply(test_long_sig_list, sum), na.rm = TRUE)), 871)
})

test_that("a list of paths works", {
  expect_equal(
    ceiling(sum(sapply(create_signature(list(urls[[1]], urls[[1]])), sum))),
    133)
})

test_that("NA works", {
  expect(is.na(create_signature(test_na)[[1]]),
         "create_signature(NA) did not return NA.")
})

test_that("rm_black_bars works", {
  expect_output(print(create_signature(urls[[12]], rm_black_bars = FALSE)),
                "1.00")
  expect_output(print(create_signature(urls[[12]])), "1.82")
})

test_that("tiny images return NA", {
  expect_output(print(create_signature(urls[[13]])), "NA")
})

