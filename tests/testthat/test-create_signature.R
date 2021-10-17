#### TESTS FOR create_signature ################################################

test_that("a single matchr_image works", {
  expect_equal(sum(get_hash(create_signature(test_img))[[1]]), 64)
  })

test_that("a vector of matchr_img works", {
  expect_equal(sapply(get_hash(test_sig), sum), c(64, 64))
})

test_that("a vector of paths works", {
  expect_equal(sapply(get_hash(test_long_sig), sum), 
               c(64, 64, 64, NA, rep(64, 9), NA, 64))
})

test_that("NA works", {
  expect(is.na(create_signature(test_na)), 
         "create_signature(NA) did not return NA.")
})

test_that("rm_black_bars works", {
  expect_output(print(create_signature(test_urls[13], rm_black_bars = FALSE)),
                "1.00")
  expect_output(print(create_signature(test_urls[13])), "2.44")
  expect_output(print(create_signature(black_image)), "1.00")
  expect_output(print(create_signature(
    "http://upgo.lab.mcgill.ca/resources/img_8_top.jpg")), "2.53")
  expect_output(print(create_signature(
    "http://upgo.lab.mcgill.ca/resources/img_8_bottom.jpg")), "2.60")
  expect_equal(sum(dim(remove_black_bars(load_image(
    "http://upgo.lab.mcgill.ca/resources/img_8.jpg")))), 285)
  expect_equal(black_image, remove_black_bars(black_image))
})

test_that("tiny images return NA", {
  expect(is.na(create_signature(test_urls[14])), 
         "create_signature(test_urls[14]) did not return NA.")
})

test_that("backups work", {
  expect_equal(sum(is.na(suppressWarnings(
    create_signature(rep("test", 1200))))), 1200)
  assign("sig_hash", rlang::hash(rep("test", 1200)), envir = .matchr_env)
  assign("sig_backup", list(rep(list(list(NA, NA)), 600), NULL), 
         envir = .matchr_env)
  expect_error(sum(suppressWarnings(create_signature(rep("test", 1199)))))
  expect_equal(sum(is.na(suppressWarnings(suppressMessages(
    create_signature(rep("test", 1200)))))), 1200)
})

test_that("cs_internal edge conditions are handled", {
  expect_equal(cs_internal(array(1:20, dim = c(2, 2, 1, 5))), NA)
  expect_equal(cs_internal(array(1:20, dim = c(2, 2, 5))), NA)
  expect_equal(length(cs_internal(array(1:4000, dim = c(100, 40, 1)))), 128)
})