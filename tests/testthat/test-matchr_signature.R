#### TESTS FOR matchr_signature ################################################

save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code
  
  path
}

test_that("matchr_signature handles NA", {
  expect_true(suppressWarnings(is.na(create_signature("flkj"))))
})

test_that("matchr_signature objects are printed properly", {
  expect_output(print(test_sig[1]), "aspect ratio")
  expect_output(print(suppressWarnings(create_signature("fdlkj"))), "NA")
})

test_that("length 0 matchr_signature objects are printed properly", {
  expect_output(print(test_sig[0]), "0 signatures")
})

test_that("length > 1 matchr_signature objects are printed properly", {
  expect_output(print(test_sig), "2 signatures")
})

test_that("matchr_signature objects can be concatenated", {
  expect_output(print(c(test_sig, test_sig)), "4 signatures")
})

test_that("long matchr_signature objects are truncated", {
  expect_output(print(c(test_sig, test_sig, test_sig, test_sig, test_sig, 
                        test_sig, test_sig, test_sig, test_sig, test_sig, 
                        test_sig, test_sig, test_sig)), 
                "with 16 more signatures")
})

test_that("different displays work", {
  expect_output(print(test_sig), "c$", width = 20)
  expect_output(print(test_sig), "a.r.", width = 35)
  expect_output(print(test_sig), "aspect ratio", width = 45)
  expect_output(print(test_sig), "ces", width = 60)
  expect_output(print(test_sig), "aspect ratio", width = 80)
  expect_output(print(test_sig), "https", width = 100)
  expect_output(print(dplyr::tibble(x = test_sig)), "<sig>")
  expect_output(print(dplyr::tibble(a = "fdsalkjfdslakj", x = test_sig)), "88…", 
                width = 30)
})

test_that("other methods works", {
  expect_true(is_signature(test_sig))
  expect_output(str(test_sig), "sig")
})

test_that("a single NA is printed correctly", {
  expect_output(print(test_long_sig[4], width = 20), "[^a.r.]")
  expect_output(print(test_long_sig[4], width = 31), "a.r.")
  expect_output(print(test_long_sig[4], width = 39), "aspect")
  expect_output(print(test_long_sig[4], width = 50), "a.r.")
  expect_output(print(test_long_sig[4], width = 69), ", (tests)|(resources)")
})

test_that("empty signatures print correctly", {
  expect_output(print(trim_hash(test_long_sig, 0)), "NULL")
})

test_that("concatenation works", {
  expect_equal(length(c(test_long_sig, test_long_sig)), 30L)
  expect_error(c(test_long_sig, test_long_sig, recursive = TRUE), "recursive")
  expect_error(c(test_long_sig, test_long_sig, use.names = FALSE), "use.names")
})
