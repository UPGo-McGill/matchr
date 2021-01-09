#### TESTS FOR matchr_helpers ##################################################

test_that("chunk splits vectors correctly", {
  expect_equal(length(1:3711), sum(lengths(chunk(1:3711, 32))))
})

test_that("package absences are handled gracefully", {
  old_fn <- base::requireNamespace
  requireNamespace1 <- function(...) FALSE
  unlockBinding("requireNamespace", as.environment("package:base"))
  assign("requireNamespace", requireNamespace1, "package:base")
  
  suppressMessages(expect_message(.onAttach(), "Install"))
  expect_equal(number_of_threads(), 1)
  expect_true(
    inherits(load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png"),
             "matchr_image"))
  expect_equal(ceiling(sum(create_signature(test_img))), 67)
  r1 <- match_signatures(test_long_sig, compare_aspect_ratios = FALSE)
  expect_equal(attr(r1, "x_total"), sum(sapply(field(r1, "matrix"), nrow), 
                                        length(attr(r1, "x_na"))))
  
  requireNamespace2 <- function(x, ...) {
    if (x %in% c("crayon", "future.apply")) FALSE else TRUE
  }
  assign("requireNamespace", requireNamespace2, "package:base")
  expect_message(par_lapply(1:3, function(x) x), "apply")
  expect_equal(length(handler_matchr("test")), 1)
  
  assign("requireNamespace", old_fn, "package:base")
  lockBinding("requireNamespace", as.environment("package:base"))

})

test_that("future.globals are set and unset correctly", {
  fut_var <- getOption("future.globals.maxSize")
  .onLoad()
  expect_equal(getOption("future.globals.maxSize"), +Inf)
  .onUnload()
  expect_equal(getOption("future.globals.maxSize"), fut_var)
})
