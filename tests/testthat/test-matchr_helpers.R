#### TESTS FOR matchr_helpers ##################################################

test_that("chunk splits vectors correctly", {
  expect_equal(length(1:3711), sum(lengths(chunk(1:3711, 32))))
})

test_that("package absences are handled gracefully", {
  old_fn <- base::requireNamespace
  myrequireNamespace <- function(...) FALSE
  unlockBinding("requireNamespace", as.environment("package:base"))
  assign("requireNamespace",myrequireNamespace, "package:base")
  
  expect_true(
    inherits(load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png"),
             "matchr_image"))
  expect_equal(ceiling(sum(create_signature(test_img))), 67)
  r1 <- match_signatures(test_long_sig, compare_aspect_ratios = FALSE)
  expect_equal(attr(r1, "x_total"), sum(sapply(field(r1, "matrix"), nrow), 
                                        length(attr(r1, "x_na"))))
  
  assign("requireNamespace",old_fn, "package:base")
  lockBinding("requireNamespace", as.environment("package:base"))
  rm(old_fn, myrequireNamespace)
  
  
})