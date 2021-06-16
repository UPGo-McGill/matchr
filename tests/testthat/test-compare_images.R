#### TESTS FOR compare_images ##################################################

test_that("function works", {
  expect_equal(nrow(suppressWarnings(compare_images(test_confirm))), 0)
  expect_equal(nrow(suppressWarnings(compare_images(
    test_confirm, remove_duplicates = FALSE))), 0)
  expect_warning(compare_images(test_confirm), "interactive")
})

test_that("zero-row input works", {
  expect_equal(nrow(compare_images(test_confirm[0,])), 0)
})

# test_that("non-Shiny comparison works", {
#   old_fn <- base::requireNamespace
#   unlockBinding("requireNamespace", as.environment("package:base"))
#   
#   requireNamespace1 <- function(...) FALSE
#   assign("requireNamespace", requireNamespace1, "package:base")
#   suppressMessages(expect_warning(compare_images(test_confirm), "interactive"))
# 
#   requireNamespace2 <- function(x, ...) if (x == "shinyjs") FALSE else TRUE
#   assign("requireNamespace", requireNamespace2, "package:base")
#   expect_error(compare_images(test_confirm), "shinyjs")
#   
#   requireNamespace3 <- function(x, ...) if (x == "waiter") FALSE else TRUE
#   assign("requireNamespace", requireNamespace3, "package:base")
#   expect_error(compare_images(test_confirm), "waiter")
#   
#   assign("requireNamespace", old_fn, "package:base")
#   lockBinding("requireNamespace", as.environment("package:base"))
#   
# })
