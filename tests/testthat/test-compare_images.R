#### TESTS FOR compare_images ##################################################

test_that("function works", {
  expect_equal(nrow(suppressWarnings(compare_images(test_confirm))), 0)
  expect_equal(nrow(suppressWarnings(compare_images(
    test_confirm, remove_duplicates = FALSE))), 0)
  expect_warning(compare_images(test_confirm), "interactive")
})

test_that("helper functions each work", {
  test_result <- test_confirm[c(1:5, 6, 6, 6, 6),]
  field(test_result$x_sig, "path")[7:9] <- c(
    "test_dir_1/xx.jpg",
    "test_dir_1/xy.jpg",
    "test_dir_2/xz.jpg"
  )
  paths <- compare_get_paths(test_result)
  output <- compare_prepare_table(test_result, FALSE, 0.9995)
  df <- output[[1]]
  df_all <- output[[2]]
  df_prev <- output[[3]]
  df_cor <- output[[4]]
  expect_equal(sum(sapply(compare_remove_duplicates(df, 0.9995)[1:3], nrow)), 8)
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
