#### TESTS FOR confirm_matches #################################################

test_that("function works", {
  expect_equal(nrow(confirm_matches(test_identify)), 6)
  expect_equal(nrow(confirm_matches(test_identify[0,])), 0)
  expect_equal(nrow(confirm_matches(test_identify, remove_duplicates = FALSE)), 
               6)
  expect_warning(confirm_matches(
    test_identify, thresholds = c(10, 100, 150, 200)), "interactive")
})

# test_that("helper functions each work", {
#   test_result <- test_confirm[c(1:5, 6, 6, 6, 6),]
#   field(test_result$x_sig, "path")[7:9] <- c(
#     "test_dir_1/xx.jpg",
#     "test_dir_1/xy.jpg",
#     "test_dir_2/xz.jpg"
#   )
#   paths <- compare_get_paths(test_result)
#   output <- compare_prepare_table(test_result, FALSE, 0.9995)
#   df <- output[[1]]
#   df_all <- output[[2]]
#   df_prev <- output[[3]]
#   df_cor <- output[[4]]
#   expect_equal(sum(sapply(compare_remove_duplicates(df, 0.9995)[1:3], nrow)), 8)
# })
# 
# test_that("zero-row input works", {
#   expect_equal(nrow(compare_images(test_confirm[0,])), 0)
# })
