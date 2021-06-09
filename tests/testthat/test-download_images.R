#### TESTS FOR download_images #################################################

test_that("the function works", {
  skip_if_offline()
  url_groups <- list(test_urls[1:3], test_urls[4], test_urls[5], test_urls[6:7], 
                     test_urls[8:9], test_urls[10:11], test_urls[12], 
                     test_urls[13], test_urls[14], test_urls[15], "fdlskajfd")
  di_df <- data.frame(id = 1:11)
  di_df$photos <- url_groups
  temp_dir <- paste0(tempdir(check = TRUE), "/down_test")
  dir.create(temp_dir)
  test_download_1 <- download_images(di_df[1,  ], temp_dir, quiet = TRUE)
  test_download_2 <- download_images(di_df[1:2,], temp_dir, quiet = TRUE)
  test_download_3 <- download_images(di_df[1:2,], temp_dir, quiet = TRUE)
  test_download_4 <- download_images(di_df[11, ], temp_dir, quiet = TRUE)
  test_download_5 <- download_images(NULL, temp_dir, url_groups[1],
                                     1, quiet = TRUE)
  test_output <- capture.output(download_images(di_df[1,  ], temp_dir),
                                type = "output")
  expect_equal(nrow(test_download_1), 1)
  expect_equal(nrow(test_download_2), 2)
  expect_equal(nrow(test_download_3), 2)
  expect_equal(nrow(test_download_4), 1)
  expect_equal(nrow(test_download_5), 1)
  # expect_equal(sum(test_download_1$result == "success"), 1)
  # expect_equal(sum(test_download_2$result == "success"), 1)
  # expect_equal(sum(test_download_3$result == "success"), 0)
  # expect_equal(sum(test_download_4$result == "success"), 0)
  # expect_equal(sum(test_download_1$result == "duplicate"), 0)
  # expect_equal(sum(test_download_2$result == "duplicate"), 1)
  # expect_equal(sum(test_download_3$result == "duplicate"), 2)
  # expect_equal(sum(test_download_4$result == "duplicate"), 0)
  # expect_equal(sum(test_download_1$result == "error"), 0)
  # expect_equal(sum(test_download_2$result == "error"), 0)
  # expect_equal(sum(test_download_3$result == "error"), 0)
  # expect_equal(sum(test_download_4$result == "error"), 1)
  expect_equal(length(test_output), 3)
  unlink(temp_dir, recursive = TRUE)
})
