#### TESTS FOR download_images #################################################

test_that("the function works", {
  skip_if_offline()
  err_sum <- sum(sapply(urls, httr::http_error)) == 0
  if (err_sum) {
    url_groups <- list(urls[1:3], urls[4], urls[5], urls[6:7], urls[8:9], 
                       urls[10:11], urls[12], urls[13], urls[14], urls[15],
                       "fdlskajfd")
    di_df <- data.frame(id = 1:11)
    di_df$photos <- url_groups
    temp_dir <- tempdir(check = TRUE)
    test_download_1 <- download_images(di_df[1,  ], temp_dir, quiet = TRUE)
    test_download_2 <- download_images(di_df[1:2,], temp_dir, quiet = TRUE)
    test_download_3 <- download_images(di_df[1:2,], temp_dir, quiet = TRUE)
    test_download_4 <- download_images(di_df[11, ], temp_dir, quiet = TRUE)
    expect_equal(nrow(test_download_1), 1)
    expect_equal(nrow(test_download_2), 2)
    expect_equal(nrow(test_download_3), 2)
    expect_equal(nrow(test_download_4), 1)
    # expect_equal(sum(test_download_1$result == "success"), 1)
    expect_equal(sum(test_download_2$result == "success"), 1)
    expect_equal(sum(test_download_3$result == "success"), 0)
    expect_equal(sum(test_download_4$result == "success"), 0)
    expect_equal(sum(test_download_1$result == "duplicate"), 0)
    # expect_equal(sum(test_download_2$result == "duplicate"), 1)
    # expect_equal(sum(test_download_3$result == "duplicate"), 2)
    expect_equal(sum(test_download_4$result == "duplicate"), 0)
    # expect_equal(sum(test_download_1$result == "error"), 0)
    # expect_equal(sum(test_download_2$result == "error"), 0)
    # expect_equal(sum(test_download_3$result == "error"), 0)
    expect_equal(sum(test_download_4$result == "error"), 1)
  }
})
