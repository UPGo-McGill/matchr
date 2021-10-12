#### TESTS FOR remove_black_bars ###############################################

bb_test <- paste0(test_path("resources"), "/", list.files(test_path("resources")))
bb_test <- bb_test[c(13, 16:18)]
bb_test <- load_image(bb_test)

test_that("black bars are removed with both, top or bottom bars", {
  expect_equal(
    remove_black_bars(bb_test[1:3]) |> 
      get_array() |> 
      lapply(dim) |> 
      unlist() |> 
      sum(), 847)
})

test_that("ambiguous images are left alone", {
  expect_equal(
    sapply(get_array(remove_black_bars(bb_test[4])), dim), 
    sapply(get_array(bb_test[4]), dim)
    )
})
