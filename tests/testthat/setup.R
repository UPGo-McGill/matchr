### MATCHR TEST SETUP ##########################################################

# URLs to test ------------------------------------------------------------

urls <-
  c("http://upgo.lab.mcgill.ca/resources/img_1.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_1_compressed.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_1_small.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_2_corrupt.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_3.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_4.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_4_large.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_5.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_6.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_6_duplicate.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_7.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_8.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_9.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_10_grey.jpg")


# Create objects that will be used multiple times -------------------------

test_img <- load_image("https://upgo.lab.mcgill.ca/resources/img_1")[[1]]

test_na <- suppressWarnings(load_image(
  "https://upgo.lab.mcgill.ca/resources/img_2_corrupt.jpg"))

test_matchr_sig_list <- create_signature(list(test_img, test_img))
test_long_sig_list <- suppressWarnings(create_signature(urls))
