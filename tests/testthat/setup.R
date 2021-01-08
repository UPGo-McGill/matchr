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
    "http://upgo.lab.mcgill.ca/resources/img_5_grey.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_6.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_6_duplicate.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_7.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_8.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_9.jpg",
    "http://upgo.lab.mcgill.ca/resources/img_10_grey.jpg")


# Create objects that will be used multiple times -------------------------

# load_image
test_img <- load_image("https://upgo.lab.mcgill.ca/resources/img_1_small")
black_image <- 
  new_image(list(array(rep(0, 4800), dim = c(40, 40, 3))), "black_image")
test_na <- suppressWarnings(load_image(
  "https://upgo.lab.mcgill.ca/resources/img_2_corrupt.jpg"))

# create_signature
test_sig <- create_signature(c(test_img, test_img))
test_long_sig <- suppressWarnings(create_signature(urls))

# match_signatures
test_match <- match_signatures(test_long_sig)


# Data to be loaded for tests ---------------------------------------------

load("test-get_clusters.Rdata")
