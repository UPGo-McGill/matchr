# Private environment for tracking state and backups
.matchr_env <- new.env()

# Global variables to satisfy R-CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("sig_backup", "sig_hash", "match_backup",
                           "match_hash", "photos"))
}

# Vector of URLs for examples
test_urls <- 
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