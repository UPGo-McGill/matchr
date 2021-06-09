# Private environment for tracking state and backups
.matchr_env <- new.env()

# Global variables to satisfy R-CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("sig_backup", "sig_hash", "match_backup",
                           "match_hash", "photos"))
}

# Vector of URLs for examples
test_urls <- 
  c("https://upgo.lab.mcgill.ca/resources/img_1.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_1_compressed.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_1_small.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_2_corrupt.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_3.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_4.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_4_large.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_5.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_5_grey.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_6.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_6_duplicate.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_7.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_8.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_9.jpg",
    "https://upgo.lab.mcgill.ca/resources/img_10_grey.jpg")