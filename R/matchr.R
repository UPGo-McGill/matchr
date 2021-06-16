# Private environment for tracking state and backups
.matchr_env <- new.env()

# Global variables to satisfy R-CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("sig_backup", "sig_hash", "match_backup",
                           "match_hash", "photos"))
}


#' Vector of URLs for examples
#' 
#' \code{test_urls} contains 15 URLs pointing at a set of images well suited to
#' exploring the functionality of the matchr package. Elements 1-3 are the same
#' image at different resolutions and compression levels. Element 4 is a corrupt
#' image. Element 5 is a unique image. Elements 6-7 are the same image at
#' different resolutions. Elements 8-9 are the same image in colour and 
#' greyscale. Elements 10-11 are byte-for-byte identical images. Element 12 is a
#' portrait-orientation image. Element 13 is an image with large horizontal
#' black bars. Element 14 is a completely grey image. Element 15 is a
#' non-photographic, black-and-white image.
#' 
#' @export

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