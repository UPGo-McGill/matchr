# Private environment for tracking state and backups
.matchr_env <- new.env()

# Global variables to satisfy R-CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("sig_backup", "sig_hash", "match_backup",
                           "match_hash", "photos"))
}


#' Vector of URLs for examples
#' 
#' \code{example_urls} contains 15 file paths for a set of images well suited to
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

example_urls <- 
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

# SVG codes for Shiny app

open_star <- paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" ',
  'fill="currentColor" class="bi bi-star" viewBox="0 0 16 16"><path ',
  'd="M2.866 14.85c-.078.444.36.791.746.593l4.39-2.256 4.389 ',
  '2.256c.386.198.824-.149.746-.592l-.83-4.73 3.522-3.356c.33-.314.16-.', 
  '888-.282-.95l-4.898-.696L8.465.792a.513.513 0 0 0-.927 0L5.354 ',
  '5.12l-4.898.696c-.441.062-.612.636-.283.95l3.523 3.356-.83 ',
  '4.73zm4.905-2.767-3.686 1.894.694-3.957a.565.565 0 0 ',
  '0-.163-.505L1.71 6.745l4.052-.576a.525.525 0 0 0 .393-.288L8 ',
  '2.223l1.847 3.658a.525.525 0 0 0 .393.288l4.052.575-2.906 ',
  '2.77a.565.565 0 0 0-.163.506l.694 3.957-3.686-1.894a.503.503 0 0 ',
  '0-.461 0z"/></svg>')

closed_star <- paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" ',
  'fill="currentColor" class="bi bi-star-fill" viewBox="0 0 16 16">',
  '<path d="M3.612 15.443c-.386.198-.824-.149-.746-.592l.83-4.73L.173 ',
  '6.765c-.329-.314-.158-.888.283-.95l4.898-.696L7.538.792c.197-.39.73-.',
  '39.927 0l2.184 4.327 4.898.696c.441.062.612.636.282.95l-3.522 ',
  '3.356.83 4.73c.078.443-.36.79-.746.592L8 13.187l-4.389 2.256z"/></svg>'
)
