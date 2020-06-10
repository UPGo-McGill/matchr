#' Generate a unique colour signature for an image
#'
#' \code{identify_image} takes an image file and generates a numeric vector of
#' average colour values, so that the image can be compared to others.
#'
#' The function works as follows. An image is decomposed into horizontal bands
#' (with the number of bands controlled by the `bands` argument), and for each
#' band an average colour is calculated. The vector of these colour averages
#' becomes a distinctive signature that can identify a given image even if the
#' image is rescaled or compressed, and thus serves as a reliable indicator of
#' whether two images are the same.
#'
#' @param image Object of class 'cimg', probably imported using
#' \code{load_image} or \code{imager::load.image}, or file path or URL of an
#' image.
#' @param bands Integer scalar. The number of horizontal bands the image should
#' be split into for processing. Higher values will produce a more distinctive
#' colour signature, potentially decreasing the rate of matching false
#' positives, but at the cost of increased processing time and an increased rate
#' of matching false negatives.
#' @return A numeric vector of length `bands`.
#' @export

identify_image <- function(image, bands = 25) {

  # If `image` is NA, return a vector of NAs
  if (is.na(image)) return(rep(NA, times = bands))

  # If `image` isn't class `cimg`, try to load it as path instead
  if (!inherits(image, "cimg")) image <- load_image(image)

  image_split <- imager::imsplit(image, "x", bands)
  image_means <- lapply(image_split, rowMeans)
  image_means <- sapply(image_means, mean)

  return(image_means)

}
