#' Identify matches from a matchr image matrix
#'
#' \code{identify_matches} TKTK
#'
#' TKTK
#'
#' @param image_matrix A matrix of image correlations produced by
#' \code{imager::match_images}.
#' @param threshold A numeric scalar. The minimum correlation constant to
#' consider to images to be matched.
#' @return TKTK
#' @export

identify_matches <- function(image_matrix, threshold = 0.99) {

  matches <- which(image_matrix >= threshold, arr.ind = TRUE)
  dimnames(matches)[[2]] <- c("x", "y")

  if (requireNamespace("tibble", quietly = TRUE)) {
    matches <- tibble::as_tibble(matches)
  } else matches <- as.data.frame()

  return(matches)

}
