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

  # Find matches
  matches <- which(image_matrix >= threshold, arr.ind = TRUE)

  # Name results
  dimnames(matches)[[2]] <- c("x_index", "y_index")

  # Convert to tibble or data frame
  if (requireNamespace("tibble", quietly = TRUE)) {
    matches <- tibble::as_tibble(matches)
  } else matches <- as.data.frame()

  # Add names
  matches$x_name <- rownames(image_matrix)[matches$x_index]
  matches$y_name <- colnames(image_matrix)[matches$y_index]

  # Arrange output
  matches <- matches[order(matches$x_index, matches$y_index),]

  # Remove redundant matches if the matrix is generated from a single list
  if (dim(image_matrix)[[1]] == dim(image_matrix)[[2]]) {
    if (mean(rownames(image_matrix) == colnames(image_matrix)) == 1) {
      # Remove self matches
      matches <- matches[matches$x_index != matches$y_index,]

      # Remove duplicate matches
      pairs <-
        mapply(function(x, y) c(x, y)[order(c(x, y))],
               matches$x_index,
               matches$y_index,
               SIMPLIFY = FALSE)

      matches <- matches[!duplicated(pairs),]
    }
  }

  return(matches)

}
