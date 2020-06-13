#' Match images based on colour signatures
#'
#' \code{match_signatures} takes one or two lists of images and produces a
#' correlation matrix to identify matches.
#'
#' A function for identifying matching images. The function takes a list of
#' images (objects of class 'cimg) and compares their colour signatures to find
#' matches.
#'
#' The comparison is done by creating colour signatures for each input image
#' using \code{\link{identify_image}} and then computing the Pearson correlation
#' coefficient between these signatures. In general, pairs of images which were
#' identical prior to arbitrary resampling and compression will have correlation
#' coefficients of at least 0.99.
#'
#' @param x,y Lists of 'cimg' objects to be matched, or file paths and URLs
#' pointing to images which can be imported as 'cimg' objects. If `y` is missing
#' (default), each object in `x` will be matched against each other object in
#' `x.` If `y` is present, each object in `x` will be matched against each
#' object in `y`.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A correlation matrix. If `x` and `y` are both present, the matrix
#' will have `length(x)` rows and `length(y)` columns, and for the matrix `Q`
#' the cell `Q[i, j]` will be the Pearson correlation coefficient between images
#' `x[[i]]` and `y[[j]]`. If `y` is not present, the matrix will be square, and
#' the cell `Q[i, j]` will be the correlation between images `x[[i]]` and
#' `x[[j]]`.
#' @export

match_signatures <- function(x, y = NULL, quiet = FALSE) {

  # Process x names and matrix
  x_names <- names(x)
  x <- matrix(unlist(x), ncol = length(x))

  # Process y names and matrix
  if (!missing(y)) {
    y_names <- names(y)
    y <- matrix(unlist(y), ncol = length(y))
  }

  # Calculate correlation matrix
  suppressWarnings({
    if (missing(y)) result <- stats::cor(x) else result <- stats::cor(x, y)
    })

  # Add names
  rownames(result) <- x_names
  if (missing(y)) colnames(result) <- x_names else colnames(result) <- y_names

  return(result)

}
