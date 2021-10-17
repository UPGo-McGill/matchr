#' Match pairs of images based on colour signatures
#'
#' \code{match_signatures_pairwise} takes two vectors of image signatures and 
#' produces a vector of Hamming distances between the pairwise elements of the
#' signatures.
#'
#' While \code{\link{match_signatures}} produces a matrix of Hamming distances
#' between each of the `x` elements and each of the `y` elements, sometimes it
#' is helpful to simply calculate Hamming distances between pairs of elements
#' in each vector—e.g. `x[i]` and `y[i]`, `x[j]` and `y[j]`, etc. 
#' \code{match_signatures_pairwise} calculates these pairwise distances and
#' returns a numeric vector.
#' 
#' @param x,y Vectors of class `matchr_signature` to be matched.
#' @param distance A one-sided formula (or character string which can be
#' coerced to a formula) with one or both of the terms `nearest` and `bilinear`,
#' expressing how the Hamming distance between image signature vectors should be
#' calculated. The default (`~nearest * bilinear`) takes the Hamming distances
#' of each of the two image signature components and multiplies them together.
#' Any arithmetical combination of these distances is a valid argument to
#' `distance`, e.g. `~ nearest + log(bilinear)`.
#' @return A numeric vector, each element `v[i]` of which is the Hamming 
#' distance between the `x[i]` and `y[i]` signatures. The formula supplied to 
#' the `distance` argument will be present as an additional attribute to the 
#' return vector, named `formula`.
#' @examples
#' \dontrun{
#' # Setup
#' sigs <- create_signature(example_urls)
#' 
#' # Find pairwise matches between vectors
#' match_signatures_pairwise(sigs, sigs)
#' }
#' @export

match_signatures_pairwise <- function(x, y, distance = ~ nearest * bilinear) {
  out <- par_mapply(ms_internal, x, y, MoreArgs = list(distance = distance), 
                    SIMPLIFY = TRUE)
  out[lengths(out) == 0] <- NA_real_
  out <- unlist(out)
  attr(out, "formula") <- as.character(distance)[[2]]
  return(out)
}
