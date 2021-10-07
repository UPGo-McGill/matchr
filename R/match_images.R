#' Find matches from vectors of image paths
#'
#' \code{match_images} is a convenience wrapper around the core workflow of 
#' matchr. It reads in one or two character vectors of image paths and then
#' runs \code{\link{load_image}}, \code{\link{create_signature}},
#' \code{\link{match_signatures}}, and \code{\link{identify_matches}}, (in each 
#' case with the respective function's default arguments), and then optionally 
#' sends the results to the \code{\link{confirm_matches}} interactive Shiny app 
#' for manual verification and integrates any manual changes into the output 
#' data frame.
#' 
#' For large datasets where performance and memory considerations make it 
#' prudent to save intermediate outputs, or if any non-default options are 
#' required, it is recommended to run the component functions separately, but 
#' for small datasets \code{match_images} offers the simplest path from images
#' to image matches.
#'
#' @param x,y Character vector of file paths or URLs. If `y` is supplied then
#' matches will be identified between the two input vectors `x` and `y`; if it 
#' is not supplied then matches will be identified within the input vector `x`.
#' @param compare A logical scalar. Should the interactive 
#' \code{\link{confirm_matches}} Shiny app be run to manually verify match
#' results (default)?
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A tibble if {dplyr} is installed or a data frame if not, with one
#' row per identified match, and the following columns:
#' - `x_path` and `y_path`: The file paths for the images which were matched.
#' - `correlation`: The Pearson correlation coefficient of the two files'
#' image signatures.
#' - `match`: A character vector indicating match status.
#' (See \code{\link{confirm_matches}} for details.)
#' @examples
#' \dontrun{
#' # Use match_images with a single argument to identify matches within a set of images
#' match_images(test_urls)
#'
#' # Or add a second argument to identify matches between two sets of images
#' match_images(test_urls[1:8], test_urls[9:15])
#' 
#' # To retrieve results without manual verification through the Shiny app, set `compare = FALSE`
#' match_images(test_urls, compare = FALSE)
#' }
#' @export

match_images <- function(x, y = NULL, compare = TRUE, quiet = FALSE) {
  
  stopifnot(is.character(x), is.logical(compare), is.logical(quiet))
  if (!is.null(y)) stopifnot(is.character(y))
  
  x_sig <- create_signature(x, quiet = quiet)
  if (is.null(y)) y_sig <- x_sig else y_sig <- create_signature(y, quiet = quiet)
  
  matches <- identify_matches(x_sig, y_sig, quiet = quiet)
  
  if (compare) {
    changes <- confirm_matches(matches, quiet = quiet)
    matches <- integrate_changes(matches, changes)
    }
  
  return(matches)
}