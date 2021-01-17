#' Identify matches from a matchr image matrix
#'
#' \code{identify_matches} analyzes a `matchr_matrix` vector of image signature
#' correlations to identify possible matches. By default it sets a low
#' threshold for identifying matches, with the assumption that the results will
#' subsequently be refined using \code{\link{confirm_matches}}.
#'
#' @param img_matrix A vector of class `matchr_matrix`, containing image 
#' correlation matrices produced by \code{\link{match_signatures}}.
#' @param threshold A numeric scalar. The minimum correlation constant to
#' consider images to be matched.
#' @param confirm A logical scalar. Should the function run 
#' \code{\link{confirm_matches}} on the results before returning them (default)?
#' If TRUE, \code{\link{confirm_matches}} will be called with default 
#' parameters, i.e. under the assumption that the original signature matching
#' was done using greyscale signatures, and the confirmation should be carried
#' out with colour signatures with the default tolerance thresholds.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A tibble if {dplyr} is installed or a data frame if not, with one
#' row per identified match, and the following columns:
#' - `matrix`: The match's index position in the input `matchr_matrix` vector.
#' - `x_index` and `y_index`: The match's row and column index positions in the 
#' correlation matrix.
#' - `x_file` and `y_file`: The file paths for the images which were matched.
#' -  `correlation`: The Pearson correlation coefficient of the two files'
#' image signatures.
#' - `match` (if `confirm = TRUE`): A character vector indicating match status.
#' (See \code{\link{confirm_matches}} for details.)
#' @export

identify_matches <- function(img_matrix, threshold = 0.975, confirm = TRUE,
                             quiet = FALSE) {

  # Error handling
  stopifnot(is_matrix(img_matrix), is.numeric(threshold), is.logical(confirm), 
            is.logical(quiet))

  # Find matches
  match_list <- lapply(seq_along(img_matrix), function(x) {
    match_index <- which(field(img_matrix[x], "matrix")[[1]] >= threshold,
                         arr.ind = TRUE)
    dimnames(match_index)[[2]] <- c("x_index", "y_index")
    if (requireNamespace("dplyr", quietly = TRUE)) {
      match <- dplyr::as_tibble(match_index)
      } else match <- as.data.frame(match_index)
    match$matrix <- x
    match <- match[, c(3, 1, 2)]
    match$x_sig <- field(img_matrix[x], "x_sig")[[1]][match$x_index]
    match$y_sig <- field(img_matrix[x], "y_sig")[[1]][match$y_index]
    match$correlation <- field(img_matrix[x], "matrix")[[1]][match_index]
    match
    })

  # Consolidate and arrange output
  if (requireNamespace("dplyr", quietly = TRUE)) {
    matches <- dplyr::bind_rows(match_list)
  } else {
    x_sig <- do.call(c, lapply(match_list, function(x) x$x_sig))
    y_sig <- do.call(c, lapply(match_list, function(x) x$y_sig))
    matches <- do.call(rbind, lapply(match_list, function(x) x[,c(1:3, 6)]))
    matches$x_sig <- x_sig
    matches$y_sig <- y_sig
    matches <- matches[,c(1:3, 5:6, 4)]
    }
  matches <- matches[order(matches$matrix, matches$x_index, matches$y_index),]
  
  # Remove duplicates
  matches <- 
    matches[field(matches$x_sig, "file") != field(matches$y_sig, "file"),]
  matches$hash <- mapply(function(x, y) sort(c(x, y)), 
                         field(matches$x_sig, "file"), 
                         field(matches$y_sig, "file"), SIMPLIFY = FALSE)
  matches <- matches[!duplicated(matches$hash),]
  matches$hash <- NULL
  
  # Return output
  if (confirm) matches <- confirm_matches(matches)
  return(matches)

}
