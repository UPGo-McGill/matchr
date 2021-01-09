#' Identify matches from a matchr image matrix
#'
#' \code{identify_matches} analyzes a `matchr_matrix` vector of image signature
#' correlations to identify possible matches.
#'
#' @param img_matrix A vector of class `matchr_matrix`, containing image 
#' correlation matrices produced by \code{matchr::match_signatures}.
#' @param threshold A numeric scalar. The minimum correlation constant to
#' consider images to be matched.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A tibble if {dplyr} is installed or a data frame if not, with one
#' row per identified match. The data frame has the following columns:
#' - `matrix`: The match's index position in the input `matchr_matrix` vector.
#' - `x_index` and `y_index`: The match's row and column index positions in the 
#' correlation matrix.
#' - `x_file` and `y_file`: The file paths for the images which were matched.
#' -  `correlation`: The Pearson correlation coefficient of the two files
#' image signatures.
#' @export

identify_matches <- function(img_matrix, threshold = 0.975, quiet = FALSE) {

  # Error handling
  stopifnot(is_matrix(img_matrix), is.numeric(threshold), is.logical(quiet))

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
    match$x_file <- field(img_matrix[x], "x_files")[[1]][match$x_index]
    match$y_file <- field(img_matrix[x], "y_files")[[1]][match$y_index]
    match$correlation <- field(img_matrix[x], "matrix")[[1]][match_index]
    match
    })

  # Consolidate and arrange output
  if (requireNamespace("dplyr", quietly = TRUE)) {
    matches <- dplyr::bind_rows(match_list)
  } else matches <- do.call(rbind, match_list)
  matches <- matches[order(matches$matrix, matches$x_index, matches$y_index),]

  # TKTK TEMPORARILY DISABLED
  # Remove redundant matches if the matrix is generated from a single list
  # if (dim(image_matrix)[[1]] == dim(image_matrix)[[2]]) {
  #   if (mean(rownames(image_matrix) == colnames(image_matrix)) == 1) {
  #     # Remove self matches
  #     matches <- matches[matches$x_index != matches$y_index,]
  #   }
  # }

  # Remove duplicate matches
  pairs <- mapply(function(m, x, y) sort(c(m, x, y)), matches$matrix, 
                  matches$x_index, matches$y_index, SIMPLIFY = FALSE)
  matches <- matches[!duplicated(pairs),]

  # Return output
  return(matches)

}
