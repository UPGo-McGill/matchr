#' Identify matches from a matchr image matrix
#'
#' \code{identify_matches} analyzes a `matchr_matrix` vector of image signature
#' correlations to identify possible matches. By default it sets a low
#' threshold for identifying matches, with the assumption that the results will
#' subsequently be refined using \code{\link{confirm_matches}}.
#'
#' @param x A vector of class `matchr_sig`, containing image signatures produced
#' by \code{\link{create_signature}}, or `matchr_matrix`, containing image 
#' correlation matrices produced by \code{\link{match_signatures}}.
#' @param y A vector of class `matchr_sig`, containing image signatures produced
#' by \code{\link{create_signature}}. If x is not a `matchr_sig` vector, this
#' argument is ignored, and will trigger a warning if a non-NULL value is
#' supplied.
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
#' @param ... Additional arguments passed to methods.
#' @return A tibble if {dplyr} is installed or a data frame if not, with one
#' row per identified match, and the following columns:
#' - `matrix`: The match's index position in the input `matchr_matrix` vector.
#' - `list_index`, `x_index` and `y_index`: The match's list, row and column 
#' index positions in the `matchr_matrix` vector element (which is represented
#' as a matrix but is actually a list of matrices).
#' - `x_file` and `y_file`: The file paths for the images which were matched.
#' -  `correlation`: The Pearson correlation coefficient of the two files'
#' image signatures.
#' - `match` (if `confirm = TRUE`): A character vector indicating match status.
#' (See \code{\link{confirm_matches}} for details.)
#' @export

identify_matches <- function(x, y = NULL, threshold = 0.975, confirm = TRUE, 
                             quiet = FALSE, ...) {
  
  UseMethod("identify_matches")
  
}

# ------------------------------------------------------------------------------

#' @rdname identify_matches
#' @method identify_matches matchr_matrix
#' @export

identify_matches.matchr_matrix <- function(x, y = NULL, threshold = 0.975, 
                                           confirm = TRUE, quiet = FALSE, ...) {

  # Error handling
  stopifnot(is_matrix(x), is.numeric(threshold), is.logical(confirm), 
            is.logical(quiet))

  # Initialize progress reporting
  handler_matchr("Identifying matches, batch")
  prog_bar <- as.logical(as.numeric(!quiet) * progressr::handlers(global = NA))
  pb <- progressr::progressor(steps = sum(sapply(field(x, "matrix"), 
                                                 vec_size)), enable = prog_bar)
  
  # Find matches
  match_list <- 
    lapply(seq_along(x), function(n) {
      
      match_index <- lapply(field(x, "matrix")[[n]], function(y) {
        pb()
        m <- which(y >= threshold, arr.ind = TRUE)
        dimnames(m)[[2]] <- c("x_index", "y_index")
        m
      })
      
      match <- lapply(seq_along(match_index), function(i) {
        if (requireNamespace("dplyr", quietly = TRUE)) {
          match <- dplyr::as_tibble(match_index[[i]])
        } else match <- as.data.frame(match_index[[i]])
        match$list_index <- i
        match$matrix <- n
        match <- match[c(4, 3, 1, 2)]
        match
      })
      
      match <-
        mapply(function(match, match_index, x_sig, matrix) {
          match$x_sig <- x_sig[match$x_index]
          match$correlation <- matrix[match_index]
          match
        },
        match, match_index, field(x, "x_sig")[[n]], 
        field(x, "matrix")[[n]], SIMPLIFY = FALSE
        )
      
      if (requireNamespace("dplyr", quietly = TRUE)) {
        match <- dplyr::bind_rows(match)
      } else {
        x_sig <- do.call(c, lapply(match, function(x) x$x_sig))
        match <- do.call(rbind, lapply(match, function(x) x[,c(1:4, 6)]))
        match$x_sig <- x_sig
        match <- match[c(1:4, 6, 5)]
        }
      
      match$y_sig <- field(x, "y_sig")[[n]][match$y_index]
      match <- match[c(1:5, 7, 6)]
      match
    })
  
  # Consolidate and arrange output
  if (requireNamespace("dplyr", quietly = TRUE)) {
    matches <- dplyr::bind_rows(match_list)
  } else {
    x_sig <- do.call(c, lapply(match_list, function(x) x$x_sig))
    y_sig <- do.call(c, lapply(match_list, function(x) x$y_sig))
    matches <- do.call(rbind, lapply(match_list, function(x) x[c(1:4, 7)]))
    matches$x_sig <- x_sig
    matches$y_sig <- y_sig
    matches <- matches[c(1:4, 6:7, 5)]
    }
  matches <- matches[order(matches$matrix, matches$list_index, matches$x_index, 
                           matches$y_index),]

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

# ------------------------------------------------------------------------------

#' @rdname identify_matches
#' @method identify_matches matchr_sig
#' @param method,compare_ar,stretch,mem_scale Arguments passed to 
#' \code{\link{match_signatures}}.
#' @export

identify_matches.matchr_sig <- function(x, y, threshold = 0.975, 
                                        confirm = TRUE, quiet = FALSE, 
                                        method = "grey", compare_ar = TRUE, 
                                        stretch = 1.2, mem_scale = 0.2, ...) {
  
  # Error handling and object initialization
  stopifnot(is_signature(x), is.logical(c(compare_ar, quiet)),
            method %in% c("grey", "gray", "colour", "color", "rgb", "RGB", 
                          "both"))
  if (missing(y)) y <- x else stopifnot(is_signature(y))
  par_check <- TRUE
  
  # Prepare objects for processing
  output <- match_signatures_prep(x, y, method, compare_ar, stretch, mem_scale)
  x <- output[[1]]
  y <- output[[2]]
  x_na <- output[[3]]
  y_na <- output[[4]]
  x_list <- output[[5]]
  y_list <- output[[6]]
  x_sig <- output[[7]]
  y_sig <- output[[8]]
  # rm(output)
  
  output
}
