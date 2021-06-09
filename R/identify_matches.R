#' Identify matches from a matchr image matrix
#'
#' \code{identify_matches} analyzes a `matchr_matrix` vector of image signature
#' correlations to identify possible matches. By default it sets a low
#' threshold for identifying matches, with the assumption that the results will
#'be refined using \code{\link{confirm_matches}}, either within the function if
#'`confirm = TRUE`, or subsequently with a separate function call.
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
#' - `x_index` and `y_index`: The match's row and column index positions in the 
#' `matchr_matrix` vector element.
#' - `x_path` and `y_path`: The file paths for the images which were matched.
#' - `correlation`: The Pearson correlation coefficient of the two files'
#' image signatures.
#' - `match` (if `confirm = TRUE`): A character vector indicating match status.
#' (See \code{\link{confirm_matches}} for details.)
#' @examples
#' \dontrun{
#' # Setup
#' sigs <- create_signature(test_urls)
#' matches <- match_signatures(sigs)
#' 
#' # By default, confirm_matches will be called inside identify_matches
#' result <- identify_matches(matches)
#' 
#' # Skip this step with confirm = FALSE
#' result_no_confirm <- identify_matches(matches, confirm = FALSE)
#' }
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
  pb <- progressr::progressor(steps = sum(sapply(get_array(x), vec_size)), 
                              enable = prog_bar)
  
  # Find matches
  match_list <- lapply(seq_along(x), identify_matches_internal, x, pb, 
                       threshold)
  
  # Finish output
  matches <- identify_matches_finish(match_list)

  # Return output
  if (confirm) matches <- confirm_matches(matches)
  return(matches)

}

# ------------------------------------------------------------------------------

#' @rdname identify_matches
#' @method identify_matches matchr_signature
#' @param method,compare_ar,stretch,mem_scale Arguments passed to 
#' \code{\link{match_signatures}}.
#' @export

identify_matches.matchr_signature <- function(
  x, y = NULL, threshold = 0.975, confirm = TRUE, quiet = FALSE, 
  method = "grey", compare_ar = TRUE, stretch = 1.2, mem_scale = 0.2, ...) {
  
  # Error handling and object initialization
  stopifnot(is_signature(x), is.logical(c(compare_ar, quiet)),
            method %in% c("grey", "gray", "colour", "color", "rgb", "RGB", 
                          "both"))
  if (missing(y)) y <- x else stopifnot(is_signature(y))
  par_check <- TRUE
  
  # Prepare objects for processing
  output <- suppressWarnings(match_signatures_prep(
    x, y, method, compare_ar, stretch, mem_scale, mem_override = TRUE))
  x <- output[[1]]
  y <- output[[2]]
  x_list <- output[[5]]
  y_list <- output[[6]]
  x_sig <- output[[7]]
  y_sig <- output[[8]]
  rm(output)
  
  # Initialize progress reporting
  handler_matchr("Identifying matches, row")
  prog_bar <- as.logical((vec_size(x) >= 5000) * as.numeric(!quiet) *
                           progressr::handlers(global = NA))
  pb <- progressr::progressor(steps = vec_size(x), enable = prog_bar)
  
  # Calculate correlation matrices
  result <- vector("list", length(x_list))
  for (i in seq_along(x_list)) {
    result[[i]] <- match_signatures_internal(x_list[[i]], y_list[[i]])
    
    result[[i]] <- new_matrix(
      array = result[i],
      x_ar = list(get_ratios(x_list[[i]])),
      y_ar = list(get_ratios(y_list[[i]])),
      x_sig = x_sig[i],
      y_sig = y_sig[i],
      x_total = vec_size(x_list[[i]]),
      y_total = vec_size(y_list[[i]]),
      x_na = character(),
      y_na = character()
    )
    
    result[[i]] <- identify_matches_internal(i, result[[i]], function() NULL, 
                                             threshold)
    
    pb(amount = sum(sapply(x_list[[i]], vec_size)))
  }

  # Finish output
  matches <- identify_matches_finish(result)
  
  # Return output
  if (confirm) matches <- confirm_matches(matches)
  return(matches)
}

# ------------------------------------------------------------------------------

identify_matches_internal <- function(n, x, pb, threshold) {
  
  match_index <- which(get_array(x[[n]])[[1]] >= threshold, arr.ind = TRUE)
  dimnames(match_index)[[2]] <- c("x_index", "y_index")
  
  if (requireNamespace("dplyr", quietly = TRUE)) {
    match <- dplyr::as_tibble(match_index)
  } else match <- as.data.frame(match_index)
  
  match$matrix <- n
  match <- match[c("matrix", "x_index", "y_index")]
  
  match$x_sig <- get_x_sig(x[[n]])[[1]][match$x_index]
  match$y_sig <- get_y_sig(x[[n]])[[1]][match$y_index]
  match$correlation <- get_array(x[[n]])[[1]][match_index]
  
  match
  
}

# ------------------------------------------------------------------------------

identify_matches_finish <- function(match_list) {
  
  # Consolidate and arrange output
  if (requireNamespace("dplyr", quietly = TRUE)) {
    matches <- dplyr::bind_rows(match_list)
  } else {
    x_sig <- do.call(c, lapply(match_list, function(x) x$x_sig))
    y_sig <- do.call(c, lapply(match_list, function(x) x$y_sig))
    matches <- do.call(rbind, lapply(match_list, function(x) 
      x[c("matrix", "x_index", "y_index", "correlation")]))
    matches$x_sig <- x_sig
    matches$y_sig <- y_sig
    matches <- matches[c("matrix", "x_index", "y_index", "x_sig", "y_sig", 
                         "correlation")]
  }
  matches <- matches[order(matches$matrix, matches$x_index, matches$y_index),]
  
  # Remove duplicates
  matches <-
    matches[get_path(matches$x_sig) != get_path(matches$y_sig),]
  matches$hash <- mapply(function(x, y) sort(c(x, y)),
                         get_path(matches$x_sig),
                         get_path(matches$y_sig), SIMPLIFY = FALSE)
  matches <- matches[!duplicated(matches$hash),]
  matches$hash <- NULL
  
  matches
}
