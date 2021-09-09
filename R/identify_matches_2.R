#' Identify matches from a matchr image matrix
#'
#' \code{identify_matches_2} analyzes a `matchr_matrix` vector of image signature
#' Hamming distances to identify possible matches. By default it sets a low
#' threshold for identifying matches, with the assumption that the results will
#'be refined using \code{\link{confirm_matches}}, either within the function if
#'`confirm = TRUE`, or subsequently with a separate function call.
#'
#' @param x A vector of class `matchr_sig`, containing image signatures produced
#' by \code{\link{create_signature}}, or `matchr_matrix`, containing image 
#' signature Hamming distance matrices produced by 
#' \code{\link{match_signatures}}.
#' @param y A vector of class `matchr_sig`, containing image signatures produced
#' by \code{\link{create_signature}}. If x is not a `matchr_sig` vector, this
#' argument is ignored, and will trigger a warning if a non-NULL value is
#' supplied.
#' @param threshold A numeric scalar. The maximum Hamming distance to consider 
#' images to be matched.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @param ... Additional arguments passed to methods.
#' @return A tibble if {dplyr} is installed or a data frame if not, with one
#' row per identified match, and the following columns:
#' - `index`: The match's index position in the input `matchr_matrix` vector.
#' Each element is a length-three integer vector giving, respectively, the
#' matrix, row index position, and column index position of the match.
#' - `x_path` and `y_path`: The file paths for the images which were matched.
#' - `distance`: An integer vector giving the Hamming distance between the two 
#' files' image signatures. (The lower the distance, the more perceptually 
#' similar the images are.)
#' - `dd`: An integer vector giving the product of the Hamming distances between
#' the two files' primary and secondary image signatures. The secondary image
#' signature is less reliable than the primary one, but can be a helpful
#' additional data point in assessing potential matches with borderline 
#' primary-signature Hamming distances. In general, the lower the `dd` value, 
#' the higher the chance that two images are the same.
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

identify_matches_2 <- function(x, y = NULL, threshold = 12, quiet = FALSE, 
                               ...) {
  
  UseMethod("identify_matches_2")
  
}

# ------------------------------------------------------------------------------

#' @rdname identify_matches_2
#' @method identify_matches_2 matchr_matrix_2
#' @export

identify_matches_2.matchr_matrix_2 <- function(x, y = NULL, threshold = 12, 
                                             quiet = FALSE, ...) {
  
  # Error handling
  stopifnot(is_matrix_2(x), is.numeric(threshold), is.logical(quiet))
  
  # Initialize progress reporting
  handler_matchr("Identifying matches, batch")
  prog_bar <- as.logical(as.numeric(!quiet) * progressr::handlers(global = NA))
  pb <- progressr::progressor(steps = sum(sapply(get_array(x), vec_size)), 
                              enable = prog_bar)
  
  # Find matches
  match_list <- lapply(seq_along(x), identify_matches_2_internal, x, pb, 
                       threshold)
  
  # Finish output and return
  matches <- identify_matches_2_finish(match_list)
  return(matches)
  
}

# ------------------------------------------------------------------------------

#' @rdname identify_matches_2
#' @method identify_matches_2 matchr_signature_2
#' @param compare_ar,stretch,mem_scale Arguments passed to 
#' \code{\link{match_signatures}}.
#' @export


#### TKTK DOES ANY OF THIS WORK?
identify_matches_2.matchr_signature_2 <- function(
  x, y = NULL, threshold = 12, quiet = FALSE, compare_ar = TRUE, 
  stretch = 1.2, mem_scale = 0.2, ...) {
  
  # Error handling and object initialization
  stopifnot(is_signature_2(x), is.logical(c(compare_ar, quiet)))
  if (missing(y)) y <- x else stopifnot(is_signature_2(y))
  par_check <- TRUE
  
  # Prepare objects for processing
  output <- suppressWarnings(
    match_signatures_2_prep(x, y, compare_ar, stretch, mem_scale, mem_override))
  x <- output[[1]]
  y <- output[[2]]
  x_na <- output[[3]]
  y_na <- output[[4]]
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
  
  # Calculate distance matrices
  result <- vector("list", length(x_list))
  for (i in seq_along(x_list)) {
    result[[i]] <- match_signatures_2_internal(x_list[[i]], y_list[[i]])
    
    result[[i]] <- new_matrix_2(
      array = result,
      x_ar = lapply(x_list, get_ratios),
      y_ar = lapply(y_list, get_ratios),
      x_sig = x_sig,
      y_sig = y_sig,
      x_total = length(unique(c(get_path(x), get_path(x_na)))),
      y_total = length(unique(c(get_path(y), get_path(y_na)))),
      x_na = get_path(x_na),
      y_na = get_path(y_na)
    )
    
    result[[i]] <- 
      identify_matches_2_internal(i, result, function() NULL, threshold)
    
    pb(amount = sum(sapply(x_list[[i]], vec_size)))
  }
  
  # Finish and return output
  matches <- identify_matches_2_finish(result)
  return(matches)
}

# ------------------------------------------------------------------------------

identify_matches_2_internal <- function(n, x, pb, threshold) {
  
  if (threshold <= 1) {
    match_index <- which(get_array(x[[n]])[[1]] >= threshold, arr.ind = TRUE)  
  } else {
    match_index <- which(get_array(x[[n]])[[1]] <= threshold, arr.ind = TRUE)  
  }
  
  dimnames(match_index)[[2]] <- c("x_index", "y_index")
  
  if (requireNamespace("dplyr", quietly = TRUE)) {
    match <- dplyr::as_tibble(match_index)
  } else match <- as.data.frame(match_index)
  
  match$matrix <- n
  match <- match[c("matrix", "x_index", "y_index")]
  
  match$x_sig <- get_x_sig(x[[n]])[[1]][match$x_index]
  match$y_sig <- get_y_sig(x[[n]])[[1]][match$y_index]
  match$distance <- get_array(x[[n]])[[1]][match_index]
  
  match
  
}

# ------------------------------------------------------------------------------

identify_matches_2_finish <- function(match_list) {
  
  # Consolidate and arrange output
  if (requireNamespace("dplyr", quietly = TRUE)) {
    matches <- dplyr::bind_rows(match_list)
  } else {
    x_sig <- do.call(c, lapply(match_list, function(x) x$x_sig))
    y_sig <- do.call(c, lapply(match_list, function(x) x$y_sig))
    matches <- do.call(rbind, lapply(match_list, function(x) 
      x[c("matrix", "x_index", "y_index", "distance")]))
    matches$x_sig <- x_sig
    matches$y_sig <- y_sig
    matches <- matches[c("matrix", "x_index", "y_index", "x_sig", "y_sig", 
                         "distance")]
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
  
  # Merge index fields
  matches$index <- mapply(c, matches$matrix, matches$x_index, matches$y_index, 
                          SIMPLIFY = FALSE)
  matches <- matches[c("index", "x_sig", "y_sig", "distance")]
  
  # Calculate dd values
  matches$dd <- matches$distance *
    match_signatures_2_pairwise(matches$x_sig, matches$y_sig, "ahash")
  
  # Return output
  matches
}
