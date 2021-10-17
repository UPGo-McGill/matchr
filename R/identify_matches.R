#' Identify matches from a matchr image matrix
#'
#' \code{identify_matches} analyzes a `matchr_matrix` vector of image signature
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
#' - `x_sig` and `y_sig`: `matchr_signature` vectors containing the image 
#' signatures which were matched.
#' - `distance`: An integer vector giving the Hamming distance between the two 
#' files' image signatures. (The lower the distance, the more perceptually 
#' similar the images are.)
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

identify_matches <- function(x, y = NULL, threshold = 200, quiet = FALSE, ...) {
  
  UseMethod("identify_matches")
  
}

# ------------------------------------------------------------------------------

#' @rdname identify_matches
#' @method identify_matches matchr_matrix
#' @export

identify_matches.matchr_matrix <- function(x, y = NULL, threshold = 200, 
                                           quiet = FALSE, ...) {
  
  # Error handling
  stopifnot(is_matrix(x), is.numeric(threshold), is.logical(quiet))
  if (!missing(y)) {
    stop("`identify_matches` cannot take a `y` argument when the `x` argument ",
         "is a matchr_matrix.")
  }
  
  # Initialize progress reporting
  handler_matchr("Identifying matches, batch")
  prog_bar <- as.logical(
    as.numeric(!quiet) * progressr::handlers(global = NA) * check_env())
  pb <- progressr::progressor(steps = sum(sapply(get_array(x), vec_size)), 
                              enable = prog_bar)
  
  # Find matches
  match_list <- lapply(seq_along(x), im_internal, x, pb, threshold)
  
  # Finish output and return
  matches <- im_finish(match_list)
  return(matches)
  
}

# ------------------------------------------------------------------------------

#' @rdname identify_matches
#' @method identify_matches matchr_signature
#' @param distance,compare_ar,stretch,mem_scale Arguments passed to 
#' \code{\link{match_signatures}}.
#' @export

identify_matches.matchr_signature <- function(
  x, y = NULL, threshold = 200, quiet = FALSE, distance = ~ nearest * bilinear, 
  compare_ar = TRUE, stretch = 1.2, mem_scale = 0.2, ...) {
  
  # Error handling and object initialization
  stopifnot(is_signature(x), (is.language(distance) | is.character(distance)),
            is.logical(c(compare_ar, quiet)))
  if (missing(y)) y <- x else stopifnot(is_signature(y))
  stopifnot(
    "`distance` must contain one or both of `nearest` and `bilinear" = 
      sum(c("nearest", "bilinear") %in% as.character(distance[[2]])) > 0)
  par_check <- TRUE
  
  # Prepare objects for processing
  output <- suppressWarnings(ms_prep(x, y, compare_ar, stretch, mem_scale, 
                                     mem_override = FALSE))
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
                           progressr::handlers(global = NA) * check_env())
  pb <- progressr::progressor(steps = vec_size(x), enable = prog_bar)
  
  # Loop over signatures one-by-one to reduce memory footprint
  result <- vector("list", length(x_list))
  for (i in seq_along(x_list)) {
    
    # Create distance matrix
    result[[i]] <- ms_internal(x_list[[i]], y_list[[i]], distance)
    
    # Create intermediate matchr_matrix
    result[[i]] <- new_matrix(
      array = result[i],
      x_ar = list(get_ratios(x_list[[i]])),
      y_ar = list(get_ratios(y_list[[i]])),
      x_sig = x_sig[i],
      y_sig = y_sig[i],
      x_total = length(unique(c(get_path(x), get_path(x_na)))),
      y_total = length(unique(c(get_path(y), get_path(y_na)))),
      x_na = get_path(x_na),
      y_na = get_path(y_na),
      formula = as.character(distance)[[2]]
    )
    
    # Find matches from intermediate matrix
    result[[i]] <- im_internal(1, result[[i]], pb, threshold)
    result[[i]]$matrix <- i
    
    pb(amount = sum(sapply(x_list[[i]], vec_size)))
  }
  
  # Finish and return output
  matches <- im_finish(result)
  return(matches)
}

# ------------------------------------------------------------------------------

im_internal <- function(n, x, pb, threshold) {
  
  match_index <- which(get_array(x)[[n]] <= threshold, arr.ind = TRUE)
  
  dimnames(match_index)[[2]] <- c("x_index", "y_index")
  
  if (requireNamespace("dplyr", quietly = TRUE)) {
    match <- dplyr::as_tibble(match_index)
  } else match <- as.data.frame(match_index)
  
  match$matrix <- n
  match <- match[c("matrix", "x_index", "y_index")]
  
  match$x_sig <- get_x_sig(x[[n]])[[1]][match$x_index]
  match$y_sig <- get_y_sig(x[[n]])[[1]][match$y_index]
  match$distance <- get_array(x[[n]])[[1]][match_index]
  match$distance <- as.integer(match$distance)
  
  match
  
}

# ------------------------------------------------------------------------------

im_finish <- function(match_list) {
  
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
  
  # Merge index fields and return output
  matches$index <- mapply(c, matches$matrix, matches$x_index, matches$y_index, 
                          SIMPLIFY = FALSE)
  matches <- matches[c("index", "x_sig", "y_sig", "distance")]
  return(matches)
}
