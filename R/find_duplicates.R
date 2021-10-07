#' Find and label duplicate image signatures
#'
#' \code{find_duplicates} takes a data frame with two matchr_signature vectors
#' and identifies and labels duplicate signatures, according to a given set of
#' thresholds.
#'
#' @param x A data frame with two columns containing matchr_signature vectors.
#' If there are columns named `x_sig` and `y_sig`, these will be the vectors
#' which will be analyzed. If not, the first two columns containing 
#' matchr_signature vectors will be used. If more than two such columns are
#' present, a warning will be issued.
#' @param threshold A length-one integer vector. Which Hamming distance should
#' be used to consider images to be identical? If the distance between two `x` 
#' or two `y` images is <= `threshold`, the images will be considered 
#' duplicates.
#' @param find_all A logical scalar. Should the function find all `y` duplicates
#' even for rows which do not have `x` duplicates (default FALSE)? If FALSE,
#' rows will be checked for `x` duplicates first, and any row without an `x`
#' duplicate will be removed from the search for `y` duplicates. This can result
#' in considerable speed gains if the goal is to find rows which have both `x`
#' and `y` duplicates (e.g. for subsequent processing in `confirm_matches`).
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return The input data frame, with additional `x_id` and `y_id` fields which
#' identify duplicated image signatures.
#' @export

find_duplicates <- function(x, threshold = 80, find_all = FALSE, 
                            quiet = FALSE) {
  
  # Return early if input is empty
  if (nrow(x) == 0) {
    x$x_id <- NA_character_
    x$y_id <- NA_character_
    return(x)
  }
  
  # Identify x_sig and y_sig
  if (all(c("x_sig", "y_sig") %in% names(x))) {
    x_sig <- x[["x_sig"]]
    y_sig <- x[["y_sig"]]
  } else {
    sig_names <- names(which(sapply(result, is_signature)))
    stopifnot("`x` must have at least two matchr_signature fields" = 
                length(sig_names) >= 2)
    if (length(sig_names) > 2) warning(
      "More than two matchr_signature fields were detected. ",
      "The first two will be used.", call. = FALSE)
    x_sig <- x[[sig_names[1]]]
    y_sig <- x[[sig_names[2]]]
  }
  
  # Add path names
  x$.x_name <- get_path(x_sig)
  x$.y_name <- get_path(y_sig)
  
  # Identify x images with correlation ~= 1
  x_matches <- 
    x_sig[!duplicated(x_sig)] |> 
    match_signatures(quiet = quiet) |> 
    identify_matches(threshold = threshold, quiet = quiet)
  
  # Exit early if no matches
  if (nrow(x_matches) == 0) {
    x$x_id <- x$y_id <- NA_integer_
    x$.x_name <- x$.y_name <- NULL
    return(x)
  }
  
  # Get list of x path pairs
  x_matches <- mapply(function(x, y) c(x, y), get_path(x_matches$x_sig),
                      get_path(x_matches$y_sig), SIMPLIFY = FALSE, 
                      USE.NAMES = FALSE)
  
  # Add duplicates
  dup_x <- table(get_path(x_sig))
  dup_x <- dup_x[dup_x >= 2]
  dup_x <- as.list(names(dup_x))
  x_matches <- c(x_matches, dup_x)
  
  # Reduce x_matches
  unique_name <- unique(unlist(x_matches))
  x_index <- lapply(x_matches, \(x) which(unique_name %in% x))
  x_reduced <- reduce_int(x_index)
  x_reduced <- lapply(x_reduced, \(x) unique_name[x])
  
  # Create x table and join IDs to x
  if (requireNamespace("dplyr", quietly = TRUE)) {
    x_table <- lapply(seq_along(x_reduced), function(n) 
      dplyr::tibble(x_id = n, .x_name = x_reduced[[n]]))
    x_table <- dplyr::bind_rows(x_table)
    x <- dplyr::full_join(x, x_table, by = ".x_name")
  } else {
    x_table <- lapply(seq_along(x_reduced), function(n) 
      data.frame(x_id = n, .x_name = x_reduced[[n]]))
    x_table <- do.call(rbind, x_table)
    x <- merge(x, x_table, all = TRUE)
  }
  
  # Optionally exclude y images with no x matches
  if (!find_all) y_sig <- y_sig[!is.na(x$x_id)]
  
  # Identify y images with correlation ~= 1
  y_matches <- 
    y_sig[!duplicated(get_path(y_sig))] |> 
    match_signatures(quiet = quiet) |> 
    identify_matches(threshold = threshold, quiet = TRUE)
  
  # Exit early if no matches
  if (nrow(y_matches) == 0) {
    x$x_id <- x$y_id <- NA_integer_
    x$.x_name <- x$.y_name <- NULL
    return(x)
  }
  
  # Get list of y path pairs
  y_matches <- mapply(function(x, y) c(x, y), get_path(y_matches$x_sig),
                      get_path(y_matches$y_sig), SIMPLIFY = FALSE, 
                      USE.NAMES = FALSE)
  
  # Add duplicates
  dup_y <- table(get_path(y_sig))
  dup_y <- dup_y[dup_y >= 2]
  dup_y <- as.list(names(dup_y))
  y_matches <- c(y_matches, dup_y)
  
  # Reduce y_matches
  unique_name <- unique(unlist(y_matches))
  y_index <- lapply(y_matches, \(x) which(unique_name %in% x))
  y_reduced <- reduce_int(y_index)
  y_reduced <- lapply(y_reduced, \(x) unique_name[x])
  
  # Create y table and join IDs to x
  if (requireNamespace("dplyr", quietly = TRUE)) {
    y_table <- lapply(seq_along(y_reduced), function(n) 
      dplyr::tibble(y_id = n, .y_name = y_reduced[[n]]))
    y_table <- dplyr::bind_rows(y_table)
    x <- dplyr::full_join(x, y_table, by = ".y_name")
  } else {
    y_table <- lapply(seq_along(y_reduced), function(n) 
      data.frame(y_id = n, .y_name = y_reduced[[n]]))
    y_table <- do.call(rbind, y_table)
    x <- merge(x, y_table, all = TRUE)
  }
  
  # Clean up and return result
  x$.x_name <- x$.y_name <- NULL
  return(x)
}
