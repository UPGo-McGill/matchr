#' Confirm matches using a more detailed colour signature
#'
#' \code{confirm_matches} takes the results of \code{\link{identify_matches}}
#' and performs an additional analysis on image signature pairs whose
#' correlations were not high enough to guarantee a match but not low enough to
#' discount a match. The default implementation assumes that image signatures
#' were compared using the greyscale components, and so it performs an
#' additional correlation using the colour components.
#'
#' @param data A data frame produced from \code{\link{identify_matches}}.
#' @param x_sigs,y_sigs Vectors of class `matchr_signature`. If these arguments 
#' are NULL (default), new signatures will be calculated for the rows in `data`.
#' @param check_threshold A numeric scalar. Matches with correlations higher
#' than this value (default 0.99) will be treated as matching and not 
#' re-checked.
#' @param confirm_thresholds A numeric vector of length 2. New correlation 
#' coefficients below the first value (default 0.95) will be classified as "no 
#' match", coefficients above the second value (default 0.98) will be classified 
#' as "likely match", and coefficients between the two values will be classified
#' as "possible match".
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @param ... Additional named arguments to pass to 
#' \code{\link{create_signature}} if the `x_sigs` or `y_sigs` argument is empty,
#' or to \code{\link{match_signatures}}.
#' @return The input `data` data frame, with an additional `confirmation` field
#' which is a character vector with the following possible values:
#' - "match": the original correlation coefficient was greater than or equal to
#'  `check_threshold`.
#' - "likely match": the original correlation coefficient was less than
#' `check_threshold` and the new coefficient was greater than or equal to
#' `confirm_thresholds[2]`.
#' - "possible match: the original correlation coefficient was less than
#' `check_threshold` and the new coefficient was less than 
#' `confirm_thresholds[2]` and greater than or equal to
#' `confirm_thresholds[1]`.
#' - "no match": the original correlation coefficient was less than
#' `check_threshold` and the new coefficient was less than 
#' `confirm_thresholds[1]`.
#' @export

confirm_matches <- function(data, x_sigs = NULL, y_sigs = NULL,
                            check_threshold = 0.99,
                            confirm_thresholds = c(0.95, 0.98), 
                            quiet = FALSE, ...) {

  # Error handling
  stopifnot(is.data.frame(data), is.numeric(check_threshold), 
            is.numeric(confirm_thresholds), is.logical(quiet))
  if (!missing(x_sigs)) stopifnot(is_signature(x_sigs))
  if (!missing(y_sigs)) stopifnot(is_signature(y_sigs))


  # Subset data
  to_check <- data[data$correlation < check_threshold,]

  # Exit early if to_check is empty
  if (nrow(to_check) == 0) {
    data$confirmation <- "match"
    return(data)
  }
  
  # Get x signatures
  if (!missing(x_sigs)) {
    x_sigs <- x_sigs[sapply(to_check$x_file, function(x) 
      which(field(x_sigs, "file") == x))]
  } else x_sigs <- create_signature(to_check$x_file, quiet = quiet, ...)

  # Get y signatures
  if (!missing(y_sigs)) {
    y_sigs <- y_sigs[sapply(to_check$y_file, function(x) 
      which(field(y_sigs, "file") == x))]
  } else y_sigs <- create_signature(to_check$y_file, quiet = quiet, ...)
  
  # Match by colour
  to_check$colour <- match_signatures_pairwise(x_sigs, y_sigs)
  
  # Compile results
  to_check$confirmation <- "no match"
  to_check[to_check$colour >= confirm_thresholds[[1]],]$confirmation <-
    "possible match"
  to_check[to_check$colour >= confirm_thresholds[[2]],]$confirmation <-
    "likely match"
  data <- data[data$correlation >= check_threshold,]
  data$confirmation <- "match"

  # Rbind data and to_check
  if (requireNamespace("dplyr", quietly = TRUE)) {
    data <- dplyr::bind_rows(data, to_check[,c(1:6, 8)])
    } else data <- rbind(data, to_check[,c(1:6, 8)])
  data <- data[order(data$matrix, data$x_index, data$y_index),]

  # Return result
  return(data)

}
