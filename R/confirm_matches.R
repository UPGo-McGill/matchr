#' Confirm matches using a more detailed colour signature
#'
#' \code{confirm_matches} takes the results of \code{\link{identify_matches}}
#' and performs an additional analysis on image signature pairs whose
#' correlations were not high enough to guarantee a match but not low enough to
#' discount a match. The default implementation assumes that image signatures
#' were compared using the greyscale components, and so it performs an
#' additional correlation using the colour components.
#' 
#' By default, \code{confirm_matches} is called with default parameters by
#' \code{\link{identify_matches}}. It is therefore usually only necessary to
#' run \code{confirm_matches} on its own in order to change the parameters.
#'
#' @param data A data frame produced from \code{\link{identify_matches}}.
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
#' \code{\link{match_signatures}}.
#' @return The input `data` data frame, with an additional `match` field which 
#' is a character vector with the following possible values:
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
#' @examples
#' \dontrun{
#' # Setup
#' sigs <- create_signature(test_urls)
#' matches <- match_signatures(sigs)
#' 
#' # compare_images is only necessary to run if identify_matches was run with `confirm = FALSE`
#' matches <- identify_matches(matches, confirm = FALSE)
#' confirm <- confirm_matches(matches, check_threshold = 0.995)
#' }
#' @export

confirm_matches <- function(data, check_threshold = 0.99,
                            confirm_thresholds = c(0.95, 0.98), 
                            quiet = FALSE, ...) {

  # Error handling
  stopifnot(is.data.frame(data), is.numeric(check_threshold), 
            is.numeric(confirm_thresholds), is.logical(quiet))

  # Subset data
  to_check <- which(data$correlation < check_threshold)

  # Exit early if to_check is empty
  if (length(to_check) == 0) {
    data$match <- "match"
    return(data)
  }
  
  # Match by colour
  data$colour <- 2
  data[to_check,]$colour <- 
    match_signatures_pairwise(data[to_check,]$x_sig, data[to_check,]$y_sig)
  
  # Compile results
  data$match <- NA_character_
  data[data$colour < confirm_thresholds[1],]$match <- "no match"
  data[data$colour >= confirm_thresholds[1],]$match <- "possible match"
  data[data$colour >= confirm_thresholds[2],]$match <- "likely match"
  data[data$colour > 1,]$match <- "match"
  data$colour <- NULL

  # Return result
  return(data)

}
