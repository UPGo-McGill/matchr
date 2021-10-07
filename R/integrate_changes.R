#' Integrate changes in image matching
#'
#' \code{integrate_changes} takes the results of manual image comparison using
#' the \code{\link{compare_images}} Shiny app and integrates them into the data
#' frame of image matches produced by \code{\link{identify_matches}} or
#' \code{\link{confirm_matches}}. Matches whose status was confirmed as either
#' "match" or "no match" will have their value in the `match` field updated
#' accordingly, and will additionally receive a TRUE value for a new `confirmed`
#' field which \code{integrate_changes} adds. Matches whose status was not
#' confirmed will have their `match` value unchanged, and will received a
#' `confirmed` value of FALSE. Matches which were highlighted using the in-app
#' interface will receive a TRUE value for a new `highlight` field.
#'
#' @param result A data frame produced by \code{\link{identify_matches}} (with
#' `confirm = TRUE` or with \code{\link{confirm_matches}} subsequently run on 
#' the results). Any additional fields added to the data frame will be preserved 
#' in the output.
#' @param change_table A data frame produced by \code{\link{compare_images}}.
#' @return The original data frame given in the `result` argument, with three
#' changes: 1) matches which were confirmed as "match" or "no match" will have 
#' their value in the `match` field updated accordingly; 2) a `confirmed` field 
#' will be added if not already present, and matches which were confirmed will 
#' receive a value of TRUE (others will be FALSE); and 3) a `highlight` field
#' will be added with a value of TRUE for matches highlighted using the in-app 
#' interface and FALSE otherwise.
#' @examples
#' \dontrun{
#' # Setup
#' sigs <- create_signature(test_urls)
#' matches <- match_signatures(sigs)
#' result <- identify_matches(matches)
#' 
#' # Use compare_images to manually verify match status
#' change_table <- compare_images(result)
#' 
#' # Then integrate the changes into `result`
#' result <- integrate_changes(result, change_table)
#' }
#' @export

integrate_changes <- function(result, change_table) {
  
  stopifnot(is.data.frame(result), is.data.frame(change_table))
  output <- result
  change <- change_table
  
  # Expand index list column
  output[c("matrix", "x_index", "y_index")] <- 
    t(matrix(unlist(output$index), nrow = 3))
  output$index <- NULL
  change[c("matrix", "x_index", "y_index")] <- 
    t(matrix(unlist(change$index), nrow = 3))
  change$index <- NULL
  
  # Merge results
  output <- merge(output, change, all.x = TRUE)
  if (suppressWarnings(is.null(output$match))) {
    output$match <- output$new_match_status
  } else output[!is.na(output$new_match_status),]$match <-
    output[!is.na(output$new_match_status),]$new_match_status
  output$new_match_status <- NULL
  
  # Update highlight field
  if (suppressWarnings(is.null(output$highlight))) {
    output$highlight <- output$new_highlight
  } else output[!is.na(output$new_highlight),]$highlight <- 
    output[!is.na(output$new_highlight),]$new_highlight
  output$new_highlight <- NULL
  
  # Re-merge index fields
  output$index <- mapply(c, output$matrix, output$x_index, output$y_index, 
                         SIMPLIFY = FALSE)
  output <- output[c("index", "x_sig", "y_sig", "distance", setdiff(
    names(output), c("index", "matrix", "x_index", "y_index", "x_sig",
                     "y_sig", "distance")))]
  
  # Return output
  if (requireNamespace("dplyr", quietly = TRUE)) output <- dplyr::as_tibble(
    output)
  return(output)
}
