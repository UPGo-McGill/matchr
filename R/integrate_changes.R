#' Integrate changes in image matching
#'
#' \code{integrate_changes} TKTK
#'
#' TKTK
#'
#' @param result TKTK
#' @param change_table TKTK
#' @return TKTK
#' @export

integrate_changes <- function(result, change_table) {

  stopifnot(is.data.frame(result), is.data.frame(change_table))
  result <- merge(result, change_table[c(1:4, 7:8)], all.x = TRUE)
  stopifnot(sum(!is.na(result$new_match_status), na.rm = TRUE) ==
              nrow(change_table))
  result[!is.na(result$new_match_status),]$match <-
    result[!is.na(result$new_match_status),]$new_match_status

  # Keep confirmed column if it already exists
  if (suppressWarnings(!is.null(result$confirmed))) {
    result$confirmed <- ifelse(is.na(result$new_match_status), result$confirmed,
                               TRUE)
  } else result$confirmed <- ifelse(is.na(result$new_match_status), FALSE, TRUE)

  result$new_match_status <- NULL
  if (requireNamespace("dplyr", quietly = TRUE)) result <- dplyr::as_tibble(
    result)
  
  result <- result[c("matrix", "x_index", "y_index", "x_sig", "y_sig",
                     "correlation", "match", "confirmed")]

  return(result)
}
