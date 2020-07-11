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

  stopifnot(is.data.frame(result))

  result <- merge(result, change_table, all.x = TRUE)

  stopifnot(sum(!is.na(result$new_match_status), na.rm = TRUE) ==
              nrow(change_table))

  result[!is.na(result$new_match_status),]$confirmation <-
    result$new_match_status

  result$new_match_status <- NULL

  return(result)
}


