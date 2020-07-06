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

  stopifnot(is.data.frame(result),
            inherits(change_table, "matchr_change_table"),
            identical(result[change_table$match_index,]$x_name,
                      change_table$x_name),
            identical(result[change_table$match_index,]$y_name,
                      change_table$y_name))

  result[change_table$match_index,]$confirmation <-
    change_table$new_match_status

  return(result)
}


