#' Create a new matchr_change_table object
#'
#' @param x A data frame.
#' @return An data frame of class `matchr_change_table`.
#' @export

new_matchr_change_table <- function(x) {

  stopifnot(is.data.frame(x))

  structure(x,
            class = c("matchr_change_table", class(x))
  )

}
