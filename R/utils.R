#' Get image signatures from a matchr_matrix vector
#'
#' TKTK
#'
#' TKTK
#'
#' @param x TKTK
#' @param x_or_y TKTK
#' @return TKTK
#' @export

get_sig <- function(x, x_or_y) {
  field(x, paste0(x_or_y, "_sig"))
}
