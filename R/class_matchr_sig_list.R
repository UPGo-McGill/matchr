#' Create a new matchr_sig_list object
#'
#' @param x A list of matchr_sig objects.
#' @return An object of class `matchr_sig_list`.
#' @export

new_matchr_sig_list <- function(x) {

  stopifnot(is.list(x))

  structure(x, class = c("matchr_sig_list", "list"))

}


#' @method print matchr_sig_list
#' @export

print.matchr_sig_list <- function(x, ...) {

  list_msg <- paste0('# An image signature list: ',
                     prettyNum(length(x), ","),
                     " signatures")

  # Deal with length-one list
  if (length(x) == 1) list_msg <- substr(list_msg, 1, nchar(list_msg) - 1)

  # Truncate list if there are more than 12 elements
  if (length(x) > 12) {

    extra_row <- paste0("# \u2026 with ", length(x) - 10,
                        " more signatures\n")

    x_display <- x[1:10]

  } else {

    x_display <- x

    # Add "\n" if final element is NA
    if (is.na(x_display[[length(x_display)]])) {
      extra_row <- "\n"} else extra_row <- NULL
  }

  cat(list_msg)
  cat("\n")
  lapply(x_display, print)
  cat(extra_row)

  invisible(x)

}

#' @method c matchr_sig_list
#' @export

c.matchr_sig_list <- function(...) {

  dots <- lapply(list(...), unclass)
  new_matchr_sig_list(do.call(c, dots))

}
