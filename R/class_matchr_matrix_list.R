#' Create a new matchr_matrix_list object
#'
#' @param x A list of matchr_matrix objects.
#' @param x_files A numeric scalar: the number of files in the matrix rows.
#' @param y_files A numeric scalar: the number of files in the matrix columns
#' @return An object of class `matchr_matrix_list`.
#' @export

new_matchr_matrix_list <- function(x, x_files, y_files) {

  stopifnot(is.list(x))
  stopifnot(is.numeric(x_files))
  stopifnot(is.numeric(y_files))

  structure(x,
            class = c("matchr_matrix_list", "list"),
            x_files = x_files,
            y_files = y_files
  )

}


#' @method print matchr_matrix_list
#' @export

print.matchr_matrix_list <- function(x, ...) {

  list_msg <- paste0('# An image matrix list: ',
                     prettyNum(attr(x, "x_files"), ","),
                     " x ",
                     prettyNum(attr(x, "y_files"), ","),
                     " in ",
                     length(x),
                     if (length(x) > 1) " matrices" else " matrix")

  # Get max digits
  max_lead <- nchar(length(x))
  max_x <- max(sapply(x, function(x) dim(x)[[1]]))
  max_x <- nchar(max_x) + {max_x >= 1000} + {max_x >= 1000000}
  max_y <- max(sapply(x, function(x) dim(x)[[2]]))
  max_y <- nchar(max_y) + {max_y >= 1000} + {max_y >= 1000000}

  element_msgs <- lapply(seq_along(x), function(n) {

    lead_number <- formatC(n, width = max_lead, format = "fg")
    x_number <- formatC(dim(x[[n]])[[1]], width = max_x, format = "fg",
                        big.mark = ",")

    y_number <- formatC(dim(x[[n]])[[2]], width = max_y, format = "fg",
                        big.mark = ",")

    x_ratio <- attr(x[[n]], "x_aspect_ratio")
    y_ratio <- attr(x[[n]], "y_aspect_ratio")

    if (is.null(x_ratio) && is.null(y_ratio)) ratios <- "" else {

      x_ratio <- round(x_ratio, 2)
      x_ratio <- paste0(formatC(x_ratio, 2, format = "f"), collapse = " - ")

      y_ratio <- round(y_ratio, 2)
      y_ratio <- paste0(formatC(y_ratio, 2, format = "f"), collapse = " - ")

      ratios <- paste0(" (aspect ratios: x ", x_ratio, ", y ", y_ratio, ")")
      }

    msg <- paste0(lead_number,
                  ": ",
                  x_number,
                  " x ",
                  y_number,
                  ratios,
                  "\n")

    if (requireNamespace("crayon", quietly = TRUE)) {
      msg <- crayon::silver(crayon::italic(msg))
    }

    msg

  })

  if (length(element_msgs) > 12) {

    extra_row <- paste0("# \u2026 with ", length(element_msgs) - 10,
                        " more matrices")

    element_msgs <- c(element_msgs[1:10], extra_row)

  }

  cat(list_msg)
  cat("\n")
  lapply(element_msgs, cat)

  invisible(x)

}
