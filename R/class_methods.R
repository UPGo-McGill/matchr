#' @method print matchr_sig
#' @export

print.matchr_sig <- function(x, ...) {

  max_chars <- getOption("width")
  first_line_chars <- max_chars - 48
  file_length <- nchar(attr(x, "file"))
  file_trunc <- substr(attr(x, "file"), max(1, file_length - first_line_chars),
                       file_length)

 msg <- sprintf('Image signature from file "%s". Aspect ratio %.2f.',
                file_trunc, attr(x, "aspect_ratio"))

 values <- sprintf("%.2f", as.numeric(x))
 values <- paste0(values, collapse = ", ")
 values <- substr(values, 1, nchar(msg) - 3)
 values <- paste0(values, '...')

 if (requireNamespace("crayon", quietly = TRUE)) {
   values <- crayon::silver(crayon::italic(values))
 }

 cat(msg)
 cat("\n")
 cat(values)

 invisible(x)

}


#' @method print matchr_matrix
#' @export

print.matchr_matrix <- function(x, ...) {

  msg <- paste0("# An image matrix: ",
                prettyNum(dim(x)[[1]], ","),
                ' x ',
                prettyNum(dim(x)[[2]], ","))

  x_ratio <- attr(x, "x_aspect_ratio")
  x_ratio <- round(x_ratio, 2)
  x_ratio <- paste0(x_ratio, collapse = " - ")
  x_ratio <- paste0("x aspect ratios: ", x_ratio)

  y_ratio <- attr(x, "y_aspect_ratio")
  y_ratio <- round(y_ratio, 2)
  y_ratio <- paste0(y_ratio, collapse = " - ")
  y_ratio <- paste0("y aspect ratios: ", y_ratio)

  if (requireNamespace("crayon", quietly = TRUE)) {
    x_ratio <- crayon::silver(crayon::italic(x_ratio))
    y_ratio <- crayon::silver(crayon::italic(y_ratio))
  }

  cat(msg)
  cat("\n")
  cat(x_ratio)
  cat("\n")
  cat(y_ratio)
  cat("\n")

  invisible(x)

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
                     " matrices")

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

    x_ratios <- round(attr(x[[n]], "x_aspect_ratio"), 2)
    x_ratios <- paste0(formatC(x_ratios, 2, format = "f"), collapse = " - ")
    y_ratios <- round(attr(x[[n]], "y_aspect_ratio"), 2)
    y_ratios <- paste0(formatC(y_ratios, 2, format = "f"), collapse = " - ")

    msg <- paste0(lead_number,
                  ": ",
                  x_number,
                  " x ",
                  y_number,
                  " (aspect ratios: x ",
                  x_ratios,
                  ", y ",
                  y_ratios,
                  ")\n")

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
