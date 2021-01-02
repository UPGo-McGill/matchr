#' Create a new matchr_sig object
#'
#' @param x A numeric vector.
#' @param file A character string, corresponding to the path or URL of the file
#' from which the signature has been generated.
#' @param aspect_ratio A numeric scalar, giving the aspect ratio of the image.
#' @return An object of class `matchr_sig`.
#' @export

new_matchr_sig <- function(x, file, aspect_ratio) {

  stopifnot(is.numeric(x))
  stopifnot(is.character(file))
  stopifnot(is.numeric(aspect_ratio))

  structure(x,
            class = c("matchr_sig", "numeric"),
            file = file,
            aspect_ratio = aspect_ratio
  )

}


#' @method print matchr_sig
#' @export

print.matchr_sig <- function(x, ...) {

  # Exit early on NA
  if (anyNA(x)) {
    cat(NA)
    cat("\n")
    return(invisible(x))
    }

  max_chars <- getOption("width")
  first_line_chars <- max_chars - 49
  file_length <- nchar(attr(x, "file"))
  file_trunc <- substr(attr(x, "file"), max(1, file_length - first_line_chars),
                       file_length)

  msg <- sprintf('Image signature from file "%s". Aspect ratio %.2f.',
                 file_trunc, attr(x, "aspect_ratio"))

  if (requireNamespace("crayon", quietly = TRUE)) {

    if (crayon::has_color()) {

      # Greyscale values
      g_values <- sprintf("%.2f", as.numeric(x[1:(length(x) / 4)]))
      g_values <- substr(g_values, 3, 4)
      g_values <- ifelse(substr(g_values, 1, 1) == "0", substr(g_values, 2, 2),
                         g_values)
      g_values <- sapply(g_values, function(x) crayon::style("\u25a0",
                                                             paste0("grey", x)))

      # RGB values
      c_values <- ceiling(unclass(x[(length(x) / 4 + 1):length(x)]) * 16) * 11
      c_values <- as.character(unname(c(c_values, recursive = TRUE)))
      c_values <- ifelse(c_values == "0", "00", c_values)
      c_values <- ifelse(c_values == "110", "AA", c_values)
      c_values <- ifelse(c_values == "121", "BB", c_values)
      c_values <- ifelse(c_values == "132", "CC", c_values)
      c_values <- ifelse(c_values == "143", "DD", c_values)
      c_values <- ifelse(c_values == "154", "EE", c_values)
      c_values <- ifelse(c_values == "165", "FF", c_values)
      c_values <- ifelse(c_values == "176", "FF", c_values)

      c_values[1:(length(c_values) / 3)] <-
        paste0("#", c_values[1:(length(c_values) / 3)], "0000")

      c_values[(1 + length(c_values) / 3):(2 * length(c_values) / 3)] <-
        paste0(
          "#00",
          c_values[(1 + length(c_values) / 3):(2 * length(c_values) / 3)], "00")

      c_values[(1 + 2 * length(c_values) / 3):length(c_values)] <-
        paste0("#0000",
               c_values[(1 + 2 * length(c_values) / 3):length(c_values)])

      c_values <-
        sapply(c_values,
               function(x) crayon::style("\u25a0",
                                         crayon::make_style(x, colors = 256)))

      values <- c(g_values, c_values)

      if (length(values) > min(max_chars, 80)) {

        sect_len <- floor(min(max_chars, 80) / 4)

        values <- values[c(seq_len(sect_len),
                           seq_len(sect_len) + length(values) * 0.25,
                           seq_len(sect_len) + length(values) * 0.5,
                           seq_len(sect_len) + length(values) * 0.75)]

        values <- paste0(values, collapse = "")

      } else values <- paste0(values, collapse = "")

    } else {

      values <- sprintf("%.2f", as.numeric(x))
      values <- paste0(values, collapse = ", ")
      values <- substr(values, 1, nchar(msg) - 3)
      values <- paste0(values, '...')

      values <- crayon::silver(crayon::italic(values))

    }

  } else {

    values <- sprintf("%.2f", as.numeric(x))
    values <- paste0(values, collapse = ", ")
    values <- substr(values, 1, nchar(msg) - 3)
    values <- paste0(values, '...')

  }

  cat(msg)
  cat("\n")
  cat(values)
  cat("\n")

  invisible(x)

}


#' @method is.na matchr_sig
#' @export

is.na.matchr_sig <- function(x, ...) {

  if (mean(is.na(unclass(x))) == 1) TRUE else FALSE

}
