#' Create a new matchr_img object
#'
#' @param x An object of class `cimg`.
#' @param file A character string, corresponding to the path or URL of the file
#' from which the image has been generated.
#' @return An object of class `matchr_img`.
#' @export

new_matchr_img <- function(x, file) {

  stopifnot(imager::is.cimg(x) || is.na(x))
  stopifnot(is.character(file))

  structure(x,
            class = append(class(x), "matchr_img", after = 0),
            file = file)

}

#' @method print matchr_img
#' @export

print.matchr_img <- function(x, ...) {

  # Exit early on NA
  if (anyNA(x)) {
    cat(NA)
    return(invisible(x))
  }

  d <- dim(x)
  file <- attr(x, "file")
  max_chars <- getOption("width")
  file_max <- max_chars - 38 - sum(nchar(d))
  file_length <- nchar(attr(x, "file"))

  if (file_length > file_max) {

    file_trunc <-
      paste0("...", substr(file, file_length - file_max + 4, file_length))

    } else file_trunc <- file


  msg <- sprintf(
    'Image from file "%s". %i x %i, %i colour channels.\n',
    file_trunc, d[1], d[2], d[4])

  cat(msg)
  invisible(x)

}

#' @method is.na matchr_img
#' @export

is.na.matchr_img <- function(x, ...) {

  if (length(x) == 1 && is.na(unclass(x))) TRUE else FALSE

}
