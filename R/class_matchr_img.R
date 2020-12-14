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

  if (is.na(x)) print(NA) else {
    class(x) <- setdiff(class(x), "matchr_img")
    print(x)
    }

}

#' @method is.na matchr_img
#' @export

is.na.matchr_img <- function(x, ...) {

  if (length(x) == 1 && is.na(unclass(x))) TRUE else FALSE

}
