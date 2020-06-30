#' Create a new matchr_sig object
#'
#' @param x A numeric vector.
#' @param file A character string, corresponding to the path or URL of the file
#' from which the signature has been generated.
#' @param method A character string, either "greyscale" or "rgb", depending on
#' the method of signature generation used.
#' @param aspect_ratio A numeric scalar, giving the aspect ratio of the image.
#' @return An object of class `matchr_sig`.
#' @export

new_matchr_sig <- function(x, file, method, aspect_ratio) {

  stopifnot(is.numeric(x))
  stopifnot(is.character(file))
  stopifnot(is.character(method))
  stopifnot(is.numeric(aspect_ratio))

  structure(x,
            class = c("matchr_sig", "numeric"),
            file = file,
            method = method,
            aspect_ratio = aspect_ratio
            )

}

#' Create a new matchr_sig_list object
#'
#' @param x A list of matchr_sig objects.
#' @return An object of class `matchr_sig_list`.
#' @export

new_matchr_sig_list <- function(x) {

  stopifnot(is.list(x))

  structure(x, class = c("matchr_sig_list", "list"))

}


new_matchr_matrix <- function(x, x_aspect_ratio, y_aspect_ratio) {

  stopifnot(is.matrix(x))
  stopifnot(is.numeric(x_aspect_ratio))
  stopifnot(is.numeric(y_aspect_ratio))

  structure(x,
            class = c("matchr_matrix", "matrix"),
            x_aspect_ratio = x_aspect_ratio,
            y_aspect_ratio = y_aspect_ratio
            )

}


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


new_matchr_change_table <- function(x) {

  stopifnot(is.data.frame(x))

  structure(x,
            class = c("matchr_change_table", class(x))
            )

}
