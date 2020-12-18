#' Create a new matchr_matrix object
#'
#' @param x A matrix of correlations.
#' @param x_aspect_ratio A numeric scalar.
#' @param y_aspect_ratio A numeric scalar.
#' @return An object of class `matchr_matrix`.
#' @export

new_matchr_matrix <- function(x, x_aspect_ratio, y_aspect_ratio) {

  stopifnot(is.matrix(x))
  stopifnot(is.numeric(x_aspect_ratio) || is.null(x_aspect_ratio))
  stopifnot(is.numeric(y_aspect_ratio) || is.null(y_aspect_ratio))

  structure(x,
            class = c("matchr_matrix", "matrix"),
            x_aspect_ratio = x_aspect_ratio,
            y_aspect_ratio = y_aspect_ratio
  )

}


#' @method print matchr_matrix
#' @export

print.matchr_matrix <- function(x, ...) {

  msg <- paste0("# An image matrix: ",
                prettyNum(dim(x)[[1]], ","),
                ' x ',
                prettyNum(dim(x)[[2]], ","))

  cat(msg)
  cat("\n")

  x_ratio <- attr(x, "x_aspect_ratio")

  if (!is.null(x_ratio)) {

    x_ratio <- round(x_ratio, 2)
    x_ratio <- paste0(x_ratio, collapse = " - ")
    x_ratio <- paste0("x aspect ratios: ", x_ratio)

    if (requireNamespace("crayon", quietly = TRUE)) {
      x_ratio <- crayon::silver(crayon::italic(x_ratio))
    }

    cat(x_ratio)
    cat("\n")

  }

  y_ratio <- attr(x, "y_aspect_ratio")

  if (!is.null(y_ratio)) {

    y_ratio <- round(y_ratio, 2)
    y_ratio <- paste0(y_ratio, collapse = " - ")
    y_ratio <- paste0("y aspect ratios: ", y_ratio)

    if (requireNamespace("crayon", quietly = TRUE)) {
      y_ratio <- crayon::silver(crayon::italic(y_ratio))
    }

    cat(y_ratio)
    cat("\n")

  }

  invisible(x)

}
