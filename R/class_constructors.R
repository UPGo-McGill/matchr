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
