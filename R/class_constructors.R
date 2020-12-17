

new_matchr_matrix <- function(x, x_aspect_ratio, y_aspect_ratio) {

  stopifnot(is.matrix(x))
  stopifnot(is.numeric(x_aspect_ratio) | is.null(x_aspect_ratio))
  stopifnot(is.numeric(y_aspect_ratio) | is.null(y_aspect_ratio))

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
