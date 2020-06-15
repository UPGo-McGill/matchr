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
