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
