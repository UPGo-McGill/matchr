#' Create a new matchr_image object
#'
#' @param x A list of pixel arrays.
#' @param path A character string, corresponding to the path or URL of the files
#' from which the arrays have been generated.
#' @return An object of class `matchr_image`.

new_image <- function(x = list(), path = character()) {
  vec_assert(x, list())
  vec_assert(path, character())
  new_rcrd(fields = list(array = x, path = path), class = "matchr_image")
}

# ------------------------------------------------------------------------------

#' Test if the object is a matchr_image
#' 
#' This function returns TRUE for `matchr_image` objects and FALSE for all other
#' objects.
#' 
#' @param x An object to test
#' @return A logical scalar, TRUE if `x` inherits from class "matchr_image" and 
#' FALSE otherwise.
#' @examples
#' \dontrun{
#' # Setup
#' img <- load_image(test_urls)
#' 
#' # TRUE
#' is_image(img)
#' 
#' # FALSE
#' is_image("text")
#' }
#' @export

is_image <- function(x) {
  inherits(x, "matchr_image")
}

# ------------------------------------------------------------------------------

#' @export

format.matchr_image <- function(x, ...) {
  
  max_chars <- getOption("width")
  d <- dim(x)
  cols <- ifelse(is.na(d[,3]), "greyscale", "RGB")
  cols[is.na(x)] <- "NA"
  path <- get_path(x)
  path_length <- nchar(path)
  path_max <- max_chars - nchar(cols) - 
    apply(d, 1, function(x) sum(nchar(x[1:2]), na.rm = TRUE)) - 12
  path <- ifelse(path_length > path_max, 
                 paste0("...", substr(path, path_length - path_max + 4, 
                                      path_length)), path)
  
  msg_1 <- sprintf('%i x %i, %s', d[,2], d[,1], cols)
  msg_1[is.na(x)] <- NA_character_
  msg_2 <- sprintf(', %s', path)
  msg <- paste0(msg_1, msg_2)
  msg
}

# ------------------------------------------------------------------------------

#' @export

vec_ptype_abbr.matchr_image <- function(x, ...) {
  "image"
}

# ------------------------------------------------------------------------------

#' @export

is.na.matchr_image <- function(x, ...) is.na(get_array(x))

# ------------------------------------------------------------------------------

#' @export

dim.matchr_image <- function(x, ...) {
  dims <- lapply(get_array(x), dim)
  dims[sapply(dims, is.null)] <- NA
  dims[lengths(dims) < 3] <- lapply(dims[lengths(dims) < 3], function(x) x[1:3])
  do.call(rbind, dims)
}

# ------------------------------------------------------------------------------

#' @export

length.matchr_image <- function(x) vctrs::vec_size(x)
