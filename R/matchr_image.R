#' Create a new matchr_image object
#'
#' @param x A list of pixel arrays.
#' @param file A character string, corresponding to the path or URL of the files
#' from which the arrays have been generated.
#' @return An object of class `matchr_image`.

new_image <- function(x = list(), file = character()) {
  vec_assert(x, list())
  vec_assert(file, character())
  new_rcrd(fields = list(array = x, file = file), class = "matchr_image")
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
  file <- field(x, "file")
  file_length <- nchar(file)
  file_max <- max_chars - nchar(cols) - 
    apply(d, 1, function(x) sum(nchar(x[1:2]), na.rm = TRUE)) - 12
  file <- ifelse(file_length > file_max, 
                 paste0("...", substr(file, file_length - file_max + 4, 
                                      file_length)), file)
  
  msg_1 <- sprintf('%i x %i, %s', d[,2], d[,1], cols)
  msg_1[is.na(x)] <- NA_character_
  msg_2 <- sprintf(', %s', file)
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

is.na.matchr_image <- function(x, ...) is.na(field(x, "array"))

# ------------------------------------------------------------------------------

#' @export

dim.matchr_image <- function(x, ...) {
  dims <- lapply(field(x, "array"), dim)
  dims[sapply(dims, is.null)] <- NA
  dims[lengths(dims) < 3] <- lapply(dims[lengths(dims) < 3], function(x) x[1:3])
  do.call(rbind, dims)
}

# ------------------------------------------------------------------------------

#' @export

plot.matchr_image <- function(x, ...) {
  
  # Temporarily trim to just the first image
  if (vec_size(x) > 1) {
    warning("Only the first image will be plotted.", call. = FALSE)
    x <- x[1]
  }
  
  # Plot greyscale
  if (is.na(dim(x)[,3])) {
    
    r <- grDevices::gray(t(field(x, "array")[[1]]))
    dim(r) <- dim(field(x, "array")[[1]])[1:2]
    class(r) <- "raster"
    plot(r)
    invisible(x)
    
    # Plot colour
  } else {
    
    r <- grDevices::rgb(t(field(x, "array")[[1]][,,1]),
                        t(field(x, "array")[[1]][,,2]),
                        t(field(x, "array")[[1]][,,3]))
    dim(r) <- dim(field(x, "array")[[1]])[1:2]
    class(r) <- "raster"
    plot(r)
    invisible(x)
    
  }
}
  