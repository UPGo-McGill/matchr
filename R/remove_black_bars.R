#' Remove horizontal black bars from an image vector
#'
#' \code{remove_black_bars} takes a `matchr_image` vector, a pixel array, or a
#' list of pixel arrays and removes black bars at the top and bottom of the 
#' images, if any are identified.
#'
#' @param x Vector of class `matchr_image`, pixel array, or list of pixel 
#' arrays.
#' @param ... Not used.
#' @return Object of the same class and length as the input `x`, but with black 
#' bars removed.
#' @examples
#' \dontrun{
#' # Setup
#' img <- load_image(test_urls)
#' 
#' # Use remove_black_bars on matchr_image vectors
#' remove_black_bars(img)
#' 
#' # Or on raw pixel arrays, singly or in a list
#' remove_black_bars(get_array(img))
#' remove_black_bars(get_array(img)[[13]])
#' }
#' @export

remove_black_bars <- function(x, ...) {
  
  UseMethod("remove_black_bars")
  
}

# ------------------------------------------------------------------------------

#' @rdname remove_black_bars
#' @method remove_black_bars matchr_image
#' @export

remove_black_bars.matchr_image <- function(x, ...) {
  
  # Get arrays and array dimensions
  array <- get_array(x[!is.na(x)])
  dims <- lapply(array, dim)
  
  # Get colour arrays to check
  img_c <- which(lengths(lapply(array, dim)) == 3)
  if (length(img_c) > 0) {
    check_c <- sapply(array[img_c], 
                      \(x) mean(x[1,1,]) < 0.1 || mean(x[nrow(x),1,]) < 0.1)
    img_c <- img_c[check_c]
  }
  
  # Get greyscale arrays to check
  img_g <- which(lengths(lapply(array, dim)) == 2)
  if (length(img_g) > 0) {
    check_g <- sapply(array[img_g], \(x) x[1,1] < 0.1 || x[nrow(x),1] < 0.1)
    img_g <- img_g[check_g]
  }
  
  # Delegate to helper functions for candidates which passed the check
  array_c <- rm_bb_i_c(array[img_c])
  array_g <- rm_bb_i_g(array[img_g])
  array[img_c] <- array_c
  array[img_g] <- array_g
  array <- mapply(dim_fixer, array, dims, SIMPLIFY = FALSE)

  # Return result  
  get_array(x[!is.na(x)]) <- array
  return(x)
  
}

# ------------------------------------------------------------------------------

#' @rdname remove_black_bars
#' @method remove_black_bars list
#' @export

remove_black_bars.list <- function(x, ...) {
  
  # Error checking and object initialization
  stopifnot(all(unique(unlist(sapply(x, class))) %in% 
                  c("array", "matrix", "logical")))
  array <- x[!is.na(x)]
  dims <- lapply(array, dim)
  
  # Get colour arrays to check
  img_c <- which(lengths(lapply(array, dim)) == 3)
  check_c <- sapply(array[img_c], \(x) 
                    mean(x[1,1,]) < 0.1 || mean(x[nrow(x),1,]) < 0.1)
  img_c <- img_c[check_c]
  
  # Get greyscale arrays to check
  img_g <- which(lengths(lapply(array, dim)) == 2)
  check_g <- sapply(array[img_g], \(x) x[1,1] < 0.1 || x[nrow(x),1] < 0.1)
  img_g <- img_g[check_g]
  
  # Delegate to helper functions for candidates which passed the check
  array_c <- rm_bb_i_c(array[img_c])
  array_g <- rm_bb_i_g(array[img_g])
  array[img_c] <- array_c
  array[img_g] <- array_g
  array <- mapply(dim_fixer, array, dims, SIMPLIFY = FALSE)
  
  # Return result  
  x[!is.na(x)] <- array
  return(x)
  
}

# ------------------------------------------------------------------------------

#' @rdname remove_black_bars
#' @method remove_black_bars array
#' @export

remove_black_bars.array <- function(x, ...) {
  
  # Colour array
  if (length(dim(x)) == 3 && 
      (mean(x[1,1,]) < 0.1 || mean(x[nrow(x),1,]) < 0.1)) {
    x <- rm_bb_c(x)
  }
  
  # Greyscale array
  if (length(dim(x)) == 2 && (x[1,1] < 0.1 || x[nrow(x),1] < 0.1)) {
    x <- rm_bb_g(x)
  }

  # Return result  
  return(x)
  
}

# ------------------------------------------------------------------------------

#' @rdname remove_black_bars
#' @method remove_black_bars logical
#' @export

remove_black_bars.logical <- function(x, ...) x
  
# ------------------------------------------------------------------------------

dim_fixer <- function(x, y) {
  y[1] <- length(x) / prod(y[-1])
  dim(x) <- y
  x
}
