#' Remove horizontal black bars from an image
#'
#' \code{remove_black_bars} takes a single `matchr_image` or raw pixel array and 
#' removes black bars at the top and bottom of the image, if any are identified.
#'
#' @param x Pixel array or length-1 vector of class `matchr_image`.
#' @return An array if the input was an array, and a length-1 vector of class 
#' `matchr_image` if the input was a `matchr_image` vector.
#' @export

remove_black_bars <- function(x) {
  
  if (is_image(x)) {
    img_check <- TRUE
    x_img <- x
    x <- field(x, "array")[[1]]
  } else img_check <- FALSE
  
  rm_1 <- rowMeans(x[,,1])
  rm_2 <- rowMeans(x[,,2])
  rm_3 <- rowMeans(x[,,3])
  
  rm_total <- (rm_1 + rm_2 + rm_3) / 3
  
  # First check for all black image and return NA if so
  if (mean(rm_total) < 0.005) {
    if (img_check) return(new_image(list(NA), field(x_img, "file")))
    return(NA)
  }
  
  if (sum(rm_total < 0.005) > 0) {
    
    black_strips <- which(rm_total < 0.005)
    black_strips <- black_strips[apply(x[black_strips,,1:3], 1, function(x) 
      sum(x > 0.02)) == 0]
    
    if (black_strips[1] != 1) top_bound <- 1 else {
      # Get largest index position which is in a continuous sequence with 1
      top_bound <- sapply(seq_along(black_strips), function(x) 
        length(seq_len(black_strips[x])) == length((black_strips[seq_len(x)])))
      top_bound <- max(which(top_bound)) + 1L
    }
    
    if (black_strips[length(black_strips)] != length(rm_total)) {
      bottom_bound <- length(rm_total)
    } else {
      # Get smallest index position which is in a continuous sequence with end
      bottom_bound <- sapply(seq_along(black_strips), function(x) 
        length(seq.int(black_strips[x], max(black_strips))) == 
          length(seq.int(x, length(black_strips)))
      )
      bottom_bound <- black_strips[min(which(bottom_bound))] - 1L
    }
    
    x <- x[top_bound:bottom_bound,,]
    
  }
  
  if (img_check) {
    field(x_img, "array")[[1]] <- x
    x <- x_img
  }
  
  x
  
}