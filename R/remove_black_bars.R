#' Remove horizontal black bars from an image
#'
#' \code{remove_black_bars} takes an image file and removes black bars at the
#' top and bottom of the image, if any are identified.
#'
#' TKTK
#'
#' @param image Object of class `matchr_img`.
#' @param bands Integer scalar. The number of horizontal bands the image should
#' be split into for processing.
#' @return An object of class `matchr_img`.

# remove_black_bars <- function(image, bands = 20) {
# 
#   row_split <- imager::imsplit(image, "y", bands)
#   row_means <- lapply(row_split, rowMeans)
#   row_means <- sapply(row_means, mean)
# 
#   if (sum(row_means == 0) > 0) {
# 
#     # Save file name to reappend later
#     file <- attr(image, "file")
# 
#     black_strips <- which(row_means == 0)
#     top_bound <- max(black_strips[black_strips <= bands / 2]) + 1
#     bottom_bound <- min(black_strips[black_strips > bands / 2]) - 1
# 
#     image <- imager::imappend(row_split[top_bound:bottom_bound], "y")
#     attr(image, "file") <- file
# 
#   }
# 
#   return(image)
# 
# }
