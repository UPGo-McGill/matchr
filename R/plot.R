#' Plot a matchr_image vector
#' 
#' @param x Vector of class `matchr_image`
#' @param path Logical scalar. Should the image's path be displayed above the
#' plot (default TRUE)?
#' @param max_plot Positive integer. The maximum number of images to plot at 
#' once. 
#' @param n_rows Either "auto" or a positive integer. The number of rows with 
#' which to plot images. If "auto", six or fewer images will be arranged in an 
#' adaptive layout which maximizes legibility, while seven or more images will 
#' always be arranged in a three-column grid.
#' @param ... Not used.
#' @export

plot.matchr_image <- function(x, path = TRUE, max_plot = 12, n_rows = "auto", 
                              ...) {
  
  # Check arguments
  stopifnot(is.logical(path), is.numeric(max_plot))
  stopifnot(n_rows == "auto" || is.numeric(n_rows))
  
  # Exit early if there's nothing to plot
  if (sum(is.na(x)) == vctrs::vec_size(x)) {
    warning("No non-NA images to plot")
    return(invisible(x))
  }
  
  # Trim to the first {max_plot} valid images
  y <- x[!is.na(x)]
  if (sum(is.na(x)) > 0 || length(y) > max_plot) {
    message("Only the first ", max_plot, " non-NA images will be plotted.")
    y <- y[seq_len(min(max_plot, length(y)))]
  }
  
  # Plot function
  img_plot <- function(x) {
    if (is.na(dim(x)[,3])) {
      r <- grDevices::gray(t(get_array(x)[[1]]))
    } else r <- grDevices::rgb(t(get_array(x)[[1]][,,1]),
                               t(get_array(x)[[1]][,,2]),
                               t(get_array(x)[[1]][,,3]))
    dim(r) <- dim(get_array(x)[[1]])[1:2]
    class(r) <- "raster"
    plot(r)
    if (path) {
      t <- sub("^.*/", "", get_path(x))
      graphics::title(t)  
    }
    
  }
  
  # Magic formula for grid cells
  n <- length(y)
  if (n_rows != "auto") {
    dims <- c(n_rows, ceiling(n / n_rows))
  } else dims <- c(ceiling(n / 3) + n %in% c(2, 3, 5, 6), 
                   min(ceiling((n + 2) / 4), 3))
  
  # Plot images
  old_par <- graphics::par(mfrow = dims, mai = c(0.25, 0.1, 0.25, 0.1))
  lapply(y, img_plot)
  graphics::par(old_par)
  invisible(x)
  
}

# ------------------------------------------------------------------------------

#' Plot a matchr_signature vector
#' 
#' @param x Vector of class `matchr_signature`
#' @param max_plot Positive integer. The maximum number of signatures to plot at 
#' once (default 20). 
#' @param n_rows Either "auto" or a positive integer. The number of rows with 
#' which to plot images. If "auto", nine or fewer images will be arranged in an 
#' adaptive layout which maximizes legibility, while ten or more images will 
#' always be arranged in a four-column grid.
#' @param ... Not used.
#' @export

plot.matchr_signature <- function(x, max_plot = 20, n_rows = "auto", ...) {
  
  # Check arguments
  stopifnot(is.numeric(max_plot))
  stopifnot(n_rows == "auto" || is.numeric(n_rows))
  
  # Exit early if there's nothing to plot
  if (sum(is.na(x)) == vctrs::vec_size(x)) {
    warning("No non-NA signatures to plot")
    return(invisible(x))
  }
  
  # Trim to the first {max_plot} valid signatures
  y <- x[!is.na(x)]
  if (sum(is.na(x)) > 0 || length(y) > max_plot) {
    message("Only the first ", max_plot, " non-NA signatures will be plotted.")
    y <- y[seq_len(min(max_plot, length(y)))]
  }
  
  # Create rasters
  rasters <- 
    y |> 
    get_hash() |> 
    lapply(matrix, nrow = 8) |> 
    lapply(grDevices::as.raster)
  
  
  # Plot function
  img_plot <- function(y, img) {
    plot(img)
    t <- sub("^.*/", "", get_path(y))
    graphics::title(t)
  }
  
  # Magic formula for grid cells
  n <- length(y)
  if (n_rows != "auto") {
    dims <- c(n_rows, ceiling(n / n_rows))
  } else dims <- c(ceiling(n / 4)  + n %in% c(3, 4, 7, 8), 
                   min(ceiling((n + 3) / 4) + (n == 5), 4))
  
  # Plot images
  old_par <- graphics::par(mfrow = dims, mai = c(0.25, 0.1, 0.25, 0.1))
  mapply(img_plot, y, rasters)
  graphics::par(old_par)
  invisible(x)
  
}
