#' Generate a unique colour signature for an image
#'
#' \code{identify_image} takes an image file and generates a numeric vector of
#' average colour values, so that the image can be compared to others.
#'
#' The function works as follows. An image is decomposed into horizontal bands
#' (with the number of bands controlled by the `bands` argument), and for each
#' band an average colour is calculated. The vector of these colour averages
#' becomes a distinctive signature that can identify a given image even if the
#' image is rescaled or compressed, and thus serves as a reliable indicator of
#' whether two images are the same.
#'
#' @param image Object (or list of objects) of class 'cimg', probably imported
#' using \code{load_image} or \code{imager::load.image}, or file path
#' or URL (or vector/list of file paths or URLs) of an image.
#' @param bands Integer scalar. The number of horizontal bands the image should
#' be split into for processing. Higher values will produce a more distinctive
#' colour signature, potentially decreasing the rate of matching false
#' positives, but at the cost of increased processing time and an increased rate
#' of matching false negatives.
#' @param ... Additional arguments passed to methods. E.g. `quiet` to control
#' whether the function provides progress updates.
#' @return A numeric vector of length `bands`.
#' @export

identify_image <- function(image, bands = 25, ...) {

  UseMethod("identify_image")

}


#' @rdname identify_image
#' @method identify_image cimg
#' @export

identify_image.cimg <- function(image, bands = 25, ...) {

  image_split <- imager::imsplit(image, "x", bands)
  image_means <- lapply(image_split, rowMeans)
  image_means <- sapply(image_means, mean)

  return(image_means)

}


#' @rdname identify_image
#' @method identify_image character
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @export

identify_image.character <- function(image, bands = 25, quiet = FALSE, ...) {

  image <- load_image(image, quiet = quiet)

  if (inherits(image, "list")) return(
    identify_image(image, bands, quiet = quiet))

  image_split <- imager::imsplit(image, "x", bands)
  image_means <- lapply(image_split, rowMeans)
  image_means <- sapply(image_means, mean)

  return(image_means)

}


#' @rdname identify_image
#' @method identify_image list
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @export

identify_image.list <- function(image, bands = 25, quiet = FALSE, ...) {

  ### Handle future options ####################################################

  if (requireNamespace("future", quietly = TRUE)) {

    if (!requireNamespace("future.apply", quietly = TRUE)) {
      warning("Please install the `future.apply` package to enable ",
              "parallel processing.", call. = FALSE, immediate. = TRUE)
    }

    if (requireNamespace("future.apply", quietly = TRUE)) {

      # Overwrite lapply with future.lapply for parallel processing
      lapply <- future.apply::future_lapply

    }
  }


  ### Handle progressr options #################################################

  ## Create NULL progress bar in case {progressr} is not installed -------------

  pb <- function() NULL


  ## Disable progress reporting for fewer than 10 items ------------------------

  if (length(image) < 10) quiet <- TRUE


  ## Set up progress bar -------------------------------------------------------

  if (!quiet) {

    if (requireNamespace("progressr", quietly = TRUE)) {

      if (requireNamespace("crayon", quietly = TRUE)) {

        # Used styled text if crayon package is present
        progressr::handlers(
          progressr::handler_progress(
            format = crayon::silver(crayon::italic(paste0(
              "Analyzing image :current of :total ",
              "(:tick_rate/s) [:bar] :percent, ETA: :eta"))),
            show_after = 0
          ))

      } else {

        # Otherwise use default text
        progressr::handlers(
          progressr::handler_progress(
            format = paste0(
              "Analyzing image :current of :total ",
              "(:tick_rate/s) [:bar] :percent, ETA: :eta"),
            show_after = 0
          ))
      }

    } else quiet <- TRUE
  }


  ### Run function #############################################################

  if (!quiet) {

    progressr::with_progress({

      pb <- progressr::progressor(steps = length(image))
      results <- lapply(image, function(x) {
        pb()
        identify_image(x, bands)
        })
    })
  } else results <- lapply(image, function(x) identify_image(x, bands))

  return(results)
}

