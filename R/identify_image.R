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
#' @param ... Additional arguments passed to methods.
#' @return A numeric vector of length `bands`.
#' @export

identify_image <- function(image, bands = 20, ...) {

  UseMethod("identify_image")

}


#' @rdname identify_image
#' @method identify_image cimg
#' @export

identify_image.cimg <- function(image, bands = 20, ...) {

  row_split <- imager::imsplit(image, "x", bands)
  row_means <- lapply(row_split, rowMeans)
  col_split <- imager::imsplit(image, "y", bands)
  col_means <- lapply(col_split, colMeans)
  image_means <- c(sapply(row_means, mean), sapply(col_means, mean))

  return(image_means)

}


#' @rdname identify_image
#' @method identify_image character
#' @param batch_size An integer scalar. How many images should the function
#' load into memory before extracting image signatures and releasing the
#' associated memory? Higher values will lead to the function executing more
#' quickly, but can result in enormous memory requirements.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @export

identify_image.character <- function(image, bands = 20, batch_size = 100,
                                     quiet = FALSE, ...) {

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


  ### Prepare iterations and results according to batch_size argument ##########

  iterations <- ceiling(length(image) / batch_size)

  results <- vector("list", iterations)


  ### Run function #############################################################

  for (i in seq_len(iterations)) {

    start <- (i - 1) * batch_size + 1
    end <- min(i * batch_size, length(image))

    results[[i]] <- load_image(image[start:end], quiet = quiet)

    if (inherits(results[[i]], "list")) {

      results[[i]] <- identify_image(results[[i]], bands, quiet = quiet)

    } else {

      row_split <- imager::imsplit(results[[i]], "x", bands)
      row_means <- lapply(row_split, rowMeans)
      col_split <- imager::imsplit(results[[i]], "y", bands)
      col_means <- lapply(col_split, colMeans)
      results[[i]] <- c(sapply(row_means, mean), sapply(col_means, mean))

      }
  }

  results <- unlist(results, recursive = FALSE)

  if (inherits(results, "list")) names(results) <- image

  return(results)

}


#' @rdname identify_image
#' @method identify_image list
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @export

identify_image.list <- function(image, bands = 20, quiet = FALSE, ...) {

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

        pb <- progressr::progressor(along = image)
        results <- lapply(image, function(x) {
          pb()
          identify_image(x, bands)
        })
      })

    } else results <- lapply(image, function(x) identify_image(x, bands))

  return(results)
}


#' @rdname identify_image
#' @method identify_image logical
#' @export

identify_image.logical <- function(image, bands = 20, ...) {

  rep(NA, times = bands * 2)

}

