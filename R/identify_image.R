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
#' @param mem_limit An integer scalar. How many images should the function load
#' into memory before extracting image signatures and releasing the associated
#' memory? Higher values will lead to the function executing more quickly, but
#' can result in enormous memory requirements. The function uses approximately
#' 10 MB per image, so the default value of 50 will cause peak memory usage of
#' approximately 5 GB.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @export

identify_image.character <- function(image, bands = 25, mem_limit = 50,
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


  ### Prepare iterations and results according to mem_limit argument ###########

  iterations <- ceiling(length(image) / mem_limit)

  suppressWarnings({data_list <- split(image, seq_len(iterations))})
  results <- vector("list", iterations)


  ### Run function #############################################################

  for (i in seq_len(iterations)) {

    results[[i]] <- load_image(data_list[[i]], quiet = quiet)

    if (inherits(results[[i]], "list")) {

      results[[i]] <- identify_image(results[[i]], bands, quiet = quiet)

    } else {

      image_split <- imager::imsplit(results[[i]], "x", bands)
      image_means <- lapply(image_split, rowMeans)
      image_means <- sapply(image_means, mean)
      results[[i]] <- image_means

      }
  }

  results <- unlist(results, recursive = FALSE)

  names(results) <- image

  return(results)

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

        pb <- progressr::progressor(along = image)
        results <- lapply(image, function(x) {
          pb()
          identify_image(x, bands)
        })
      })

    } else results <- lapply(image, function(x) identify_image(x, bands))

  return(results)
}

