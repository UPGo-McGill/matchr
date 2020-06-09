#' Match images based on colour signatures
#'
#' \code{match_images} takes a list of images and produces a correlation matrix
#' to identify matches.
#'
#' A function for identifying matching images. The function takes a list of
#' images (objects of class 'cimg) and compares their colour signatures to find
#' matches.
#'
#' The comparison is done by creating colour signatures for each input image
#' using \code{\link{identify_image}} and then computing the Pearson correlation
#' coefficient between these signatures. In general, pairs of images which were
#' identical prior to arbitrary resampling and compression will have correlation
#' coefficients of at least 0.99.
#'
#' @param x,y Lists of 'cimg' objects to be matched, or file paths and URLs
#' pointing to images which can be imported as 'cimg' objects. If `y` is missing
#' (default), each object in `x` will be matched against each other object in
#' `x.` If `y` is present, each object in `x` will be matched against each
#' object in `y`.
#' @param bands Integer scalar to be passed to \code{\link{identify_image}}. The
#' number of horizontal bands the image should be split into for processing.
#' Higher values will produce a more distinctive colour signature, potentially
#' decreasing the rate of matching false positives, but at the cost of increased
#' processing time and an increased rate of matching false negatives.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A correlation matrix. If `x` and `y` are both present, the matrix
#' will have `length(x)` rows and `length(y)` columns, and for the matrix `Q`
#' the cell `Q[i, j]` will be the Pearson correlation coefficient between images
#' `x[[i]]` and `y[[j]]`. If `y` is not present, the matrix will be square, and
#' the cell `Q[i, j]` will be the correlation between images `x[[i]]` and
#' `x[[j]]`.
#' @export

match_images <- function(x, y = NULL, bands = 25, quiet = FALSE) {

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

  if (length(file) < 10) quiet <- TRUE


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


  ### Get image signatures #####################################################

  ## Get signatures for x ------------------------------------------------------

  if (!quiet) {

    progressr::with_progress({

      if (!missing(y)) {
        total_img <- length(x) + length(y)
      } else total_img <- length(x)

      pb <- progressr::progressor(steps = total_img)

      x_ids <- lapply(x, function(x) {
        pb()
        identify_image(x, bands = bands)
      })
    })
  } else x_ids <- lapply(x, identify_image, bands = bands)

  # Create matrix
  x_ids <- matrix(unlist(x_ids), ncol = length(x))


  ## Get signatures for y ------------------------------------------------------

  if (!missing(y)) {

    if (!quiet) {

      progressr::with_progress({

        y_ids <- lapply(y, function(x) {
          pb()
          identify_image(y, bands = bands)
        })
      })
    } else y_ids <- lapply(y, identify_image, bands = bands)

    # Create matrix
    y_ids <- matrix(unlist(y_ids), ncol = length(y))

  }


  ### Calculate correlation ####################################################

  # Currently single-threaded with no progress reporting

  if (missing(y)) stats::cor(x_ids) else stats::cor(x_ids, y_ids)

}
