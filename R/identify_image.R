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
#' @param bands Integer scalar. The number of horizontal and vertical bands the
#' image should be split into for processing. Higher values will produce a more
#' distinctive colour signature, potentially decreasing the rate of matching
#' false positives, but at the cost of increased processing time and an
#' increased rate of matching false negatives.
#' @param rm_black_bars Logical scalar. Should horizontal black bars be
#' detected and removed from the image signature? Because these bands lead to an
#' image signature dominated by black values, leaving them in the signature can
#' lead to false positive matches.
#' @param ... Additional arguments passed to methods.
#' @return An object of class `matchr_sig`, which contains a numeric vector of
#' length `bands` * 2, a file name (optionally), and an aspect ratio.
#' @export

identify_image <- function(image, bands = 20, rm_black_bars = TRUE, ...) {

  UseMethod("identify_image")

}


#' @rdname identify_image
#' @method identify_image cimg
#' @export

identify_image.cimg <- function(image, bands = 20, rm_black_bars = TRUE, ...) {

  stopifnot(is.numeric(bands))
  stopifnot(is.logical(rm_black_bars))

  row_split <- imager::imsplit(image, "y", bands)
  row_means <- lapply(row_split, rowMeans)
  row_means <- sapply(row_means, mean)

  # Check for black bars
  if (rm_black_bars) {

    # First check for all black image and return NA if so
    if (sum(row_means) == 0) return(identify_image.default(image))

    if (sum(row_means == 0) > 0) {

      black_strips <- which(row_means == 0)
      suppressWarnings({
        top_bound <- max(black_strips[black_strips <= bands / 2]) + 1
      })
      if (is.infinite(top_bound)) top_bound <- 1
      suppressWarnings({
        bottom_bound <- min(black_strips[black_strips > bands / 2]) - 1
      })
      if (is.infinite(bottom_bound)) bottom_bound <- bands

      image <- structure(
        imager::imappend(row_split[top_bound:bottom_bound], "y"),
        file = attr(image, "file")
      )

      row_split <- imager::imsplit(image, "y", bands)
      row_means <- lapply(row_split, rowMeans)
      row_means <- sapply(row_means, mean)

    }
  }

  col_split <- imager::imsplit(image, "x", bands)
  col_means <- lapply(col_split, colMeans)
  col_means <- sapply(col_means, mean)
  result <- c(row_means, col_means)

  result <- new_matchr_sig(
    result,
    if (is.null(attr(image, "file"))) NA_character_ else attr(image, "file"),
    imager::width(image) / imager::height(image))

  return(result)

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

identify_image.character <- function(image, bands = 20, rm_black_bars = TRUE,
                                     batch_size = 100, quiet = FALSE, ...) {

  ### Error checking ###########################################################

  stopifnot(is.numeric(bands))
  stopifnot(is.logical(rm_black_bars))


  ### Prepare iterations and result according to batch_size argument ###########

  iterations <- ceiling(length(image) / batch_size)
  result <- vector("list", iterations)


  ### Run function #############################################################

  for (i in seq_len(iterations)) {

    start <- (i - 1) * batch_size + 1
    end <- min(i * batch_size, length(image))

    # Produce list of cimg objects
    result[[i]] <- load_image(image[start:end], quiet = quiet)

    # Send list through the identify_image.list method
    result[[i]] <- identify_image(result[[i]], bands,
                                  rm_black_bars = rm_black_bars, quiet = quiet)

  }

  if (inherits(result[[1]], "list")) {
    result <- unlist(result, recursive = FALSE)
  } else {
    result <- result[[1]]
  }

  return(result)

}


#' @rdname identify_image
#' @method identify_image list
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @export

identify_image.list <- function(image, bands = 20, rm_black_bars = TRUE,
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


  ### Handle progressr options #################################################

  ## Create NULL progress functions in case {progressr} is not installed -------

  with_progress <- function(expr) expr
  progressor <- function(...) function(...) NULL


  ## Disable progress reporting for fewer than 10 items ------------------------

  if (length(image) < 10) quiet <- TRUE


  ## Set up progress bar -------------------------------------------------------

  if (!quiet) {

    if (requireNamespace("progressr", quietly = TRUE)) {

      with_progress <- progressr::with_progress
      progressor <- progressr::progressor

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

  with_progress({

    pb <- progressor(along = image)
    result <- lapply(image, function(x) {
      pb()
      identify_image(x, bands, rm_black_bars = rm_black_bars)
      })

  })

  return(result)
}


#' @rdname identify_image
#' @method identify_image default
#' @export

identify_image.default <- function(image, bands = 20, rm_black_bars = TRUE,
                                   ...) {

  new_matchr_sig(
    rep(NA_real_, times = bands * 2),
    if (is.null(attr(image, "file"))) NA_character_ else attr(image, "file"),
    NA_real_
  )
}

