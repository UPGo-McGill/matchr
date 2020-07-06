#' Generate a unique colour signature for an image
#'
#' \code{identify_image} takes an image file and generates a numeric vector of
#' average colour values, so that the image can be compared to others.
#'
#' The function works as follows. An image is decomposed into horizontal bands
#' (with the number of bands controlled by the `bands` argument), and for each
#' band an average greyscale or colour value is calculated. The vector of these
#' averages becomes a distinctive signature that can identify a given image even
#' if the image is rescaled or compressed, and thus serves as a reliable
#' indicator of whether two images are the same.
#'
#' @param image Object (or list of objects) of class 'cimg', probably imported
#' using \code{load_image} or \code{imager::load.image}, or file path
#' or URL (or vector/list of file paths or URLs) of an image.
#' @param method Character string. The method to be used to create image
#' signatures. Valid options are "greyscale" or "rgb". The former will take
#' a single average darkness value for each band while the latter will take
#' average values for each of the red, green and blue colour channels for each
#' band.
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

identify_image <- function(image, method = "greyscale", bands = 20,
                           rm_black_bars = TRUE, ...) {

  UseMethod("identify_image")

}


#' @rdname identify_image
#' @method identify_image cimg
#' @export

identify_image.cimg <- function(image, method = "greyscale", bands = 20,
                                rm_black_bars = TRUE, ...) {

  ### Error checking ###########################################################

  stopifnot(method %in% c("greyscale", "rgb"), is.numeric(bands),
            is.logical(rm_black_bars))


  ### Return NA if the image doesn't have enough rows or columns ###############

  if (dim(image)[[1]] < bands || dim(image)[[2]] < bands) {

    return(
      new_matchr_sig(
        rep(NA_real_, times = bands * if (method == "greyscale") 2 else 3),
        if (is.null(attr(image, "file"))) NA_character_ else attr(image,
                                                                  "file"),
        NA_character_,
        NA_real_)
    )

  }


  ### Split the image then check for black bars ################################

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

      if (method == "greyscale") {
        row_split <- imager::imsplit(image, "y", bands)
        row_means <- lapply(row_split, rowMeans)
        row_means <- sapply(row_means, mean)
      }


    }
  }


  ### Finish process if method is "greyscale" ##################################

  if (method == "greyscale") {
    col_split <- imager::imsplit(image, "x", bands)
    col_means <- lapply(col_split, colMeans)
    col_means <- sapply(col_means, mean)
    result <- c(row_means, col_means)
  }


  ### Finish process if method is "rgb" ########################################

  if (method == "rgb") {

    # Fail safe in case image is greyscale
    if (dim(image)[[4]] == 1) {
      colour_split <- imager::as.imlist(list(image, image, image))
      } else colour_split <- imager::channels(image, 1:3)

    row_split <- lapply(colour_split, imager::imsplit, "y", bands)
    row_split <- unlist(row_split, recursive = FALSE)
    row_means <- lapply(row_split, rowMeans)
    row_means <- sapply(row_means, mean)

    col_split <- lapply(colour_split, imager::imsplit, "x", bands)
    col_split <- unlist(col_split, recursive = FALSE)
    col_means <- lapply(col_split, colMeans)
    col_means <- sapply(col_means, mean)

    result <- c(row_means, col_means)

  }


  ### Construct matchr_sig object and return result ############################

  result <- new_matchr_sig(
    result,
    if (is.null(attr(image, "file"))) NA_character_ else attr(image, "file"),
    method,
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

identify_image.character <- function(image, method = "greyscale", bands = 20,
                                     rm_black_bars = TRUE,
                                     batch_size = 100, quiet = FALSE, ...) {

  ### Error checking ###########################################################

  stopifnot(method %in% c("greyscale", "rgb"), is.numeric(bands),
            is.logical(rm_black_bars), is.numeric(batch_size),
            is.logical(quiet))


  ### Prepare iterations and result according to batch_size argument ###########

  iterations <- ceiling(length(image) / batch_size)

  # With >= 10 batches, replace progress reporting
  if (iterations >= 10) {



  }

  result <- vector("list", iterations)


  ### Run function #############################################################

  for (i in seq_len(iterations)) {

    start <- (i - 1) * batch_size + 1
    end <- min(i * batch_size, length(image))

    if (iterations > 1) message("Processing batch ", i, " of ", iterations, ".")

    # Produce list of cimg objects
    result[[i]] <- load_image(image[start:end], quiet = quiet)

    # Send list through the identify_image.list method
    result[[i]] <- identify_image(result[[i]], method, bands,
                                  rm_black_bars = rm_black_bars, quiet = quiet)

  }

  if (inherits(result[[1]], "list")) {
    result <- unlist(result, recursive = FALSE)
    result <- new_matchr_sig_list(result)
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

identify_image.list <- function(image, method = "greyscale", bands = 20,
                                rm_black_bars = TRUE, quiet = FALSE, ...) {

  ### Error checking ###########################################################

  stopifnot(method %in% c("greyscale", "rgb"), is.numeric(bands),
            is.logical(rm_black_bars), is.logical(quiet))


  ### Run function #############################################################

  handler_matchr("Analyzing image")

  with_progress({

    pb <- progressor(along = image)
    result <- par_lapply(image, function(x) {
      pb()
      identify_image(x, method, bands, rm_black_bars = rm_black_bars)
      })

  })

  result <- new_matchr_sig_list(result)

  return(result)
}


#' @rdname identify_image
#' @method identify_image default
#' @export

identify_image.default <- function(image, method = "greyscale", bands = 20,
                                   rm_black_bars = TRUE, ...) {

  stopifnot(method %in% c("greyscale", "rgb"), is.numeric(bands),
            is.logical(rm_black_bars))

  new_matchr_sig(
    rep(NA_real_, times = bands * if (method == "greyscale") 2 else 3),
    if (is.null(attr(image, "file"))) NA_character_ else attr(image, "file"),
    NA_character_,
    NA_real_
  )
}
