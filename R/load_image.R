#' Load image from file or URL
#'
#' \code{load_image} is a vectorized version of \code{imager::load.image}. It
#' optionally supports parallel processing (via \code{future} and
#' \code{future.apply}) and progress reporting (via \code{progressr}).
#'
#' The function is a wrapper around \code{imager::load.image} with three
#' enhancements. It can take a vector of input paths instead a single path, it
#' supports parallel processing, and it support progress reporting.
#'
#' @param file A vector of file paths or URLs to be passed to
#' \code{imager::load.image}.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return If the input is a single file path or URL, the output will be a
#' single object of class 'cimg'. If the input is a vector with length > 1,
#' the output will be a list of 'cimg' objects with the same length.
#' @export

load_image <- function(file, quiet = FALSE) {

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

  ## Create NULL progress bar in case progressr is not installed ---------------

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
              "Loading image :current of :total ",
              "(:tick_rate/s) [:bar] :percent, ETA: :eta"))),
            show_after = 0
          ))

      } else {

        # Otherwise use default text
        progressr::handlers(
          progressr::handler_progress(
            format = paste0(
              "Loading image :current of :total ",
              "(:tick_rate/s) [:bar] :percent, ETA: :eta"),
            show_after = 0
          ))
      }

    } else quiet <- TRUE
  }


  ### Import and process images ################################################

  ## Import images with proper progress handling -------------------------------

  if (!quiet) {

    progressr::with_progress({

      pb <- progressr::progressor(steps = length(file))

      imgs <- lapply(file, function(x) {
        pb()
        tryCatch(imager::load.image(x), error = function(e) {
          warning("Input '", x, "' is invalid; output is NULL.", call. = FALSE)
          NULL
          })
      })
    })

  } else imgs <- lapply(file, function(x) {

    tryCatch(imager::load.image(x), error = function(e) {
      warning("Input '", x, "' is invalid; output is NULL.", call. = FALSE)
      NULL
    })

    })


  ## Name list elements with file names ----------------------------------------

  names(imgs) <- file


  ## Collapse list if length == 1 ----------------------------------------------

  if (length(imgs) == 1) imgs <- imgs[[1]]


  ## Return output -------------------------------------------------------------

  return(imgs)

}
