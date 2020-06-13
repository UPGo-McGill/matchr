#' Match images based on colour signatures
#'
#' \code{match_signatures} takes one or two lists of images and produces a
#' correlation matrix to identify matches.
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
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A correlation matrix. If `x` and `y` are both present, the matrix
#' will have `length(x)` rows and `length(y)` columns, and for the matrix `Q`
#' the cell `Q[i, j]` will be the Pearson correlation coefficient between images
#' `x[[i]]` and `y[[j]]`. If `y` is not present, the matrix will be square, and
#' the cell `Q[i, j]` will be the correlation between images `x[[i]]` and
#' `x[[j]]`.
#' @export

match_signatures <- function(x, y = NULL, quiet = FALSE) {

  ### Handle future options ####################################################

  parallel <- FALSE

  if (requireNamespace("future", quietly = TRUE)) {

    if (!requireNamespace("future.apply", quietly = TRUE)) {
      warning("Please install the `future.apply` package to enable ",
              "parallel processing.", call. = FALSE, immediate. = TRUE)
    }

    if (requireNamespace("future.apply", quietly = TRUE)) {

      if (future::nbrOfWorkers() > 1) parallel <- TRUE

      # Overwrite lapply with future.lapply for parallel processing
      lapply <- future.apply::future_lapply

    }
  }


  ### Handle progressr options #################################################

  ## Create NULL progress functions in case {progressr} is not installed -------

  with_progress <- function(expr) expr
  progressor <- function(...) NULL


  ## Disable progress reporting for single thread or fewer than 100 items ------

  if (length(x) < 100 || !parallel) quiet <- TRUE


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
              "Correlating image :current of :total ",
              "(:tick_rate/s) [:bar] :percent, ETA: :eta"))),
            show_after = 0
          ))

      } else {

        # Otherwise use default text
        progressr::handlers(
          progressr::handler_progress(
            format = paste0(
              "Correlating image :current of :total ",
              "(:tick_rate/s) [:bar] :percent, ETA: :eta"),
            show_after = 0
          ))
      }

    } else quiet <- TRUE
  }

  ## Process names -------------------------------------------------------------

  x_names <- names(x)
  if (!missing(y)) y_names <- names(y)


  ## Convert to matrix for single thread ---------------------------------------

  if (!parallel) {

    # Process matrices
    x <- matrix(unlist(x), ncol = length(x))
    if (!missing(y)) y <- matrix(unlist(y), ncol = length(y))

    # Calculate correlation matrix
    suppressWarnings({
      if (missing(y)) result <- stats::cor(x) else result <- stats::cor(x, y)
    })

    # Add names
    rownames(result) <- x_names
    if (missing(y)) colnames(result) <- x_names else colnames(result) <- y_names

    # Return result and exit function
    return(result)

  }


  ## Split data for multithreaded processing -----------------------------------

  chunks <- future::nbrOfWorkers() * 4
  chunk_size <- ceiling(length(x) / chunks)

  data_list <- vector("list", chunks)

  for (i in seq_len(chunks)) {

    start <- (i - 1) * chunk_size + 1
    end <- min(i * chunk_size, length(x))

    data_list[[i]] <-
      matrix(unlist(x[start:end]), ncol = length(start:end))

  }


  ## Correlate matrices --------------------------------------------------------

  # Suppress sd = 0 warnings
  with_progress({suppressWarnings({

    pb <- progressor(along = x)

    if (missing(y)) {

      x_matrix <- matrix(unlist(x), ncol = length(x))
      result <- lapply(data_list, function(.x) {
        pb(amount = length(.x))
        stats::cor(.x, x_matrix)
      })

    } else {

      y_matrix <- matrix(unlist(y), ncol = length(y))
      result <- lapply(data_list, function(.x) {
        pb(amount = length(.x))
        stats::cor(.x, y_matrix)
      })
    }
  })})


  ## Combine output and return result ------------------------------------------

  result <- do.call(rbind, result)

  rownames(result) <- x_names
  if (missing(y)) colnames(result) <- x_names else colnames(result) <- y_names

  return(result)

}
