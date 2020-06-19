#' Identify matches from a matchr image matrix
#'
#' \code{identify_matches} TKTK
#'
#' TKTK
#'
#' @param image_matrix A matrix of image correlations produced by
#' \code{imager::match_images}.
#' @param threshold A numeric scalar. The minimum correlation constant to
#' consider to images to be matched.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return TKTK
#' @export

identify_matches <- function(image_matrix, threshold = 0.99, quiet = FALSE) {

  ### Error handling and object initialization #################################

  stopifnot(inherits(image_matrix, "matchr_matrix_list"))
  stopifnot(is.numeric(threshold))
  stopifnot(is.logical(quiet))


  ### Handle future options ####################################################

  parallel <- FALSE

  if (requireNamespace("future", quietly = TRUE)) {

    options(future.globals.maxSize = +Inf)

    if (!requireNamespace("future.apply", quietly = TRUE)) {
      warning("Please install the `future.apply` package to enable ",
              "parallel processing.", call. = FALSE, immediate. = TRUE)
    }

    if (requireNamespace("future.apply", quietly = TRUE)) {

      if (future::nbrOfWorkers() > 1) parallel <- TRUE

      # Overwrite *apply with future.apply for parallel processing
      lapply <- future.apply::future_lapply

    }
  }


  ### Handle progressr options #################################################

  ## Create NULL progress functions in case {progressr} is not installed -------

  with_progress <- function(expr) expr
  progressor <- function(...) function(...) NULL


  ## Disable progress reporting for single thread or fewer than 100 items ------

  if (length(image_matrix) < 5 || !parallel) quiet <- TRUE


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
              "Identifying matches, batch :current of :total ",
              "(:tick_rate/s) [:bar] :percent, ETA: :eta"))),
            show_after = 0
          ))

      } else {

        # Otherwise use default text
        progressr::handlers(
          progressr::handler_progress(
            format = paste0(
              "Identifying matches, batch :current of :total ",
              "(:tick_rate/s) [:bar] :percent, ETA: :eta"),
            show_after = 0
          ))
      }

    } else quiet <- TRUE
  }


  ### Find matches #############################################################

  ## Run lapply loop -----------------------------------------------------------

  with_progress({

    pb <- progressor(along = image_matrix)

    match_list <- lapply(image_matrix, function(x) {

      pb()

      # Find matches
      match_index <- which(x >= threshold, arr.ind = TRUE)

      # Name results
      dimnames(match_index)[[2]] <- c("x_index", "y_index")

      # Convert to tibble or data frame
      if (requireNamespace("tibble", quietly = TRUE)) {
        matches <- tibble::as_tibble(match_index)
      } else matches <- as.data.frame(match_index)

      # Add names
      matches$x_name <- rownames(x)[matches$x_index]
      matches$y_name <- colnames(x)[matches$y_index]
      matches$correlation <- x[match_index]

      matches

    })

  })


  ## Consolidate and arrange output --------------------------------------------

  for (i in seq_along(match_list)) {

    match_list[[i]]$matrix <- i
    match_list[[i]] <- match_list[[i]][c(6, 1:5)]

  }


  if (requireNamespace("dplyr", quietly = TRUE)) {
    matches <- dplyr::bind_rows(match_list)
  } else matches <- do.call(rbind, match_list)

  # Arrange output
  matches <- matches[order(matches$matrix, matches$x_index, matches$y_index),]


  # TKTK TEMPORARILY DISABLED
  # Remove redundant matches if the matrix is generated from a single list
  # if (dim(image_matrix)[[1]] == dim(image_matrix)[[2]]) {
  #   if (mean(rownames(image_matrix) == colnames(image_matrix)) == 1) {
  #     # Remove self matches
  #     matches <- matches[matches$x_index != matches$y_index,]
  #   }
  # }

  # Remove duplicate matches
  pairs <-
    mapply(function(m, x, y) sort(c(m, x, y)),
           matches$matrix,
           matches$x_index,
           matches$y_index,
           SIMPLIFY = FALSE)

  matches <- matches[!duplicated(pairs),]

  return(matches)

}
