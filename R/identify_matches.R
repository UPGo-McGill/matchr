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

identify_matches <- function(image_matrix, threshold = 0.975, quiet = FALSE) {

  ### Error handling and object initialization #################################

  stopifnot(
    inherits(image_matrix, "matchr_matrix_list"),
    is.numeric(threshold),
    is.logical(quiet)
    )


  ### Find matches #############################################################

  ## Run lapply loop -----------------------------------------------------------

  handler_matchr("Identifying matches, batch")

  with_progress({

    pb <- progressor(along = image_matrix)

    match_list <- par_lapply(image_matrix, function(x) {

      pb()

      # Find matches
      match_index <- which(x >= threshold, arr.ind = TRUE)

      # Name results
      dimnames(match_index)[[2]] <- c("x_index", "y_index")

      # Convert to tibble or data frame
      if (requireNamespace("dplyr", quietly = TRUE)) {
        matches <- dplyr::as_tibble(match_index)
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
