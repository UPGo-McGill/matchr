#' Match images based on colour signatures
#'
#' \code{match_signatures} takes one or two lists of images and produces a
#' correlation matrix to identify matches.
#'
#' A function for identifying matching images. The function takes a list of
#' images signatures (objects of class 'matchr_sig') and compares their colour
#' signatures to find matches.
#'
#' The comparison is done by creating colour signatures for each input image
#' using \code{\link{identify_image}} and then computing the Pearson correlation
#' coefficient between these signatures. In general, pairs of images which were
#' identical prior to arbitrary resampling and compression will have correlation
#' coefficients of at least 0.99.
#'
#' The function can optionally filter images by aspect ratio, so only images
#' with very similar aspect ratios will be compared. This can remove potential
#' false positives and possibly speed up function execution, if images are
#' relatively evenly split between aspect ratios.
#'
#' @param x,y Lists of 'matchr_sig' objects to be matched. If `y` is missing
#' (default), each object in `x` will be matched against each other object in
#' `x.` If `y` is present, each object in `x` will be matched against each
#' object in `y`.
#' @param compare_aspect_ratios A logical scalar. Should signatures only be
#' compared for images with similar aspect ratios (default)? If TRUE, k-means
#' clustering is used to identify breakpoints between aspect ratios that
#' maximize between-group distance and minimize the total number of calculations
#' that the function needs to execute. (Values of k between 3 and 15 are
#' evaluated.) Image signatures from `x` are split into lists between these
#' break points. In order to catch matches that would fall across a break point,
#' image signatures from `y` are split into lists between the break point / 1.2
#' on the lower bound and the break point * 1.2 on the upper bound. This
#' argument is currently only implemented when both `x` and `y` are provided to
#' the function.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return An object of class `matchr_matrix`. This object is a list containing
#' a number of correlation matrices equal to the number of aspect-ratio
#' categories. If `x` and `y` are both present, each matrix will have
#' `length(x)` rows and `length(y)` columns, and for the matrix `Q` the cell
#' `Q[i, j]` will be the Pearson correlation coefficient between images `x[[i]]`
#' and `y[[j]]`. If `y` is not present, each matrix will be square, and
#' the cell `Q[i, j]` will be the correlation between images `x[[i]]` and
#' `x[[j]]`.
#' @export

match_signatures <- function(x, y = NULL, compare_aspect_ratios = TRUE,
                             quiet = FALSE) {

  ### Error handling and object initialization #################################

  stopifnot(is.logical(compare_aspect_ratios), is.logical(quiet))

  # Temporarily disable compare_aspect_ratios unless y is provided
  if (missing(y)) compare_aspect_ratios <- FALSE


  ### Split clusters for compare_aspect_ratios == TRUE #########################

  if (compare_aspect_ratios == TRUE) {

    ## Get aspect ratios -------------------------------------------------------

    x_ratios <- sapply(x, attr, "aspect_ratio")
    y_ratios <- sapply(y, attr, "aspect_ratio")


    ## Calculate clusters which minimize calculations --------------------------

    max_group <- sapply(3:15, function(n) {

      means <- stats::kmeans(stats::na.omit(c(x_ratios, y_ratios)), n)
      centres <- sort(means$centers)
      cuts <-
        c(0, sapply(seq_len(n - 1), function(x) mean(centres[x:(x + 1)])), Inf)

      lengths_x <- vector("integer", n)
      lengths_y <- vector("integer", n)

      for (i in seq_len(n)) {

        lengths_x[[i]] <-
          length(which(x_ratios >= cuts[i] & x_ratios < cuts[i + 1]))
        lengths_y[[i]] <-
          length(which(
            y_ratios >= cuts[i] / 1.2 & y_ratios < cuts[i + 1] * 1.2))

      }

      sum(as.numeric(lengths_x) * as.numeric(lengths_y))

    })

    # Choose appropriate k value
    k <- which.min(max_group) + 2


    ## Recalculate cut points and add 1.2x buffer to y -------------------------

    means <- stats::kmeans(stats::na.omit(c(x_ratios, y_ratios)), k)
    centres <- sort(means$centers)
    cuts <-
      c(0, sapply(seq_len(k - 1), function(x) mean(centres[x:(x + 1)])), Inf)

    x_list <- vector("list", k)
    y_list <- vector("list", k)

    for (i in seq_len(k)) {

      x_index <- which(x_ratios >= cuts[i] & x_ratios < cuts[i + 1])
      y_index <- which(y_ratios >= cuts[i] / 1.2 & y_ratios < cuts[i + 1] * 1.2)

      x_list[[i]] <- x[x_index]
      y_list[[i]] <- y[y_index]

    }


    ## Get aspect ratios for later ---------------------------------------------

    x_aspect_ratios <- vector("list", k)
    y_aspect_ratios <- vector("list", k)

    for (i in seq_len(k)) {

      x_aspect_ratios[[i]] <- c(cuts[[i]], cuts[[i + 1]])
      y_aspect_ratios[[i]] <- c(cuts[[i]] / 1.2, cuts[[i + 1]] * 1.2)

    }

  } else {

    x_list <- list(x)
    y_list <- list(y)

    x_aspect_ratios <- list(c(0, Inf))
    y_aspect_ratios <- list(c(0, Inf))

  }


  ### Subdivide x lists for parallel processing ################################

  x_list <- lapply(x_list, function(x_elem) {

    chunks <- min(nbrOfWorkers() * 2, max(floor(length(x_elem) / 4), 1))
    chunk_size <- ceiling(length(x_elem) / chunks)

    # Check to make sure the last chunk won't be empty
    while (chunk_size * (chunks - 1) >= length(x_elem)) chunks <- chunks - 1

    data_list <- vector("list", chunks)

    for (i in seq_len(chunks)) {

      start <- (i - 1) * chunk_size + 1
      end <- min(i * chunk_size, length(x_elem))
      data_list[[i]] <- x_elem[start:end]

      }

    data_list

    })


  ### Calculate correlations ###################################################

  ## Version for x and y -------------------------------------------------------

  if (!missing(y)) {

    handler_matchr("Correlating images, batch")

    with_progress({

      pb <- progressor(along = x_list)

      result <- vector("list", length(x_list))

      for (n in seq_along(x_list)) {

        pb()

        # Retrieve names
        x_names <- lapply(x_list[[n]], sapply, attr, "file")
        y_names <- sapply(y_list[[n]], attr, "file")

        # Prepare matrices
        x_matrix <- lapply(x_list[[n]], function(x) {
          matrix(unlist(x), ncol = length(x))})

        y_matrix <- matrix(unlist(y_list[[n]]), ncol = length(y_list[[n]]))

        # Calculate correlation matrix
        suppressWarnings({
          result[[n]] <- par_mapply(function(x, names) {

            output <- stats::cor(x, y_matrix)
            rownames(output) <- names
            colnames(output) <- y_names

            return(output)

        }, x_matrix, x_names, SIMPLIFY = FALSE)})
      }
    })


  ## Do single matrix for just x -----------------------------------------------

  } else {

    # Get names
    x_names <- sapply(x, attr, "file")

    # Process matrices
    x <- matrix(unlist(x), ncol = length(x))

    # Calculate correlation matrix
    suppressWarnings({result <- stats::cor(x)})

    # Add names
    rownames(result) <- x_names
    colnames(result) <- x_names

    # Return result and exit function
    result <- list(result)

    }


  ### Construct matchr_matrix objects of out list elements #####################

  ## rbind ---------------------------------------------------------------------

  handler_matchr("Combining results, batch")

  with_progress({

    pb <- progressor(along = result)

    result <- lapply(result, function(x) {
      pb()
      do.call(rbind, x)
      })

  })


  ## Assign class --------------------------------------------------------------

  result <- mapply(new_matchr_matrix, result, x_aspect_ratios, y_aspect_ratios,
                   SIMPLIFY = FALSE)


  ### Construct matchr_matrix_list object out of result ########################

  new_matchr_matrix_list(result, length(x), length(y))

}
