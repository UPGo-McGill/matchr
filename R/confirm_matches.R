#' Confirm matches using a more detailed colour signature
#'
#' \code{confirm_matches} TKTK
#'
#' TKTK
#'
#' @param data TKTK
#' @param check_threshold TKTK
#' @param confirm_thresholds TKTK
#' @param quiet TKTK
#' @return TKTK
#' @export

confirm_matches <- function(data, check_threshold = 0.99,
                            confirm_thresholds = c(0.95, 0.98), quiet = FALSE) {

  ### Error handling and object initialization #################################

  stopifnot(is.numeric(check_threshold))
  stopifnot(is.numeric(confirm_thresholds))
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


  ### Subset data and load relevant images #####################################

  to_check <- data[data$correlation < check_threshold,]

  x_samples <- identify_image(to_check$x_name, method = "rgb")
  y_samples <- identify_image(to_check$y_name, method = "rgb")

  to_check$colour <- match_signatures_pairwise(x_samples, y_samples)


  ### Compile results ##########################################################

  ## Use colour signatures to indicate likelihood of matches -------------------

  to_check$confirmation <- "no match"

  to_check[to_check$colour >= confirm_thresholds[[1]],]$confirmation <-
    "possible match"

  to_check[to_check$colour >= confirm_thresholds[[2]],]$confirmation <-
    "likely match"

  data <- data[data$correlation >= check_threshold,]
  data$confirmation <- "match"


  ## Rbind data and to_check ---------------------------------------------------

  if (requireNamespace("dplyr", quietly = TRUE)) {
    data <- dplyr::bind_rows(data, to_check)
  } else data <- rbind(data, to_check)

  data$colour <- NULL

  data <- data[order(data$matrix, data$x_index, data$y_index),]

  data$confirmation <- factor(data$confirmation,
                              levels = c("no match", "possible match",
                                         "likely match", "match"),
                              ordered = TRUE)


  ## Return result -------------------------------------------------------------

  return(data)

}


