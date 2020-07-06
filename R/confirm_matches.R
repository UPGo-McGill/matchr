#' Confirm matches using a more detailed colour signature
#'
#' \code{confirm_matches} TKTK
#'
#' TKTK
#'
#' @param data TKTK
#' @param check_threshold TKTK
#' @param confirm_thresholds TKTK
#' @param batch_size TKTK
#' @param quiet TKTK
#' @return TKTK
#' @export

confirm_matches <- function(data, check_threshold = 0.99,
                            confirm_thresholds = c(0.95, 0.98),
                            batch_size = 100,
                            quiet = FALSE) {

  ### Error handling and object initialization #################################

  stopifnot(is.numeric(check_threshold))
  stopifnot(is.numeric(confirm_thresholds))
  stopifnot(is.logical(quiet))


  ### Subset data and load relevant images #####################################

  to_check <- data[data$correlation < check_threshold,]

  # Exit early if to_check is empty
  if (nrow(to_check) == 0) {

    data$confirmation <- "match"
    return(data)

  }

  x_samples <- identify_image(to_check$x_name, method = "rgb",
                              batch_size = batch_size)
  y_samples <- identify_image(to_check$y_name, method = "rgb",
                              batch_size = batch_size)

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


  ## Return result -------------------------------------------------------------

  return(data)

}
