### PACKAGE HELPERS ############################################################

#' Wrapper for progressr::with_progress
#'
#' @param expr An expression to be evaluated.
#' @return The result of evaluating the expression.

with_progress <- function(expr) {

  quiet <- get("quiet", envir = parent.frame(n = 1))

  if (!quiet && requireNamespace("progressr", quietly = TRUE)) {

    progressr::with_progress(expr)

  } else expr

}


# Wrapper for progressr::progressor

progressor <-

  if (requireNamespace("progressr", quietly = TRUE)) {

    progressr::progressor

  } else function(...) function(...) NULL


# Wrapper for future::nbrOfWorkers

number_of_threads <- function() {

  if (requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE)) {

    future::nbrOfWorkers()

    } else 1

}


# Wrapper for lapply

par_lapply <- function(...) {

  if (requireNamespace("future", quietly = TRUE)) {

    if (requireNamespace("future.apply", quietly = TRUE)) {

      # Overwrite lapply with future.lapply for parallel processing
      future.apply::future_lapply(...)

    } else {

      message("Please install the `future.apply` package to enable ",
              "parallel processing.")

      lapply(...)

    }

  } else lapply(...)

}


par_mapply <- function(...) {

  if (requireNamespace("future", quietly = TRUE)) {

    if (requireNamespace("future.apply", quietly = TRUE)) {

      # Overwrite lapply with future.lapply for parallel processing
      future.apply::future_mapply(...)

    } else {

      message("Please install the `future.apply` package to enable ",
              "parallel processing.")

      mapply(...)

    }

  } else mapply(...)

}


#' Helper function to set a progress reporting strategy

#' @param message The message to be displayed
#' @return A progressr handler.

handler_matchr <- function(message) {

  if (requireNamespace("progressr", quietly = TRUE)) {

    if (!requireNamespace("progress", quietly = TRUE)) {

      progressr::handlers("txtprogressbar")

    } else {

      format_string <- paste0(
        message,
        " :current of :total (:tick_rate/s) [:bar] :percent, ETA: :eta")

      if (requireNamespace("crayon", quietly = TRUE)) {
        progressr::handlers(
          progressr::handler_progress(
            format = crayon::silver(crayon::italic(format_string)),
            show_after = 0
          ))
      } else {
        progressr::handlers(
          progressr::handler_progress(
            format = format_string,
            show_after = 0
          ))
      }
    }
  }
}
