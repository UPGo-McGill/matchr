### PACKAGE HELPERS ############################################################

number_of_threads <- function() {

  if (requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE)) {
    future::nbrOfWorkers()
    } else 1

}

# ------------------------------------------------------------------------------

par_lapply <- function(...) {

  if (requireNamespace("future", quietly = TRUE)) {
    if (requireNamespace("future.apply", quietly = TRUE)) {
      
      par_check <- TRUE
      
      if (exists("par_check", envir = parent.frame(n = 1), mode = "logical")) {
        par_check <- get("par_check", envir = parent.frame(n = 1))}
      
      if (par_check) future.apply::future_lapply(..., future.seed = NULL) else 
        lapply(...)
      
      } else {
      message("Please install the `future.apply` package to enable ",
              "parallel processing.")
      lapply(...)
    }
  } else lapply(...)
}

# ------------------------------------------------------------------------------

par_mapply <- function(...) {

  if (requireNamespace("future", quietly = TRUE)) {
    if (requireNamespace("future.apply", quietly = TRUE)) {

      par_check <- TRUE

      if (exists("par_check", envir = parent.frame(n = 1), mode = "logical")) {
        par_check <- get("par_check", envir = parent.frame(n = 1))}

      if (par_check) future.apply::future_mapply(..., future.seed = NULL) else
        mapply(...)

      } else {
        message("Please install the `future.apply` package to enable ",
                "parallel processing.")
        mapply(...)
    }
  } else mapply(...)
}

# ------------------------------------------------------------------------------

#' Helper function to set a progress reporting strategy

#' @param message The message to be displayed
#' @return A progressr handler.

handler_matchr <- function(message) {
  
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

# ------------------------------------------------------------------------------

#' Helper function to divide vector into equal-sized chunks
#' @param x The vector to be split
#' @param n_chunks The number of chunks to split into
#' @param max_chunk_size The maximum size of each chunk (default NULL)
#' @param workers A number to make max_chunk_size a multiple of (default NULL)
#' @return A list with n_chunks elements.

chunk <- function(x, n_chunks, max_chunk_size = NULL, workers = NULL) {
  
  n_chunks <- min(n_chunks, vec_size(x))
  
  if (missing(max_chunk_size) && !missing(workers)) {
    max_chunk_size <- ceiling(length(x) / n_chunks)
    max_chunk_size <- 
      unname(floor(max_chunk_size / number_of_threads()) * number_of_threads())
  }
  
  # Simple version which splits into n_chunks pieces
  if (missing(max_chunk_size)) {
    starts <- ceiling(seq.int(from = 1, to = length(x), 
                            by = length(x) / n_chunks))
    ends <- ceiling(pmin(seq.int(from = 1, to = length(x), 
                               by = length(x) / n_chunks) + 
                         (length(x) / n_chunks - 1), length(x))) 
  } else {
    # If workers is present, make max_chunk_size a multiple of it
    if (!missing(workers)) max_chunk_size <- 
        floor(max_chunk_size / workers) * workers
    starts <- seq.int(from = 1, to = length(x), by = max_chunk_size)
    ends <- pmin(seq.int(from = 1, to = length(x), by = max_chunk_size) + 
                   max_chunk_size - 1, length(x))
  }
  
  mapply(function(a, b) x[a:b], starts, ends, SIMPLIFY = FALSE)
  
}

# ------------------------------------------------------------------------------

#' Helper function to detect URL
#' @param x A string to check
#' @return A logical scalar.

is_url <- function(x) grepl("^(http|ftp)s?://", x)

# ------------------------------------------------------------------------------

#' Helper function to decide on future plan
#' @param fun A character string indicating the parent function
#' @param ... Additional named arguments
#' @return A logical scalar.

set_par <- function(fun, ...) {
  
  args <- list(...)
  par_check <- TRUE
  
  # Version for load_image
  if (fun == "load_image") par_check <- FALSE
  
  # Version for create_signature.character
  if (fun == "create_signature_character") par_check <- TRUE
  
  # Version for create_signature.matchr_image
  if (fun == "create_signature_matchr_image") par_check <- FALSE
  
  # Version for match_signatures
  if (fun == "match_signatures" && args$x < 2000) par_check <- FALSE
  
  # First check global option
  par_opt <- 
    getOption("matchr.force_parallel", 
              as.logical(Sys.getenv("MATCHR_FORCE_PARALLEL", FALSE)))
  
  # Send message if user forces parallel in non-advisable situation
  if (par_opt && 
      !par_check && 
      requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE) &&
      !"sequential" %in% class(future::plan())) {
    
    par_check <- TRUE
    
    if (exists("quiet", envir = parent.frame(n = 1), mode = "logical")) {
      quiet <- get("quiet", envir = parent.frame(n = 1))
      if (!quiet && !exists("par_1", envir = .matchr_env, mode = "logical")) {
        message(
        "Function executing in parallel because option ", 
        "`matchr.force_parallel` is TRUE, but this may lead to degraded ", 
        "performance. See `help(matchr_options)` for details. ",
        "This message is displayed once per session.")
        .matchr_env$par_1 <- TRUE
        }
      }
  }
  
  # Send message if user plan is overridden
  if (!par_check && 
      requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE) &&
      !"sequential" %in% class(future::plan())) {
    
    if (exists("quiet", envir = parent.frame(n = 1), mode = "logical")) {
      quiet <- get("quiet", envir = parent.frame(n = 1))
      if (!quiet && !exists("par_2", envir = .matchr_env, mode = "logical")) {
        message(
        "Function executing in non-parallel (sequential) mode to ", 
        "optimize performance. See `help(matchr_options)` for details. ",
        "This message is displayed once per session.")
        .matchr_env$par_2 <- TRUE
        }
      }
  }
  
  return(par_check)
  
}

# ------------------------------------------------------------------------------

get_iterator <- function(x) {
  
  iterator <- ceiling(log10(length(x)))
  iterator <- 10 ^ (ceiling(iterator / 2) - 1) * (1 + 4 * (iterator + 1) %% 2)
  return(iterator)
  
}

# ------------------------------------------------------------------------------

trim_signature <- function(x, range) {
  
  vec_assert(x, new_signature())
  field(x, "signature") <- lapply(field(x, "signature"), `[`, range)
  x
  
}

# ------------------------------------------------------------------------------

sig_length <- function(x) {
  
  vec_assert(x, new_signature())
  l <- unique(sapply(field(x, "signature"), length))
  if (length(l) == 2 && 1 %in% l) l <- setdiff(l, 1)
  stopifnot(length(l) == 1)
  l
}
