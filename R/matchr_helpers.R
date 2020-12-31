### PACKAGE HELPERS ############################################################

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


# Wrapper for mapply

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


#' Helper function to divide vector into equal-sized chunks
#' @param x The vector to be split
#' @param n_chunks The number of chunks to split into
#' @param max_chunk_size The maximum size of each chunk (default NULL)
#' @param workers A number to make max_chunk_size a multiple of (default NULL)
#' @return A list with n elements.

chunk <- function(x, n_chunks, max_chunk_size = NULL, workers = NULL) {
  
  if (missing(max_chunk_size) && !missing(workers)) {
    
    max_chunk_size <- ceiling(length(x) / n_chunks)
    
    max_chunk_size <- 
      unname(floor(max_chunk_size / number_of_threads()) * number_of_threads())
    
  }
  
  # Simple version which splits into n_chunks pieces
  if (missing(max_chunk_size)) {
    
    starts <- round(seq.int(from = 1, to = length(x), 
                            by = length(x) / n_chunks))
    
    ends <- round(pmin(seq.int(from = 1, to = length(x), 
                               by = length(x) / n_chunks) + 
                         (length(x) / n_chunks - 1), length(x))) 
    
  } else {
    
    # If workers is present, make max_chunk_size a multiple of it
    if (!missing(workers)) {
      
      max_chunk_size <- floor(max_chunk_size / workers) * workers
      
    }
    
    starts <- seq.int(from = 1, to = length(x), by = max_chunk_size)
    
    ends <- pmin(seq.int(from = 1, to = length(x), by = max_chunk_size) + 
                   max_chunk_size - 1, length(x))
    
  }
  
  mapply(function(a, b) x[a:b], starts, ends, SIMPLIFY = FALSE)
  
}


#' Helper function to detect URL
#' @param x A string to check
#' @return A logical scalar.

is_url <- function(x) grepl("^(http|ftp)s?://", x)


#' Helper function to decide on future plan
#' @param fun A character string indicating the parent function
#' @param ... Additional named arguments
#' @return A logical scalar.

set_par <- function(fun, ...) {
  
  args <- list(...)
  
  # Version for load_image
  if (fun == "load_image") {
    
    if (sum(is_url(args$file)) > 0) {
      par_check <- TRUE
    } else {
      size <- mean(file.size(sample(args$file, min(100, length(args$file)))))
      if (is.na(size)) size <- 1
      par_check <- size >= 100000
    }

  }
  
  # Version for create_signature.list
  if (fun == "create_signature_list") par_check <- FALSE
  
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
      
      if (!quiet) message(
        "Function executing in parallel because option ", 
        "`matchr.force_parallel` is TRUE, but this may lead to degraded ", 
        "performance. See `help(matchr_options)` for details.")
      }
  }
  
  # Send message if user plan is overridden
  if (!par_check && 
      requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE) &&
      !"sequential" %in% class(future::plan())) {
    
    if (exists("quiet", envir = parent.frame(n = 1), mode = "logical")) {
      
      quiet <- get("quiet", envir = parent.frame(n = 1))
      
      if (!quiet) message(
        "Function executing in non-parallel (sequential) mode to ", 
        "optimize performance. See `help(matchr_options)` for details.")
      }
  }
  
  return(par_check)
  
}