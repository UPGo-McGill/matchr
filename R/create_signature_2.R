#' Generate a unique colour signature for an image
#'
#' \code{create_signature} takes an image file and generates a numeric vector of
#' average greyscale and colour values, so that the image can be compared to
#' others.
#'
#' An image is decomposed into horizontal and vertical bands (with the number of
#' bands controlled by the `bands` argument), and for each band an average
#' greyscale and colour value is calculated. The vector of these averages
#' becomes a distinctive signature that can identify a given image even if the
#' image is rescaled or compressed, and thus serves as a reliable indicator of
#' whether two images are the same.
#'
#' @param image Vector of class `matchr_image` (imported using 
#' \code{\link{load_image}}), or character vector of file paths or URLs which 
#' can be imported to `matchr_image` using \code{\link{load_image}}.
#' @param rm_black_bars Logical scalar. Should horizontal black bars be
#' detected and removed from the image signature? Because these bands lead to an
#' image signature dominated by black values, leaving them in the signature can
#' lead to false positive matches.
#' @param ... Additional arguments passed to methods.
#' @return A vector of class `matchr_signature` of the same length as the input
#' vector.
#' @examples
#' \dontrun{
#' # Import image with load_image then create signature
#' img <- load_image(test_urls)
#' create_signature(img)
#'
#' # Or create signature directly from path/URL
#' create_signature(test_urls)
#' 
#' # By default top/bottom black bars are removed, but leave them with rm_black_bars = FALSE
#' create_signature(img, rm_black_bars = FALSE)
#' }
#' @export

create_signature_2 <- function(image, rm_black_bars = TRUE, ...) {
  
  UseMethod("create_signature_2")
  
}

# ------------------------------------------------------------------------------

#' @rdname create_signature_2
#' @method create_signature_2 matchr_image
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @export

create_signature_2.matchr_image <- function(image, rm_black_bars = TRUE, 
                                            quiet = FALSE, ...) {
  
  # Error checking and variable initialization
  stopifnot(is.logical(rm_black_bars))
  par_check <- set_par("create_signature_matchr_image")
  
  # Initialize progress reporting
  handler_matchr("Creating signature")
  prog_bar <- as.logical((length(image) >= 10) * as.numeric(!quiet) * 
                           progressr::handlers(global = NA))
  iterator <- get_iterator(image)
  pb <- progressr::progressor(steps = length(image), enable = prog_bar)
  
  # Get hashes
  arrays <- get_array(image)
  hash <- par_lapply(seq_along(arrays), function(x) {
    if (x %% iterator == 0) pb(amount = iterator)
    create_signature_2_internal(arrays[[x]], rm_black_bars = rm_black_bars)
  })
  
  # Return output
  path <- get_path(image)
  ar <- lapply(arrays, dim)
  ar <- sapply(ar, \(x) ifelse(is.null(x), NA_real_, x[2] / x[1]))
  result <- new_signature_2(hash, path, ar)
  return(result)
  
}

# ------------------------------------------------------------------------------

#' @rdname create_signature_2
#' @method create_signature_2 character
#' @param backup A logical scalar. Should the function store an ongoing backup
#' of progress in a hidden object `.matchr_env$sig_backup` in the package's
#' environment (default)? If TRUE, the function will attempt to resume progress
#' if it detects a previous backup, and it will remove the backup if the
#' function successfully completes. Backups can be removed with
#' \code{\link{remove_backups}}.
#' @export

create_signature_2.character <- function(image, rm_black_bars = TRUE,
                                         backup = TRUE, quiet = FALSE, ...) {
  
  # Error checking and variable initialization
  stopifnot(is.character(image), is.logical(c(rm_black_bars, backup, quiet)))
  par_check <- set_par("create_signature_character", l = length(image))
  if (length(image) <= 1000) backup <- FALSE
  iterations <- 1
  input_list <- list(seq_along(image))
  result <- vector("list", 1)
  resume_from <- 1
  
  # Prepare backup
  if (backup) {
    
    # Set iterations
    iterations <- ceiling(length(image) / 1000)
    input_list <- chunk(seq_along(image), iterations, workers = n_threads())
    iterations <- length(input_list)
    result <- vector("list", iterations)
    resume_from <- 1
    
    # Check for previous backup
    if (exists("sig_backup", envir = .matchr_env)) {
      
      # Only proceed if old hash is identical to new hash
      if (.matchr_env$sig_hash == digest::digest(image) &&
          length(.matchr_env$sig_backup) == iterations) {
        result <- .matchr_env$sig_backup
        resume_from <- length(result[!sapply(result, is.null)]) + 1
        if (!quiet) message("Backup detected. Resuming from position ",
                            sum(lengths(.matchr_env$sig_backup)) + 1,
                            ".\n", sep = "")
      } else {
        stop("The backup detected in .matchr_env$sig_backup does not match ",
             "the input. Remove the previous backup with ",
             "`remove_backups()` to proceed.", call. = FALSE)
      }
    } else {
      assign("sig_hash", digest::digest(image), envir = .matchr_env)
    }
  }
  
  # Initialize progress reporting
  handler_matchr("Creating signature")
  prog_bar <- as.logical((length(image) >= 10) * as.numeric(!quiet) * 
                           progressr::handlers(global = NA) * check_env())
  iterator <- get_iterator(image)
  pb <- progressr::progressor(steps = length(image), enable = prog_bar)
  pb(amount = sum(lengths(input_list[seq_len(resume_from - 1)])))
  
  # Loop
  for (i in resume_from:iterations) {
    # Load images and get hashes
    result[[i]] <- par_lapply(input_list[[i]], \(j) {
      if (j %% iterator == 0) pb(amount = iterator)
      array <- load_image_internal(image[j])
      hash <- create_signature_2_internal(array, rm_black_bars = rm_black_bars)
      dims <- dim(array)
      return(list(hash, dims))
      })
    # Update backup
    if (backup) assign("sig_backup", result, envir = .matchr_env)
  }
  
  # Construct matchr_signature_2 object and return output
  result <- unlist(result, recursive = FALSE)
  hash <- lapply(result, `[[`, 1)
  ar <- lapply(result, `[[`, 2)
  ar <- sapply(ar, \(x) ifelse(is.null(x), NA_real_, x[2] / x[1]))
  out <- new_signature_2(hash, image, ar)
  if (backup) rm(sig_backup, sig_hash, envir = .matchr_env)
  return(out)
  
}

# ------------------------------------------------------------------------------

create_signature_2_internal <- function(x, rm_black_bars = TRUE, ...) {
  
  # Return NA if input is NA, has wrong dims, or doesn't have enough pixels
  if (is.logical(x)) return(NA)
  if (!length(dim(x)) %in% c(2, 3)) return(NA)
  if (length(dim(x)) == 3 && !dim(x)[[3]] %in% c(1, 3)) return(NA)
  if (dim(x)[[1]] < 33 || dim(x)[[2]] < 33) return(NA)
  
  # Convert to greyscale
  if (length(dim(x)) == 3 && dim(x)[[3]] == 1) dim(x) <- dim(x)[1:2]
  if (length(dim(x)) == 3) x_grey <- rgb_to_grey(x) else x_grey <- x
  
  # Check for black bars
  if (rm_black_bars) x_grey <- remove_black_bars(x_grey)
  if (is.logical(x_grey)) return(NA)
  
  # Calculate hashes
  out_1 <- OpenImageR::phash(x_grey, MODE = "binary", resize = "nearest")
  out_2 <- OpenImageR::phash(x_grey, MODE = "binary", resize = "bilinear")
  
  # Return result
  return(c(as.vector(out_1), as.vector(out_2)))
  
}
