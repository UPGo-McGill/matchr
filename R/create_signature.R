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
#' @param bands Integer scalar. The number of horizontal and vertical bands the
#' image should be split into for processing. Higher values will produce a more
#' distinctive colour signature, potentially decreasing the rate of matching
#' false positives, but at the cost of increased processing time and an
#' increased rate of matching false negatives.
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

create_signature <- function(image, bands = 20, rm_black_bars = TRUE, ...) {
  
  UseMethod("create_signature")
  
}

# ------------------------------------------------------------------------------

#' @rdname create_signature
#' @method create_signature matchr_image
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @export

create_signature.matchr_image <- function(image, bands = 20, 
                                        rm_black_bars = TRUE, quiet = FALSE,
                                        ...) {
  
  # Error checking and variable initialization
  stopifnot(is.numeric(bands), is.logical(rm_black_bars))
  par_check <- set_par("create_signature_matchr_image")
  
  # Initialize progress reporting
  handler_matchr("Creating signature")
  prog_bar <- as.logical((length(image) >= 10) * as.numeric(!quiet) * 
                           progressr::handlers(global = NA))
  iterator <- get_iterator(image)
  pb <- progressr::progressor(steps = length(image), enable = prog_bar)

  # Get signatures
  sigs <- par_lapply(seq_along(image), function(x) {
    if (x %% iterator == 0) pb(amount = iterator)
    create_signature_internal(image[x], bands = bands,
                              rm_black_bars = rm_black_bars)
  })
  
  # Return output
  path <- get_path(image)
  aspect_ratio <- 
    sapply(sigs, function(x) ifelse(length(x) == 2, x[[2]][2] / x[[2]][1], 
                                    NA_real_))
  result <- new_signature(lapply(sigs, `[[`, 1), path, aspect_ratio)
  return(result)
  
}

# ------------------------------------------------------------------------------

#' @rdname create_signature
#' @method create_signature character
#' @param backup A logical scalar. Should the function store an ongoing backup
#' of progress in a hidden object `.matchr_env$sig_backup` in the package's
#' environment (default)? If TRUE, the function will attempt to resume progress
#' if it detects a previous backup, and it will remove the backup if the
#' function successfully completes. Backups can be removed with
#' \code{\link{remove_backups}}.
#' @export

create_signature.character <- function(image, bands = 20, rm_black_bars = TRUE,
                                       backup = TRUE, quiet = FALSE, ...) {
  
  # Error checking and variable initialization
  stopifnot(is.numeric(bands), is.logical(rm_black_bars), 
            is.logical(backup), is.logical(quiet))
  par_check <- set_par("create_signature_character")
  if (length(image) <= 1000) backup <- FALSE
  iterations <- 1
  input_list <- list(seq_along(image))
  result <- vector("list", 1)
  resume_from <- 1
  
  # Prepare backup
  if (backup) {
    
    # Set iterations
    iterations <- ceiling(length(image) / 1000)
    input_list <- chunk(seq_along(image), iterations, 
                        workers = number_of_threads())
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
                            sum(sapply(.matchr_env$sig_backup, length)) + 1,
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
                           progressr::handlers(global = NA))
  iterator <- get_iterator(image)
  pb <- progressr::progressor(steps = length(image), enable = prog_bar)
  pb(amount = sum(lengths(input_list[seq_len(resume_from - 1)])))
  
  # Loop
  for (i in resume_from:iterations) {
    result[[i]] <- par_lapply(input_list[[i]], function(x) {
      if (x %% iterator == 0) pb(amount = iterator)
      img <- list(load_image_internal(image[x]))
      img <- new_image(img, image[x])
      if (is.na(img)) return(list(NA, NA))
      sig <- create_signature_internal(img, bands = bands, 
                                       rm_black_bars = rm_black_bars)
      return(sig)
      })
    if (backup) assign("sig_backup", result, envir = .matchr_env)
  }
  
  # Construct matchr_signature object and return result
  result <- unlist(result, recursive = FALSE)
  aspect_ratio <- 
    sapply(result, function(x) ifelse(length(x) == 2, x[[2]][2] / x[[2]][1], 
                                      NA_real_))
  
  result <- new_signature(lapply(result, `[[`, 1), image, aspect_ratio)
  stopifnot(sig_length(result) == bands * 8 || sig_length(result) == 1)
  if (backup) rm(sig_backup, sig_hash, envir = .matchr_env)
  return(result)
  
}

# ------------------------------------------------------------------------------

create_signature_internal <- function(image, bands = 20, 
                                          rm_black_bars = TRUE, ...) {
  
  # Error checking and variable initialization
  stopifnot(is.numeric(bands), is.logical(rm_black_bars))
  a <- get_array(image)[[1]]
  
  # Return NA if input is NA or doesn't have enough pixels
  if (is.na(image)) return(NA)
  if (dim(a)[[1]] < bands || dim(a)[[2]] < bands) return(NA)
  
  # Failsafe in case image is greyscale
  if (length(dim(a)) == 2 || dim(a)[[3]] == 1) {
    a <- sapply(list(a, a, a), I, simplify = "array")
  }
  
  # Check for black bars
  if (rm_black_bars) a <- remove_black_bars(a)
  if (is.logical(a)) return(NA)
  
  # Calculate row means
  rm_1 <- rowMeans(a[,,1])
  rm_2 <- rowMeans(a[,,2])
  rm_3 <- rowMeans(a[,,3])
  rm_1 <- sapply(chunk(rm_1, bands), mean)
  rm_2 <- sapply(chunk(rm_2, bands), mean)
  rm_3 <- sapply(chunk(rm_3, bands), mean)
  rm_total <- (rm_1 + rm_2 + rm_3) / 3
  
  # Calculate column means
  cm_1 <- colMeans(a[,,1])
  cm_2 <- colMeans(a[,,2])
  cm_3 <- colMeans(a[,,3])
  cm_1 <- sapply(chunk(cm_1, bands), mean)
  cm_2 <- sapply(chunk(cm_2, bands), mean)
  cm_3 <- sapply(chunk(cm_3, bands), mean)
  cm_total <- (cm_1 + cm_2 + cm_3) / 3
  
  # Return result
  result <- c(rm_total, cm_total, rm_1, rm_2, rm_3, cm_1, cm_2, cm_3)
  return(list(result, dim(a)))
  
}
