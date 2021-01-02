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
#' @param image Object (or list of objects) of class `matchr_img` (imported 
#' using \code{\link{load_image}}), or file path or URL (or vector/list of file 
#' paths or URLs) which can be imported to `matchr_img` using 
#' \code{\link{load_image}}.
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
#' @return If the input is a single `matchr_img` image, an object of
#' class `matchr_sig`, which contains a numeric vector of length `bands * 8`, a
#' file name (optionally), and an aspect ratio. If the input is a character
#' vector of file paths or a list of `matchr_img` images, a list of class
#' `matchr_sig_list`.
#' @examples
#' # Import image with load_image then create signature
#' img <- load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png")
#' create_signature(img)
#'
#' # Or create signature directly from path/URL
#' create_signature("https://upgo.lab.mcgill.ca/img/UPGo_logo.png")
#' @export

create_signature <- function(image, bands = 20, rm_black_bars = TRUE, ...) {
  
  UseMethod("create_signature")
  
}


#' @rdname create_signature
#' @method create_signature matchr_img
#' @export

create_signature.matchr_img <- function(image, bands = 20, 
                                          rm_black_bars = TRUE, ...) {
  
  ### Error checking ###########################################################
  
  stopifnot(is.numeric(bands), is.logical(rm_black_bars))
  
  
  ### Return NA if input is NA #################################################
  
  if (is.na(image)) return(create_signature.default(
    image, bands = bands, rm_black_bars = rm_black_bars))
  
  
  ### Return NA if the image doesn't have enough pixels ########################
  
  if (dim(image)[[1]] < bands || dim(image)[[2]] < bands) {
    
    if (is.null(attr(image, "file"))) {
      file <- NA_character_
    } else file <- attr(image, "file")
    
    return(new_matchr_sig(rep(NA_real_, times = bands * 8), file, NA_real_))
    
  }
  
  
  ### Fail safe in case image is greyscale #####################################
  
  if (length(dim(image)) == 2 || dim(image)[[3]] == 1) {
    
    image <- 
      new_matchr_img(sapply(list(image, image, image), I, simplify = "array"), 
                     attr(image, "file"))
    
  }

  
  ### Calculate row/column means ###############################################
  
  rm_1 <- rowMeans(image[,,1])
  rm_2 <- rowMeans(image[,,2])
  rm_3 <- rowMeans(image[,,3])
  
  # Check for black bars
  if (rm_black_bars) {
    
    rm_total <- (rm_1 + rm_2 + rm_3) / 3
    
    # First check for all black image and return NA if so
    if (sum(rm_total) == 0) return(create_signature.default(image))
    
    if (sum(rm_total == 0) > 0) {
      
      black_strips <- which(rm_total == 0)
      suppressWarnings({
        top_bound <- 
          max(black_strips[black_strips <= length(rm_total) / 2]) + 1
      })
      if (is.infinite(top_bound)) top_bound <- 1
      suppressWarnings({
        bottom_bound <- 
          min(black_strips[black_strips > length(rm_total) / 2]) - 1
      })
      if (is.infinite(bottom_bound)) bottom_bound <- length(rm_total)
      
      image <- new_matchr_img(image[top_bound:bottom_bound,,], 
                              file = attr(image, "file"))
      rm_1 <- rm_1[top_bound:bottom_bound]
      rm_2 <- rm_2[top_bound:bottom_bound]
      rm_3 <- rm_3[top_bound:bottom_bound]
      
    }
  }
  
  rm_1 <- sapply(chunk(rm_1, bands), mean)
  rm_2 <- sapply(chunk(rm_2, bands), mean)
  rm_3 <- sapply(chunk(rm_3, bands), mean)  

  rm_total <- (rm_1 + rm_2 + rm_3) / 3
  
  cm_1 <- sapply(chunk(rowMeans(image[,,1]), bands), mean)
  cm_2 <- sapply(chunk(rowMeans(image[,,2]), bands), mean)
  cm_3 <- sapply(chunk(rowMeans(image[,,3]), bands), mean)
  
  cm_total <- (cm_1 + cm_2 + cm_3) / 3
  
  result <- c(rm_total, cm_total, rm_1, rm_2, rm_3, cm_1, cm_2, cm_3)
  
  
  ### Construct matchr_sig object and return result ############################
  
  result <- new_matchr_sig(
    result,
    if (is.null(attr(image, "file"))) NA_character_ else attr(image, "file"),
    dim(image)[[2]] / dim(image)[[1]])
  
  return(result)
  
}


#' @rdname create_signature
#' @method create_signature character
#' @param backup A logical scalar. Should the function store an ongoing backup
#' of progress in a hidden object `.matchr_env$sig_backup` in the package's
#' environment (default)? If TRUE, the function will attempt to resume progress
#' if it detects a previous backup, and it will remove the backup if the
#' function successfully completes. Backups can be removed with
#' \code{\link{remove_backups}}.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @param diagnostic A logical scalar. Should the function report additional
#' information about parallel processing, iteration, etc?
#' @export

create_signature.character <- function(image, bands = 20, rm_black_bars = TRUE,
                                       backup = TRUE, quiet = FALSE, 
                                       diagnostic = FALSE, ...) {
  
  ### Error checking and variable initialization ###############################
  
  # Check arguments
  stopifnot(is.numeric(bands), is.logical(rm_black_bars), 
            is.logical(backup), is.logical(quiet))
  
  
  # Set parallelization options
  par_check <- set_par("create_signature_character")
  
  # Only do backup with > 1000 paths
  if (length(image) <= 1000) backup <- FALSE
  
  # Set iterations
  iterations <- 1
  input_list <- list(seq_along(image))
  result <- vector("list", 1)
  resume_from <- 1
  
  
  ### Prepare backup ###########################################################
  
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
      
      # Only proceed if old input size is identical to new input size
      if (.matchr_env$sig_backup_size == utils::object.size(image) &&
          length(.matchr_env$sig_backup) == iterations) {
        
        result <- .matchr_env$sig_backup
        resume_from <- length(result[!sapply(result, is.null)]) + 1
        if (!quiet) cat("Backup detected. Resuming from position ",
                        sum(sapply(.matchr_env$sig_backup, length)) + 1,
                        ".\n", sep = "")
        
      } else {
        stop("The backup detected in .matchr_env$sig_backup does not match ",
             "the input. Remove the previous backup with ",
             "`remove_backups()` to proceed.")
      }
      
    } else {
      
      assign("sig_backup_size", utils::object.size(image), envir = .matchr_env)
      
    }
  }
  
  
  ### Run loop #################################################################
  
  # Initialize progress reporting
  handler_matchr("Creating signature")
  prog_bar <- as.logical(
    as.numeric((length(image) >= 10)) * 
      as.numeric(!quiet) * 
      as.numeric(progressr::handlers(global = NA)))
  iterator <- ceiling(log10(length(image)))
  iterator <- 10 ^ (ceiling(iterator / 2) - 1) * (1 + 4 * (iterator + 1) %% 2)
  pb <- progressr::progressor(steps = length(image), enable = prog_bar)
  
  if (diagnostic) {
    message(crayon::silver(
      crayon::bold("create_signature diagnostic"), "\nProgress bar:", prog_bar,
      "\nParallel:", par_check, "\nWorkers:", number_of_threads(), 
      "\nIterator:", iterator, "\nIterations:", iterations, "\nLengths:", 
      paste(lengths(input_list), collapse = ", ")))
  }
  
  pb(amount = sum(lengths(input_list[seq_len(resume_from - 1)])))
  
  # Loop
  for (i in resume_from:iterations) {
    
    result[[i]] <- par_lapply(input_list[[i]], function(x) {
      
      if (x %% iterator == 0) pb(amount = iterator)
      
      img <- load_image_internal(image[x])
      img <- new_matchr_img(img, image[x])
      img <- create_signature(img, bands = bands, rm_black_bars = rm_black_bars, 
                              quiet = TRUE)
      
      return(img)
      
    })
    
    if (backup) assign("sig_backup", result, envir = .matchr_env)
    
  }
  
  
  ### Construct matchr_sig_list object and return result #######################
  
  result <- unlist(result, recursive = FALSE)
  
  if (length(result) > 1) {
    result <- new_matchr_sig_list(result)
  } else {
    result <- result[[1]]
  }
  
  # Remove backups if necessary
  if (backup) rm(sig_backup, sig_backup_size, envir = .matchr_env)
  return(result)
  
}


#' @rdname create_signature
#' @method create_signature list
#' @export

create_signature.list <- function(image, bands = 20, rm_black_bars = TRUE,
                                  quiet = FALSE, ...) {
  
  ### Error checking and preparation ###########################################
  
  stopifnot(is.numeric(bands), is.logical(rm_black_bars), is.logical(quiet), 
            "All list elements must be the same class" = 
              length(unique(sapply(image, typeof))) == 1)
  
  
  ### If list contains paths, delegate to create_signature.character ###########
  
  if (unique(sapply(image, typeof)) == "character") {
    
    image <- as.character(image)
    
    result <- create_signature(image)
    
    if (!is.list(result)) result <- list(result)
    
    return(result)
    
  }
  
  
  ### Run loop #################################################################
  
  # Parallel performance appears to be always slower than sequential
  par_check <- set_par("create_signature_list")
  
  # Initialize progress reporting
  handler_matchr("Creating signature")
  prog_bar <- as.logical(
    as.numeric((length(image) >= 10)) * 
      as.numeric(!quiet) * 
      as.numeric(progressr::handlers(global = NA)))
  iterator <- ceiling(log10(length(image)))
  iterator <- 10 ^ (ceiling(iterator / 2) - 1) * (1 + 4 * (iterator + 1) %% 2)
  
  pb <- progressr::progressor(steps = length(image), enable = prog_bar)
  
  result <- par_lapply(seq_along(image), function(x) {
      
      if (x %% iterator == 0) pb(amount = iterator)
      create_signature(image[[x]], bands = bands, rm_black_bars = rm_black_bars)
      
    })
    
  
  ### Construct matchr_sig_list object and return result #######################
  
  result <- new_matchr_sig_list(result)
  return(result)
  
}


#' @rdname create_signature
#' @method create_signature default
#' @export

create_signature.default <- function(image, bands = 20, rm_black_bars = TRUE,
                                     ...) {
  
  stopifnot(is.numeric(bands), is.logical(rm_black_bars))
  
  new_matchr_sig(
    rep(NA_real_, times = bands * 8),
    if (is.null(attr(image, "file"))) NA_character_ else attr(image, "file"),
    NA_real_
  )
}
