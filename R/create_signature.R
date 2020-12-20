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
#' @param image Object (or list of objects) of class `matchr_img` or `cimg`,
#' probably imported using \code{\link{load_image}} or
#' \code{imager::load.image}, or file path or URL (or vector/list of file paths
#' or URLs) which can be imported to `matchr_img` using
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
#' @return If the input is a single `matchr_img` or `cimg` image, an object of
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

create_signature.matchr_img <- function(image, bands = 20, rm_black_bars = TRUE,
                                        ...) {

  ### Error checking ###########################################################

  stopifnot(is.numeric(bands), is.logical(rm_black_bars))


  ### Return NA if the image doesn't have enough rows or columns ###############

  if (dim(image)[[1]] < bands || dim(image)[[2]] < bands) {

    return(
      new_matchr_sig(
        rep(NA_real_, times = bands * 8),
        if (is.null(attr(image, "file"))) NA_character_ else attr(image,
                                                                  "file"),
        NA_real_)
    )

  }


  ### Split the image then check for black bars ################################

  row_split <- imager::imsplit(image, "y", bands)
  row_means <- lapply(row_split, rowMeans)
  row_means <- sapply(row_means, mean)

  # Check for black bars
  if (rm_black_bars) {

    # First check for all black image and return NA if so
    if (sum(row_means) == 0) return(create_signature.default(image))

    if (sum(row_means == 0) > 0) {

      black_strips <- which(row_means == 0)
      suppressWarnings({
        top_bound <- max(black_strips[black_strips <= bands / 2]) + 1
      })
      if (is.infinite(top_bound)) top_bound <- 1
      suppressWarnings({
        bottom_bound <- min(black_strips[black_strips > bands / 2]) - 1
      })
      if (is.infinite(bottom_bound)) bottom_bound <- bands

      image <- new_matchr_img(
        x = imager::imappend(row_split[top_bound:bottom_bound], "y"),
        file = attr(image, "file")
      )
    }
  }


  ### Finish greyscale process #################################################

  col_split <- imager::imsplit(image, "x", bands)
  col_means <- lapply(col_split, colMeans)
  col_means <- sapply(col_means, mean)


  ### Do colour split ##########################################################

  # Fail safe in case image is greyscale
  if (dim(image)[[4]] == 1) {

    c_row_means <- rep(row_means, 3)
    c_col_means <- rep(col_means, 3)

    } else {

      colour_split <- imager::channels(image, 1:3)

      c_row_split <- lapply(colour_split, imager::imsplit, "y", bands)
      c_row_split <- unlist(c_row_split, recursive = FALSE)
      c_row_means <- lapply(c_row_split, rowMeans)
      c_row_means <- sapply(c_row_means, mean)

      c_col_split <- lapply(colour_split, imager::imsplit, "x", bands)
      c_col_split <- unlist(c_col_split, recursive = FALSE)
      c_col_means <- lapply(c_col_split, colMeans)
      c_col_means <- sapply(c_col_means, mean)

      }

  result <- c(row_means, col_means, c_row_means, c_col_means)
  result <- unname(result)


  ### Construct matchr_sig object and return result ############################

  result <- new_matchr_sig(
    result,
    if (is.null(attr(image, "file"))) NA_character_ else attr(image, "file"),
    imager::width(image) / imager::height(image))

  return(result)

}


#' @rdname create_signature
#' @method create_signature cimg
#' @export

create_signature.cimg <- function(image, bands = 20, rm_black_bars = TRUE,
                                  ...) {

  file <- if (is.null(attr(image, "file"))) NA_character_ else attr(image,
                                                                    "file")

  image <- new_matchr_img(image, file)

  create_signature(image)

}


#' @rdname create_signature
#' @method create_signature character
#' @param batch_size An integer scalar. How many images should the function
#' load into memory before extracting image signatures and releasing the
#' associated memory? Higher values will lead to the function executing more
#' quickly, but can result in enormous memory requirements.
#' @param backup A logical scalar. Should the function store an ongoing backup
#' of progress in a hidden object `.matchr_env$sig_backup` in the package's
#' environment (default)? If TRUE, the function will attempt to resume progress
#' if it detects a previous backup, and it will remove the backup if the
#' function successfully completes. Backups can be removed with
#' \code{\link{remove_backups}}.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @export

create_signature.character <- function(image, bands = 20, rm_black_bars = TRUE,
                                       batch_size = 100, backup = TRUE,
                                       quiet = FALSE, ...) {

  ### Error checking ###########################################################
  
  stopifnot(is.numeric(bands), is.numeric(batch_size),
            is.logical(rm_black_bars), is.logical(quiet))
  
  ### Prepare iteration ########################################################
  
  # If batch_size >= length(image), just do one iteration
  if (batch_size >= length(image)) {
    
    adj_batch_size <- length(image)
    
  } else {
    
    # Find largest multiple of number_of_threads <= batch_size
    adj_batch_size <- 
      floor(batch_size / number_of_threads()) * number_of_threads()
    
  }
  
  # Set iterations
  iterations <- ceiling(length(image) / adj_batch_size)
  
  # Don't have more iterations than input elements
  iterations <- min(iterations, length(image))
  
  result <- vector("list", iterations)
  resume_from <- 1
  
  
  ### Prepare backup ###########################################################
  
  if (backup) {
    
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
  
  handler_matchr("Creating signature")
  
  pb <- progressr::progressor(steps = length(image), enable = !quiet)
  
  pb(amount = (resume_from - 1) * adj_batch_size)
  
  for (i in resume_from:iterations) {
    
    # Build input list
    vec_start <- (i - 1) * adj_batch_size + 1
    vec_end <- min(i * adj_batch_size, length(image))
    vec_iter <- ceiling((vec_end - vec_start + 1) / number_of_threads())
    last_thread_plus_1 <- 
      ifelse(vec_end %% number_of_threads() == 0, number_of_threads(),
             vec_end %% number_of_threads())
    
    input_list <- lapply(1:number_of_threads(), function(n) {
      
      loop_start <-
        vec_start + (n - 1) * vec_iter - max(n - 1 - last_thread_plus_1, 0)
      
      loop_end <- 
        vec_start + n * vec_iter - 1 - (max(n - last_thread_plus_1, 0))
      
      image[loop_start:loop_end]
      
    })
    
    # Trim input_list if it goes out of bounds
    
    result[[i]] <- par_lapply(input_list, function(x) {
      
      imgs <- lapply(x, function(y) {
        pb(amount = 0.5)
        matchr:::load_image_internal(y)})
      
      imgs <- mapply(new_matchr_img, imgs, x, SIMPLIFY = FALSE)
      
      imgs[sapply(imgs, is.na)] <-
        lapply(imgs[sapply(imgs, is.na)], function(y) {
          class(y) <- "logical"
          y
        })
      
      imgs <- lapply(imgs, function(y) {
        pb(amount = 0.5)
        create_signature(y, bands = bands, rm_black_bars = rm_black_bars)
      })
      
      imgs <- new_matchr_sig_list(imgs)
      
      return(imgs)
      
    }, future.seed = NULL)
    
    if (backup) assign("sig_backup", result, envir = .matchr_env)
    
  }
  
  
  ### Construct matchr_sig_list object and return result #######################
  
  # Need to do this twice because of recursive lists
  result <- unlist(result, recursive = FALSE)
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
                                  batch_size = 100, quiet = FALSE, ...) {

  ### Error checking and preparation ###########################################

  stopifnot(is.numeric(bands), is.numeric(batch_size),
            is.logical(rm_black_bars), is.logical(quiet))

  stopifnot("All list elements must be the same class" =
              length(unique(sapply(image, typeof))) == 1)

  iterations <- ceiling(length(image) / batch_size)

  result <- vector("list", length(iterations))


  ### If list contains paths, delegate to create_signature.character ###########

  if (unique(sapply(image, typeof)) == "character") {

    image <- as.character(image)

    result <- create_signature(image)

    if (!is.list(result)) result <- list(result)

    return(result)

  }


  ### Run loop #################################################################

  handler_matchr("Creating signature")

  pb <- progressr::progressor(steps = length(image), enable = !quiet)

  for (i in seq_len(iterations)) {

    vec_start <- (i - 1) * batch_size + 1
    vec_end <- min(i * batch_size, length(image))

    result[[i]] <- par_lapply(image[vec_start:vec_end], function(x) {

      pb()

      create_signature(x, bands = bands, rm_black_bars = rm_black_bars)

    }, future.seed = NULL)

  }


  ### Construct matchr_sig_list object and return result #######################

  result <- unlist(result, recursive = FALSE)
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
