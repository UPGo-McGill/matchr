#' Load image from file or URL
#'
#' \code{load_image} is a vectorized method for reading bitmaps (jpegs, pngs and
#' bmps) into memory as arrays of pixel values. It optionally supports parallel 
#' processing (via \code{future} and \code{future.apply}) and progress 
#' reporting.
#'
#' Because the memory requirements of storing image representations in memory
#' so large, it is usually not feasible to read in more than several hundred
#' images at a time with \code{load_image}. For these cases, use
#' \code{\link{create_signature}} directly on the input file paths. By default
#' this will read images with \code{load_image} 100 at a time before
#' generating the unique colour signatures used for image matching.
#'
#' @param file A vector of file paths or URLs. If `file` is a vector of URLs, 
#' the URLs must begin with "http", "https", "ftp" or "ftps". If the URL has no 
#' extension, it will be given the extension ".jpg".
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A list of `matchr_img` objects of the same length as the input
#' vector.
#' @examples
#' load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png")
#' @export

load_image <- function(file, quiet = FALSE) {
  
  ### Error checking ###########################################################
  
  stopifnot(is.character(file), is.logical(quiet))
  
  
  ### Set parallelization options ##############################################

  par_check <- set_par("load_image")
  
  
  ### Import and process images ################################################
  
  ## Store paths for later -----------------------------------------------------
  
  paths <- file
  
  
  ## Import images with proper progress handling -------------------------------
  
  handler_matchr("Loading image")
  prog_bar <- as.logical((length(file) >= 10) * !quiet)
  iterator <- ceiling(log10(length(file)))
  iterator <- 10 ^ (ceiling(iterator / 2) - 1) * (1 + 4 * (iterator + 1) %% 2)
  pb <- progressr::progressor(steps = length(file), enable = prog_bar)
  
  imgs <- par_lapply(seq_along(file), function(x) {
    
    if (x %% iterator == 0) pb(amount = iterator)
    load_image_internal(file[x])
    
  })
  
  
  ## Construct class objects ---------------------------------------------------
  
  imgs <- mapply(new_matchr_img, imgs, paths, SIMPLIFY = FALSE)
  
  imgs[sapply(imgs, is.na)] <-
    lapply(imgs[sapply(imgs, is.na)], function(x) {
      class(x) <- "logical"
      x
    })
  
  
  ## Return output -------------------------------------------------------------
  
  return(imgs)
  
}

load_image_internal <- function(x) {
  
  # Download to tempfile if path is URL
  
  if (is_url(x)) {
    
    ext <- regmatches(x, regexpr("\\.([A-Za-z0-9]+$)", x))
    
    if (length(ext) > 0) dst <- tempfile(fileext = ext) else {
      dst <- tempfile(fileext = ".jpg")
    }
    
    downloader::download(x, dst, mode = "wb", quiet = TRUE)
    
    # Import image from temp file
    img <-
      tryCatch(readbitmap::read.bitmap(dst), error = function(e) {
        warning("Input '", x, "' is invalid; output is NA.", call. = FALSE)
        NA
      })
    
    unlink(dst)
    
  } else {
    
    # Or import image directly from file path
    img <- 
      tryCatch(suppressMessages(readbitmap::read.bitmap(x)), 
               error = function(e) {
                 warning("Input '", x, "' is invalid; output is NA.", 
                         call. = FALSE)
                 NA
                 })
    
  }
  
  if (!is.null(attr(img, "header"))) img <- img / 255
  
  return(img)
  
}
