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
#' @return A `matchr_image` vector of the same length as the input vector.
#' @examples
#' \dontrun{
#' load_image(test_urls)
#' }
#' @export

load_image <- function(file, quiet = FALSE) {
  
  ## Error checking and options setting ----------------------------------------
  
  stopifnot(is.character(file), is.logical(quiet))
  par_check <- set_par("load_image")
  
  
  ## Import images -------------------------------------------------------------
  
  handler_matchr("Loading image")
  prog_bar <- as.logical(
    as.numeric((length(file) >= 10)) * 
      as.numeric(!quiet) * 
      as.numeric(progressr::handlers(global = NA)))
  iterator <- get_iterator(file)
  pb <- progressr::progressor(steps = length(file), enable = prog_bar)
  
  imgs <- par_lapply(seq_along(file), function(x) {
    
    if (x %% iterator == 0) pb(amount = iterator)
    load_image_internal(file[x])
    
  })
  
  
  ## Construct class objects and return output ---------------------------------
  
  imgs <- new_image(imgs, file)
  return(imgs)
  
}


load_image_internal <- function(x) {
  
  # Download to tempfile if path is URL
  if (is_url(x)) {
    ext <- regmatches(x, regexpr("\\.([A-Za-z0-9]+$)", x))
    if (length(ext) > 0) d <- tempfile(fileext = ext) else
      d <- tempfile(fileext = ".jpg")
    downloader::download(x, d, mode = "wb", quiet = TRUE)
  } else d <- x 
  
  # Import image
  img <- tryCatch({
    utils::capture.output(img <- readbitmap::read.bitmap(d), type = "message")
    img
    }, error = function(e) {
      warning("Input '", x, "' is invalid; output is NA.", call. = FALSE)
      NA
      })
    
  if (is_url(x)) unlink(d)
  
  return(img)
  
}
