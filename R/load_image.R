#' Load image from file or URL
#'
#' \code{load_image} is a vectorized method for reading bitmaps (jpegs, pngs and
#' bmps) into memory as arrays of pixel values. It optionally supports parallel 
#' processing (via \code{future} and \code{future.apply}) and progress 
#' reporting.
#'
#' \code{load_image} only supports 3-channel (RGB) or 1-channel (greyscale)
#' images; 4-channel (RGBA) images will have their alpha channel silently 
#' dropped and 2-channel (greyscale with alpha) images will have *only* their
#' alpha channel preserved, while images with 5 or more channels will return as 
#' NA.
#'
#' Because the memory requirements of storing image representations in memory
#' so large, it is usually not feasible to read in more than several hundred
#' images at a time with \code{load_image}. For these cases, use
#' \code{\link{create_signature}} directly on the input file paths. By default
#' this will read images with \code{load_image} 100 at a time before
#' generating the unique colour signatures used for image matching.
#'
#' @param file A vector of file paths or URLs which identify bitmapped images in
#' the JPEG, PNG, TIFF, or BMP file formats. If `file` is length-one, it can be
#' the path to a directory containing images. Elements of `file` which are URLs 
#' must begin with "http", "https", "ftp" or "ftps".
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
  par_check <- set_par("load_image", l = length(file))
  
  # If `file` is a directory, convert it to a vector of file paths
  if (length(file) == 1 && dir.exists(file)) 
    file <- list.files(file, full.names = TRUE)
  
  
  ## Import images -------------------------------------------------------------
  
  handler_matchr("Loading image")
  prog_bar <- as.logical(
    as.numeric((length(file) >= 10)) * 
      as.numeric(!quiet) * 
      as.numeric(progressr::handlers(global = NA)))
  iterator <- get_iterator(file)
  pb <- progressr::progressor(steps = length(file), enable = prog_bar)
  
  imgs <- par_lapply(seq_along(file), \(i) {
    
    if (i %% iterator == 0) pb(amount = iterator)
    li_internal(file[i])
    
  })
  
  
  ## Construct class objects and return output ---------------------------------
  
  imgs <- new_image(imgs, file)
  return(imgs)
  
}


li_internal <- function(x) {
  
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
    stopifnot(length(dim(img)) == 2 || dim(img)[3] %in% 1:4)
    img
    }, error = function(e) {
      warning("Input '", x, "' is invalid; output is NA.", call. = FALSE)
      NA
      })
  
  # Drop transparency channel if present
  if (length(dim(img)) == 3 && dim(img)[3] == 4) img <- img[,,1:3]
  if (length(dim(img)) == 3 && dim(img)[3] == 2) img <- 1 - img[,,2]
    
  if (is_url(x)) unlink(d)
  
  return(img)
  
}
