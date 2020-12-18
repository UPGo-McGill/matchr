#' Load image from file or URL
#'
#' \code{load_image} is a vectorized version of \code{imager::load.image}. It
#' optionally supports parallel processing (via \code{future} and
#' \code{future.apply}) and progress reporting.
#'
#' The function is a wrapper around \code{imager::load.image} with five
#' enhancements. It can take a vector of input paths instead a single path; it
#' supports parallel processing; it support progress reporting; it appends the
#' file path of the image as an attribute for subsequent processing or record
#' keeping; and it suppresses file download messages to allow more meaningful
#' progress reporting.
#'
#' Because the memory requirements of storing image representations in memory
#' so large, it is usually not feasible to read in more than several hundred
#' images at a time with \code{load_image}. For these cases, use
#' \code{\link{create_signature}} directly on the input file paths. By default
#' this will read images with \code{load_image} 100 at a time before
#' generating the unique colour signatures used for image matching.
#'
#' @param file A vector of file paths or URLs to be passed to
#' \code{imager::load.image}. If `file` is a vector of URLs, the URLs must begin
#' with "http", "https", "ftp" or "ftps". If the URL has no extension, it will
#' be given the extension ".jpg".
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


  ### Import and process images ################################################

  ## Store paths for later -----------------------------------------------------

  paths <- file


  ## Import images with proper progress handling -------------------------------

  handler_matchr("Loading image")

  pb <- progressr::progressor(steps = length(file), enable = !quiet)

  imgs <- par_lapply(file, function(x) {

    pb()

    load_image_internal(x)

    }, future.seed = NULL)


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

  is_url <- grepl("^(http|ftp)s?://", x)

  if (is_url) {

    ext <- regmatches(x, regexpr("\\.([A-Za-z0-9]+$)", x))

    if (length(ext) > 0) dst <- tempfile(fileext = ext) else {
      dst <- tempfile(fileext = ".jpg")
    }

    downloader::download(x, dst, mode = "wb", quiet = TRUE)

    # Import image from temp file
    img <-
      tryCatch(imager::load.image(dst), error = function(e) {
        warning("Input '", x, "' is invalid; output is NA.", call. = FALSE)
        NA
      })

    unlink(dst)

    img

  } else {

    # Or import image from file path using imager's importer
    tryCatch(suppressMessages(imager::load.image(x)), error = function(e) {
      warning("Input '", x, "' is invalid; output is NA.", call. = FALSE)
      NA
    })

  }

}
