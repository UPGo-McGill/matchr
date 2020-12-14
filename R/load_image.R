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
#' @param file A vector of file paths or URLs to be passed to
#' \code{imager::load.image}. If `file` is a vector of URLs, the URLs must begin
#' with "http", "https", "ftp" or "ftps".
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A list of `cimg` objects with the same length as the input vector.
#' @export

load_image <- function(file, quiet = FALSE) {

  ### Error checking and progress setup ########################################

  stopifnot(is.character(file), is.logical(quiet))

  # if (requireNamespace("progressr", quietly = TRUE)) {
  #
  #   progressr::handlers(global = TRUE)
  #
  # }


  ### Import and process images ################################################

  ## Store paths for later -----------------------------------------------------

  paths <- file


  ## Import images with proper progress handling -------------------------------

  handler_matchr("Loading image")

  pb <- progressor(steps = length(file), enable = !quiet)

  imgs <- par_lapply(file, function(x) {

    pb()

    ## Download to tempfile if path is URL -------------------------------------

    is_url <- grepl("^(http|ftp)s?://", x)

    if (is_url) {

      ext <- stringr::str_extract_all(x, "\\.([A-Za-z0-9]+$)")[[1]]

      if (length(ext) > 0) dst <- tempfile(fileext = ext) else dst <- tempfile()

      downloader::download(x, dst, mode = "wb", quiet = TRUE)

      img <-
        tryCatch(imager::load.image(dst), error = function(e) {
          warning("Input '", x, "' is invalid; output is NA.", call. = FALSE)
          NA
          })

      unlink(dst)

      img

    } else {

      tryCatch(suppressMessages(imager::load.image(x)), error = function(e) {
        warning("Input '", x, "' is invalid; output is NA.", call. = FALSE)
        NA
      })

    }

    }, future.seed = NULL)


  ## Construct class objects ---------------------------------------------------

  imgs <- mapply(new_matchr_img, imgs, paths, SIMPLIFY = FALSE)


  ## Return output -------------------------------------------------------------

  return(imgs)

}
