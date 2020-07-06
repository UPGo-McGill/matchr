#' Load image from file or URL
#'
#' \code{load_image} is a vectorized version of \code{imager::load.image}. It
#' optionally supports parallel processing (via \code{future} and
#' \code{future.apply}) and progress reporting (via \code{progressr}).
#'
#' The function is a wrapper around \code{imager::load.image} with four
#' enhancements. It can take a vector of input paths instead a single path, it
#' supports parallel processing, it support progress reporting, and it appends
#' the file path of the image as an attribute for subsequent processing or
#' recordkeeping.
#'
#' @param file A vector of file paths or URLs to be passed to
#' \code{imager::load.image}.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return If the input is a single file path or URL, the output will be a
#' single object of class 'cimg'. If the input is a vector with length > 1,
#' the output will be a list of 'cimg' objects with the same length.
#' @export

load_image <- function(file, quiet = FALSE) {

  ### Error checking ###########################################################

  stopifnot(is.character(file), is.logical(quiet))


  ### Import and process images ################################################

  ## Import images with proper progress handling -------------------------------z

  handler_matchr("Loading image")

  with_progress({

   pb <- progressor(steps = length(file))

   imgs <- par_lapply(file, function(x) {
     pb()
     tryCatch(suppressMessages(imager::load.image(x)), error = function(e) {
       warning("Input '", x, "' is invalid; output is NA.", call. = FALSE)
       NA
       })
     })

   })


  ## Add file names ------------------------------------------------------------

  imgs <- mapply(function(x, y) structure(x, file = y), imgs, file,
                 SIMPLIFY = FALSE)


  ## Collapse list if length == 1 ----------------------------------------------

  if (length(imgs) == 1) imgs <- imgs[[1]]


  ## Return output -------------------------------------------------------------

  return(imgs)

}
