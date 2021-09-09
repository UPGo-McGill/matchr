#' Download images to a local folder
#'
#' \code{download_images} takes a data frame or a vector of URLs and downloads 
#' the files to a local folder, possibly with the use of an additional ID vector 
#' to rename the files and check for duplicates.
#'
#' @param x A data frame with a column containing URLs to be downloaded and
#' IDs to be linked to the downloads for file naming and duplicate detection. 
#' If x is supplied, the `path` and `id` arguments will be interpreted as field 
#' names. If x is NULL, they will be interpreted as lists or character vectors.
#' @param destination A character scalar indicating a local folder where images
#' should be downloaded to.
#' @param path If x is supplied, the name of a list or character field in x with 
#' URLs to be downloaded. If x is NULL, a list or character vector of URLs. In 
#' either case, elements should begin with "http", "https", "ftp" or "ftps". If 
#' the URL has no extension, it will be given the extension ".jpg".
#' @param id If x is supplied, the name of a character field in x with IDs to
#' match to the file downloads. If x is NULL, a character vector the same length 
#' as `path`, indicating the IDs of the files to be downloaded. Downloaded files 
#' will be named according to the IDs, as `id_n.jpg`, where `id` is an element 
#' of the `id` argument, `n` is an integer sequence of the same length as the
#' `id` element, and `.jpg` is the same file extension as the URL. (So if there 
#' are three URLs which correspond to the `id` value of "XXXX", the files 
#' downloaded will be "XXXX-1.jpg", "XXXX-2.jpg" and "XXXX-3.jpg".) If `id` has
#' repeating values, the numbering will be cumulatively sequential to avoid
#' overwriting earlier files, with, e.g., the second instance of "XXXX" 
#' beginning with "XXXX-4.jpg".
#' @param check_duplicates A logical scalar (default TRUE) indicating whether to
#' skip downloads for files whose IDs match files already present in the
#' download folder. This check is done per ID rather than per file, which means 
#' that if any files with an ID of `x` are found in the download folder, no new 
#' files with an ID of `x` will be downloaded.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return Invisibly, a data frame summarizing the results of the downloads.
#' @examples
#' \dontrun{
#' # Setup
#' df <- data.frame(id = 1:15, photos = test_urls)
#' dest <- tempdir()
#' download_images(df, dest)
#' }
#' @export

download_images <- function(x = NULL, destination, path = photos, id = id, 
                            check_duplicates = TRUE, quiet = FALSE) {
  
  # Error checking and preparation
  stopifnot(is.null(x) || is.data.frame(x))
  if (!is.null(x)) df <- TRUE else df <- FALSE
  
  # Get paths and IDs
  path <- if (df) eval(parse(text = paste0("x$", substitute(path)))) else path
  id <- if (df) eval(parse(text = paste0("x$", substitute(id)))) else id
  files <- list.files(destination)
  file_id <- unique(sub("-\\d*.jpg", "", files))
  
  # Check for duplicates
  duplicate_id <- integer()
  if (check_duplicates) duplicate_id <- which(id %in% file_id)
  
  # Prepare progress tracking
  handler_matchr("Downloading file")
  prog_bar <- as.logical(
    as.numeric((length(path) >= 10)) *
      as.numeric(!quiet) *
      as.numeric(progressr::handlers(global = NA)))
  iterator <- get_iterator(path)
  pb <- progressr::progressor(steps = length(path), enable = prog_bar)

  # Download files
  result <- vector("list", length(path))
  for (i in seq_along(path)) {
    if (i %% iterator == 0) pb(amount = iterator)
    if (any(i %in% duplicate_id)) {
      result[[i]] <- "duplicate"
      next
      }
    result[[i]] <- 
      suppressWarnings(tryCatch(utils::download.file(path[[i]], file.path(
        destination, paste0(id[[i]], "-", seq_along(path[[i]]), ".jpg")),
        quiet = TRUE), error = function(e) "error"))
  }
  
  # Assemble output
  result <- sapply(result, as.character)
  result[result == "0"] <- "success"
  n_success <- sum((result == "success") * pmax(1, lengths(path)))
  n_duplicate <- sum((result == "duplicate") * pmax(1, lengths(path)))
  n_error <- sum((result == "error") * pmax(1, lengths(path)))
  result <- data.frame(id = id, path = I(path), result = result)
  class(result$path) <- "list"
  if (requireNamespace("dplyr", quietly = TRUE)) result <- 
    dplyr::as_tibble(result)
  
  # Return output
  if (!quiet) cat(n_success, "files successfully downloaded in TKTK time.\n")
  if (check_duplicates && !quiet) cat(n_duplicate, "duplicates detected.\n")
  if (!quiet) cat(n_error, "files failed to download.\n")
  
  invisible(result)
}