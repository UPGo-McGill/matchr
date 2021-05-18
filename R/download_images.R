#' Download images to a local folder
#'
#' \code{download_images} takes a data frame or a vector of URLs and downloads 
#' the files to a local folder, possibly with the use of an additional ID vector 
#' to rename the files and check for duplicates.
#'
#' @param x A data frame with a column containing URLs to be downloaded and
#' (optionally) IDs to be linked to the downloads for file naming and duplicate
#' detection. If x is supplied, the `path` and `id` arguments will be 
#' interpreted as field names. If x is NULL, they will be interpreted as the
#' names of lists or character vectors.
#' @param destination A character scalar indicating a local folder where images
#' should be downloaded to.
#' @param path If x is supplied, the name of a list or character field in x with 
#' URLs to be downloaded. If x is NULL, a list or character vector of URLs. In 
#' either case, elements should begin with "http", "https", "ftp" or "ftps". If 
#' the URL has no extension, it will be given the extension ".jpg".
#' @param id If x is supplied, the name of a character field in x with IDs to
#' match to the file downloads. If x is NULL, a character vector the same length 
#' as `path`, indicating the IDs of the files to be downloaded. This parameter
#' can be NULL. If it is supplied, files will be named according to the IDs.
#' @param check_duplicates A logical scalar (default TRUE) indicating whether to
#' skip downloads for files whose IDs match files already present in the
#' download folder. This argument is ignored if `id` is NULL.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return Invisibly, a data frame summarizing the results of the downloads.
#' @export


download_images <- function(x = NULL, destination, path = photos, id = id, 
                            check_duplicates = TRUE, quiet = FALSE) {
  
  # Error checking and preparation
  stopifnot(is.null(x) || is.data.frame(x))
  if (!is.null(x)) df <- TRUE else df <- FALSE
  
  # Get paths and IDs
  path <- if (df) eval(parse(text = paste0("x$", substitute(path)))) else path
  if (is.null(id)) {
    id <- seq_along(path)
  } else id <- if (df) eval(parse(text = paste0("x$", substitute(id)))) else id
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
  pb <- progressr::progressor(steps = length(unlist(path)), enable = prog_bar)

  # Download files
  result <- vector("list", length(path))
  for (i in seq_along(path)) {
    pb(amount = length(path[[i]]))
    if (any(i %in% duplicate_id)) {
      result[[i]] <- "duplicate"
      next
      }
    result[[i]] <- tryCatch(utils::download.file(path[[i]], paste0(
      destination, "/", id[[i]], "-", seq_along(path[[i]]), ".jpg"),
      quiet = TRUE), error = function(e) "error")
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
  if (!quiet) cat(n_success, "files successfully downloaded.\n")
  if (check_duplicates && !quiet) cat(n_duplicate, "duplicates detected.\n")
  if (!quiet) cat(n_error, "files failed to download.\n")
  
  invisible(result)
}