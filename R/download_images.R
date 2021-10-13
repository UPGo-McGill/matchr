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
#' @param id If x is supplied, the name of a character field (or field which can
#' be coerced to a character field) in x with IDs to match to the file 
#' downloads. If x is NULL, a character vector (or vector which can be coerced
#' to a character vector) the same length as `path`, indicating the IDs of the 
#' files to be downloaded. Downloaded files will be named according to the IDs, 
#' as `id_n.jpg`, where `id` is an element of the `id` argument, `n` is an 
#' integer sequence of the same length as the `id` element, and `.jpg` is the 
#' same file extension as the URL. (So if there are three URLs which correspond 
#' to the `id` value of "XXXX", the files downloaded will be "XXXX-1.jpg", 
#' "XXXX-2.jpg" and "XXXX-3.jpg".) If `id` has repeating values, the 
#' corresponding `path` entries will be consolidated and duplicates will be 
#' removed.
#' @param check_duplicates A logical scalar (default TRUE) indicating whether to
#' skip downloads for files whose IDs match files already present in the
#' download folder. This check is done per ID rather than per file, which means 
#' that if any files with an ID of `x` are found in the download folder, no new 
#' files with an ID of `x` will be downloaded. If this is set to FALSE, existing
#' files may be overwritten with no warning.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return Invisibly, a data frame summarizing the results of the downloads.
#' @examples
#' \dontrun{
#' # Setup
#' df <- data.frame(id = 1:15, photos = example_urls)
#' dest <- tempdir()
#' download_images(df, dest)
#' }
#' @export

download_images <- function(x = NULL, destination, path = photos, id = id, 
                            check_duplicates = TRUE, quiet = FALSE) {
  
  # Error checking and preparation
  stopifnot(is.null(x) || is.data.frame(x))
  if (!is.null(x)) df <- TRUE else df <- FALSE
  start_time <- Sys.time()
  
  # Get paths and IDs
  path <- if (df) eval(parse(text = paste0("x$", substitute(path)))) else path
  id <- if (df) eval(parse(text = paste0("x$", substitute(id)))) else id
  id <- as.character(id)
  path_full <- path
  id_full <- id
  
  # Remove empty path elements
  id <- id[lengths(path) > 0]
  path <- path[lengths(path) > 0]
  id_not_empty <- id
  
  # Consolidate duplicated IDs
  duplicate_id <- which(duplicated(id))
  if (length(duplicate_id) > 0) {
    to_merge <- lapply(duplicate_id, \(x) which(id == id[x]))
    first_merge <- sapply(to_merge, min)
    path[first_merge] <- lapply(to_merge, \(x) unique(unlist(path[x])))
    path <- path[-duplicate_id]
    id <- id[-duplicate_id]
  }
  id_unique_id <- id
  
  # Check for existing files at destination
  destination <- sub("/$", "", destination)
  files <- list.files(destination)
  file_id <- unique(sub("-\\d*.jpg", "", files))
  
  # Remove duplicates
  if (check_duplicates) {
    dups <- which(id %in% file_id)
    n_duplicate <- length(unlist(path[dups]))
    if (length(dups) > 0) id <- id[-dups]
    if (length(dups) > 0) path <- path[-dups]
  }
  
  # Construct new destination paths
  dest_path <- mapply(\(x, y) {
    ext_match <- regexpr("\\.([A-Za-z0-9]+$)", x)
    ext <- vector("character", length(ext_match))
    ext[ext_match == -1] <- ".jpg"
    ext[ext_match != -1] <- regmatches(x, ext_match)
    paste0(y, "-", seq_along(x), ext)
  }, path, id)
  
  # Unlist paths and prepare to iterate
  orig_path <- unlist(path)
  dest_path <- unlist(dest_path)
  stopifnot(length(orig_path) == length(dest_path)) # Sanity check
  iterations <- ceiling(length(orig_path) / 100)
  out <- vector("integer", iterations)
  errors <- vector("list", iterations)
  
  # Prepare progress tracking
  handler_matchr("Downloading file")
  prog_bar <- as.logical(
    as.numeric((length(dest_path) >= 10)) * as.numeric(!quiet) *
      as.numeric(progressr::handlers(global = NA)) * check_env())
  pb <- progressr::progressor(steps = length(dest_path), enable = prog_bar)
  
  # Download images in for loop
  for (i in seq_len(iterations)) {
    ind <- ((i - 1) * 100 + 1):min(length(dest_path), i * 100)
    out[[i]] <- suppressWarnings(tryCatch(utils::download.file(
      orig_path[ind], paste0(destination, "/", dest_path[ind]), 
      method = "libcurl", quiet = TRUE),
      error = function(e) 1L))
    new_files <- list.files(destination)  
    errors[[i]] <- setdiff(dest_path[ind], new_files)
    pb(amount = length(ind))
  }
  
  # Assemble output
  result <- rep("success", length(id_full))
  result[lengths(path_full) == 0] <- "empty"
  error_id <- unlist(errors)
  error_id <- sub("-\\d*.jpg", "", error_id)
  error_id <- unique(error_id)
  result[id_full %in% error_id] <- "error"
  if (length(duplicate_id) > 0) result[unique(sapply(
    id_full[id_full %in% id_not_empty[duplicate_id]], \(x) 
    which(id_full %in% x)[-1]))] <- "duplicated ID"
  if (check_duplicates) result[id_full %in% id_unique_id[dups]] <- 
    "duplicated file"
  n_error <- length(unlist(errors))
  n_success <- length(orig_path) - n_error
  result <- data.frame(id = id_full, path = I(path_full), result = result)
  class(result$path) <- "list"
  if (requireNamespace("dplyr", quietly = TRUE)) result <- 
    dplyr::as_tibble(result)
  
  # Return output
  time_dif <- Sys.time() - start_time
  time_dif <- paste0(round(as.numeric(time_dif), 2), " ", units(time_dif), ".")
  if (!quiet) cat(n_success, "files successfully downloaded in", time_dif, "\n")
  if (check_duplicates && !quiet) cat(n_duplicate, "duplicates detected.\n")
  if (!quiet) cat(n_error, "files failed to download.\n")
  invisible(result)
  
}