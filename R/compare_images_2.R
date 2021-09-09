#' Compare image pairs in a Shiny app
#'
#' \code{compare_images_2} takes two sets of images and displays them in pairs
#' for comparison.
#' 
#' The interface presents pairs of images alongside a best guess as to the match
#' status ("Match" or "No match"). For matches which are correctly identified,
#' no further action is necessary, while incorrect identifications can be
#' corrected by clicking "Match" or "No match" next to the image pair. Images
#' are presented in batches, and at any point the user can click the "Save and
#' exit" button to close the comparison app and retrieve the results up through
#' the last batch which was viewed. This means that even extremely large sets of
#' potential matches can be manually verified over the course of several
#' sessions. Through the "Enable highlighting" button, specific matches can be 
#' highlighted for further follow-up after image comparison is finished.
#' 
#' 
#' The `match` field can contain any character strings, and image pairs will be
#' grouped by these values. (If there are more than 10 unique values for 
#' `match`, the function will issue a warning, since this probably implies that 
#' the `match` field was misidentified.) By default, all image pairs will start 
#' with a value of "match" in the comparison app. But if the values of the 
#' `match` field correspond to a name in the function's "match" argument, then 
#' the value will be "match" if the corresponding "match" argument value is 
#' TRUE, and "no match" if the corresponding "match" argument value is FALSE. 
#' For example, by default the "match" argument is: 
#' `c("Match" = TRUE, "Likely match" = TRUE, "Possible match" = FALSE`.
#' So any image pairs with a `match` field value of "Match" or "Likely match" 
#' will start with a value of "match" in the comparison app, while any image 
#' pairs with a `match` field value of "Possible match" will start with a value 
#' of "no match" in the comparison app.
#' 
#' @param x,y A `matchr_image`, `matchr_signature` or character vector which
#' contains references to a set of images. If a `matchr_image` vector, images 
#' will be drawn directly from the in-memory pixel information in the vector. 
#' If a `matchr_signature` vector, the images will be retrieved from the file
#' paths or URLs contained in each `matchr_signature` element. If a character 
#' vector, the vector elements will be assumed to be file paths or URLs, from 
#' which the images will be retrieved.
#' @param match An optional character vector. Image pairs will be grouped by 
#' these values. (If there are more than 10 unique values, the function will 
#' issue a warning, since this probably implies that the wrong vector was
#' passed to the argument.)
#' @param batch_size An integer scalar. The number of images to display at a 
#' time in the Shiny app (default 100).
#' @param match_defaults A named logical vector. Rows whose `match` argument 
#' value corresponds to a name in this vector will have a starting status of 
#' "match" in the comparison app if the named element's value is TRUE and "no 
#' match" if it is FALSE. If this argument is NULL, all image pairs will have a
#' starting status of "match".
#' @param info An optional data frame with information to be shown at the top of
#' the comparison app.
#' @param ... Additional named vectors which will be displayed per-image pair
#' when the "Show details" button is toggled in the comparison app. These
#' vectors must be the same length as the `x` and `y` inputs, and their contents
#' will be truncated to fit in the comparison app, so they should be relatively
#' terse. Names are only to identify `...` arguments; they will not be 
#' displayed alongside the vector contents. If multiple arguments to `...` are 
#' supplied, they will be concatenated with "; ".
#' @return A data frame with the following fields: `x` and `y` (or whatever 
#' these fields were originally named) from the original `df` data frame; 
#' `new_match_status`, which is a character vector with possible entries "match" 
#' and "no match"; and a logical vector `new_highlight` which is TRUE for any 
#' matches which were highlighted using the in-app interface. The output will 
#' only have non-missing `new_match_status` values for each image pairing that 
#' confirmed, which is determined by how many pages into the Shiny app the user 
#' proceeded, and thus how many pairings were viewed. Rows with pairings which
#' were not viewed will have an NA value in `new_match_status` and
#' `new_highlight`. If all pages are viewed, then the output will have no NA 
#' values.
#' @examples
#' \dontrun{
#' # Assign the output of compare_images to retrieve results
#' change_table <- compare_images(test_urls, test_urls)
#' }
#' @export

compare_images_2 <- function(x, y, match = NULL, batch_size = 100L, 
                             match_defaults = c("Match" = TRUE, 
                                                "Likely match" = TRUE,
                                                "Possible match" = FALSE,
                                                "No match" = FALSE),
                             info = NULL, ...) {
  
  # Check if necessary packages are installed
  if (!requireNamespace("shiny", quietly = TRUE) || 
      !requireNamespace("shinyjs", quietly = TRUE)) stop(
        "`compare_images` requires the \"shiny\" and \"shinyjs\" packages.",
        call. = FALSE)
  
  # Pre-process dots
  args <- list(...)
  
  # Error checking and object initialization
  stopifnot(
    "`x` and `y` must be matchr_image, matchr_signature or character vectors." =
      inherits(x, c("character", "matchr_image", "matchr_signature_2")) &&
      inherits(y, c("character", "matchr_image", "matchr_signature_2")))
  stopifnot(is.numeric(batch_size))
  batch_size <- floor(batch_size)
  if (!missing(match)) stopifnot(is.character(match))
  if (!is.null(match_defaults)) stopifnot(
    "`match_defaults` must be a named logical vector." = 
      is.logical(match_defaults) && !all(is.na(names(match_defaults))))
  stopifnot("`x` and `y` must be vectors of the same length." =
              length(x) == length(y))
  sapply(args, \(arg) stopifnot(
    "Arguments to `...` must be the same length as `x` and `y`." = 
      length(arg) == length(x)))
  if (!missing(info)) stopifnot(is.data.frame(info)) else info <- NULL
  
  # Exit early for zero-length input
  if (length(x) == 0) {
    output <- data.frame(x = x, y = y, new_match_status = character(),
                         new_highlight = logical())
    if (requireNamespace("dplyr", quietly = TRUE)) {
      output <- dplyr::as_tibble(output)
    }
    return(output)
  }
  
  # Exit early if not interactive
  if (!interactive()) {
    warning("The `compare_images` tool only runs in interactive mode.")
    output <- data.frame(x = x, y = y, new_match_status = character(),
                         new_highlight = logical())
    if (requireNamespace("dplyr", quietly = TRUE)) {
      output <- dplyr::as_tibble(output)
    }
    return(output)
  }
  
  # Get x_list and y_list
  x_list <- compare_get_list(x)
  y_list <- compare_get_list(y)
  
  # Update paths
  if (length(x_list$dir) > 0) x_list$value <- as.vector(mapply(
    sub, x_list$dir, x_list$path, MoreArgs = list(x = x_list$value), 
    USE.NAMES = FALSE))
  if (length(y_list$dir) > 0) y_list$value <- as.vector(mapply(
    sub, y_list$dir, y_list$path, MoreArgs = list(x = y_list$value), 
    USE.NAMES = FALSE))
  
  # Construct new_match_status vector
  if (missing(match)) match <- rep("All pairs", length(x))
  new_match_status <- rep(TRUE, length(x))
  match_false <- match %in% names(which(match_defaults == FALSE))
  new_match_status[match_false] <- FALSE
  UID <- paste0("id-", formatC(seq_along(x), 
                               width = floor(log10(length(x))) + 1, flag = "0"))
  new_match_status <- setNames(new_match_status, UID)
  
  # Need one table_n row per batch_size entries in each match status
  table_n <- table(match)
  table_n <- table_n[order(match(names(table_n), names(match_defaults)))]
  table_n <-
    data.frame(
      name = unlist(mapply(function(x, y) rep(x, ceiling(y / batch_size)), 
                           names(table_n), table_n, USE.NAMES = FALSE)),
      i_1 = unlist(sapply(table_n, function(x) seq_len(ceiling(
        x / batch_size)) * batch_size - (batch_size - 1), USE.NAMES = FALSE)),
      i_2 = unlist(sapply(table_n, function(x) 
        pmin(seq_len(ceiling(x / batch_size)) * batch_size, x), 
        USE.NAMES = FALSE)),
      row.names = NULL)
  names(table_n) <- c("name", "i_1", "i_2")
  
  # Process info_vectors
  args <- do.call(paste, c(args, sep = "; "))
  args <- ifelse(nchar(args) > 30, paste0(substr(args, 1, 29), "\u2026"), args)
  if (length(args) == 0) args <- rep("\u2026", length(x))
  info_vectors <- lapply(args, shiny::h5, align = "center")
  
  # Set Shiny options
  shiny::shinyOptions(x_list = x_list, y_list = y_list, match = match, 
                      table_n = table_n, new_match_status = new_match_status, 
                      match_defaults = match_defaults, batch_size = batch_size,
                      info = info, info_vectors = info_vectors)
  
  # Launch Shiny app then return results
  out <- shiny::runApp(system.file("compare_images_2", package = "matchr"))
  out <- data.frame(x = x, y = y, new_match_status = out[[1]], 
                    new_highlight = out[[2]], row.names = NULL)
  if (requireNamespace("dplyr", quietly = TRUE)) out <- dplyr::as_tibble(out)
  return(out)
  
}

# ------------------------------------------------------------------------------

compare_get_list <- function(x) {
  
  # Get argument name to construct path aliases
  arg_name <- deparse(substitute(x))
  
  # Start by getting names
  if (is.character(x)) x_names <- x else x_names <- get_path(x)
  x_names <- ifelse(
    nchar(x_names) > 30, 
    paste0("\u2026", substr(x_names, nchar(x_names) - 29, nchar(x_names))), 
    x_names)
  x_names <- lapply(x_names, shiny::h5, align = "center")
  
  # If the input is a matchr_image vector, note that and exit early
  if (inherits(x, "matchr_image")) {
    return(list(value = x, name = x_names, path = character(0), 
                dir = character(0), img = TRUE))
  }
  
  # Otherwise get a vector of paths and proceed
  if (inherits(x, "matchr_signature_2")) x <- get_path(x)
  dirs <- x[!is_url(x)]
  dirs <- sub("[^/]+$", "", dirs)
  dirs <- sort(unique(dirs))
  dirs <- sub("/$", "", dirs)
  paths <- paste(arg_name, seq_along(dirs), sep = "_")
  if (length(dirs) == 0) paths <- character()
  
  return(list(value = x, name = x_names, path = paths, dir = dirs, img = FALSE))
}
