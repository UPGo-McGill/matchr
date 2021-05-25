#' Compare image matches in a Shiny app
#'
#' \code{compare_images} TKTK
#'
#' TKTK
#'
#' @param result A data frame produced by \code{\link{identify_matches}} (with
#' confirm = TRUE or with \code{\link{confirm_matches}} subsequently run on the
#' results).
#' @param remove_duplicates TKTK
#' @param batch_size An integer scalar. The number of images to display at a 
#' time in the Shiny app (default 100).
#' @param show_names TKTK
#' @param corr_thresh TKTK
#' @param previous TKTK
#' @return If the Shiny package is present, a data frame with the same fields as
#' the input `result`, except that the field `match` will be replaced with
#' `new_match_status`, which is a character vector with possible entries
#' "match" and "no match". The output will have one row for each image pairing
#' that was confirmed, which is determined by how many pages into the Shiny app
#' the user proceeded, and thus how many pairings were viewed. If all pages are
#' viewed, then the output will have the same number of rows as the input.
#' @export

compare_images <- function(result, remove_duplicates = TRUE, 
                           batch_size = 100L, show_names = FALSE, 
                           corr_thresh = 0.9995, previous = TRUE) {
  
  # Check if necessary packages are installed
  if (!requireNamespace("shiny", quietly = TRUE)) stop(
    "For interactive image comparison, install the \"shiny\" package.",
    call. = FALSE)
  
  if (!requireNamespace("shinyjs", quietly = TRUE)) stop(
      "For interactive image comparison, install the \"shinyjs\" package.",
      call. = FALSE)
    
    if (!requireNamespace("waiter", quietly = TRUE)) stop(
      "For interactive image comparison, install the \"waiter\" package.",
      call. = FALSE)
  
  # Error checking and object initialization
  stopifnot(is.data.frame(result), is.numeric(c(batch_size, corr_thresh)),
            is.logical(c(remove_duplicates, show_names, previous)))
  if (!is.integer(batch_size)) batch_size <- floor(batch_size)
  
  # Get list of files, paths and folders
  x_names <- field(result$x_sig, "file")
  x_dirs <- x_names[!is_url(x_names)]
  x_dirs <- sub("[^/]+$", "", x_dirs)
  x_dirs <- sort(unique(x_dirs))
  x_dirs <- sub("/$", "", x_dirs)
  x_paths <- paste("x", seq_along(x_dirs), sep = "_")

  y_names <- field(result$y_sig, "file")
  y_dirs <- y_names[!is_url(y_names)]
  y_dirs <- sub("[^/]+$", "", y_dirs)
  y_dirs <- sort(unique(y_dirs))
  y_dirs <- sub("/$", "", y_dirs)
  y_paths <- paste("y", seq_along(y_dirs), sep = "_")

  # Launch Shiny app
  if (requireNamespace("shiny", quietly = TRUE)) {

    if (!requireNamespace("shinyjs", quietly = TRUE)) stop(
      "For interactive image comparison, install the \"shinyjs\" package.",
      call. = FALSE)

    if (!requireNamespace("waiter", quietly = TRUE)) stop(
      "For interactive image comparison, install the \"waiter\" package.",
      call. = FALSE)

    shiny::shinyOptions(result = result, x_paths = x_paths, y_paths = y_paths,
                        x_dirs = x_dirs, y_dirs = y_dirs,
                        remove_duplicates = remove_duplicates,
                        batch_size = batch_size, show_names = show_names,
                        corr_thresh = corr_thresh, previous = previous)
    output <- shiny::runApp(appDir = system.file("compare_images",
                                                 package = "matchr"))
    return(output)
  }
  
}
