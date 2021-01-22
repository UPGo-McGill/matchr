#' Compare image matches in the viewer window
#'
#' \code{compare_images} TKTK
#'
#' TKTK
#'
#' @param result TKTK
#' @param remove_duplicates TKTK
#' @param batch_size TKTK
#' @param show_names TKTK
#' @param corr_thresh TKTK
#' @param previous TKTK
#' @param quiet TKTK
#' @return TKTK
#' @export

compare_images <- function(result, remove_duplicates = TRUE, 
                           batch_size = 100, show_names = FALSE, 
                           corr_thresh = 0.9995, previous = TRUE,
                           quiet = FALSE) {
  
  # Error checking and object initialization
  stopifnot(is.data.frame(result), is.numeric(c(batch_size, corr_thresh)),
            is.logical(c(remove_duplicates, show_names, previous, quiet)))
  temp_dir <- tempfile()
  dir.create(temp_dir)
  x_dir <- paste0(temp_dir, "/x")
  dir.create(x_dir)
  y_dir <- paste0(temp_dir, "/y")
  dir.create(y_dir)
  
  # Launch Shiny app if Shiny is present
  if (requireNamespace("shiny", quietly = TRUE)) {

    if (!requireNamespace("shinyjs", quietly = TRUE)) stop(
      "For interactive image comparison, install the \"shinyjs\" package.",
      call. = FALSE)

    if (!requireNamespace("waiter", quietly = TRUE)) stop(
      "For interactive image comparison, install the \"waiter\" package.",
      call. = FALSE)

    shiny::shinyOptions(result = result, x_dir = x_dir, y_dir = y_dir,
                        remove_duplicates = remove_duplicates,
                        batch_size = batch_size, show_names = show_names,
                        corr_thresh = corr_thresh, previous = previous,
                        quiet = quiet)
    output <- shiny::runApp(appDir = system.file("compare_images",
                                                 package = "matchr"))
    return(output)
  }
  
  # Issue warning and prepare viewer
  warning("For interactive image comparison, install the \"shiny\" package. ",
          "A maximum of 1000 image pairs will be shown in the Viewer.",
          call. = FALSE)
  viewer <- getOption("viewer", default = utils::browseURL)
  
  # Copy images to temp folders
  result <- result[1:min(nrow(result), 1000),]
  result$x_name <- field(result$x_sig, "file")
  result$y_name <- field(result$y_sig, "file")
  file.copy(result$x_name, x_dir)
  file.copy(result$y_name, y_dir)
  
  # Make new path vectors
  x_paths <- strsplit(result$x_name, '/')
  x_paths <- sapply(x_paths, function(x) x[[length(x)]])
  x_paths <- paste0("x/", x_paths)
  y_paths <- strsplit(result$y_name, '/')
  y_paths <- sapply(y_paths, function(x) x[[length(x)]])
  y_paths <- paste0("y/", y_paths)
  
  # Create header and footer strings
  code_head <- '
  <!DOCTYPE html>
  <html>
  <head>
  <title>Image comparison</title>
  <style>
  table.centre {
  margin-left: auto;
  margin-right: auto;
  }
  </style>
  </head>
  <body>
  <table class="centre">
  '
  
  code_foot <- '
  </table>
  </body>
  </html>
  '
  
  # Create image elements
  image_elements <- mapply(function(x, y, z, n) {
    paste0(
      '<tr> \n <th></th> \n <th>', x,
      '</th> \n <th>', y,
      '</th> \n </tr> \n <tr> \n <td style="padding:15px"><h1>', n,
      '</h1></td> <td style="vertical-align:top;padding:5px"><img src="', x,
      '", style="width:200px"></td> \n ',
      '<td style="vertical-align:top;padding:5px"><img src="', y,
      '", style="width:200px"> </td> \n ',
      '<td style="padding:10px"><h2>', z, '</h2></tr> \n',
      '<tr><td colspan=3><hr></td></tr> \n'
    )
  }, x_paths, y_paths, result$match, seq_along(x_paths))
  
  image_elements <- paste0(image_elements, collapse = " \n ")
  
  # Combine output and display in viewer
  html_file <- file.path(temp_dir, "index.html")
  code <- paste(code_head, image_elements, code_foot, sep = "\n")
  writeLines(text = code, con = html_file)
  
  if (interactive()) {
    viewer(html_file)
    # Allow enough time to render HTML
    Sys.sleep(2)
    unlink(temp_dir, recursive = TRUE)
    return(invisible(result))
  } else {
    unlink(temp_dir, recursive = TRUE)
    return(code)
  }
  
}
