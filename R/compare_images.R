#' Compare image matches in the viewer window
#'
#' \code{compare_images} TKTK
#'
#' TKTK
#'
#' @param result TKTK
#' @param x_sigs TKTK
#' @param y_sigs TKTK
#' @param remove_duplicates TKTK
#' @return TKTK
#' @export

compare_images <- function(result, x_sigs = NULL, y_sigs = NULL,
                           remove_duplicates = TRUE) {

  ### Error checking ###########################################################

  stopifnot(is.data.frame(result), is.logical(remove_duplicates))


  ### Create temp folder and file path #########################################

  ## Create main temp objects --------------------------------------------------

  temp_dir <- tempfile()
  dir.create(temp_dir)


  ## Create image subfolders ---------------------------------------------------

  x_dir <- paste0(temp_dir, "/x")
  dir.create(x_dir)

  y_dir <- paste0(temp_dir, "/y")
  dir.create(y_dir)


  ### Launch Shiny app if Shiny is present #####################################

  if (requireNamespace("shiny", quietly = TRUE)) {

    shiny::shinyOptions(result = result, x_dir = x_dir, y_dir = y_dir,
                        x_sigs = if (!missing(x_sigs)) x_sigs else NULL,
                        y_sigs = if (!missing(y_sigs)) y_sigs else NULL,
                        remove_duplicates = remove_duplicates)

    output <-
      shiny::runApp(appDir = system.file("compare_images", package = "matchr"))

    output <- new_matchr_change_table(output)

    return(output)

  }


  ### Issue warning and prepare viewer #########################################

  warning("For interactive image comparison, install the \"shiny\" package.")

  viewer <- getOption("viewer")


  ### Copy images to temp folders ##############################################

  file.copy(result$x_name, x_dir)
  file.copy(result$y_name, y_dir)


  ### Make new path vectors ####################################################

  x_paths <- strsplit(result$x_name, '/')
  x_paths <- sapply(x_paths, function(x) x[[length(x)]])
  x_paths <- paste0("x/", x_paths)

  y_paths <- strsplit(result$y_name, '/')
  y_paths <- sapply(y_paths, function(x) x[[length(x)]])
  y_paths <- paste0("y/", y_paths)


  ### Create header and footer strings #########################################

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


  ### Create image elements ####################################################

  image_elements <- mapply(function(x, y, n) {

    paste0(
      '<tr> \n <th></th> \n <th>',
      x,
      '</th> \n <th>',
      y,
      '</th> \n </tr> \n <tr> \n <td style="padding:15px"><h1>',
      n,
      '</h1></td> <td style="vertical-align:top;padding:5px"><img src="',
      x,
      '", style="width:200px"></td> \n ',
      '<td style="vertical-align:top;padding:5px"><img src="',
      y,
      '", style="width:200px"> </td> \n </tr> \n',
      '<tr><td colspan=3><hr></td></tr> \n'
      )

  }, x_paths, y_paths, seq_along(x_paths))

  image_elements <- paste0(image_elements, collapse = " \n ")


  ### Combine output and display in viewer #####################################

  html_file <- file.path(temp_dir, "index.html")

  code <- paste(code_head, image_elements, code_foot, sep = "\n")

  writeLines(text = code, con = html_file)

  if (!is.null(viewer)) {
    viewer(html_file)
    } else utils::browseURL(html_file)

  invisible(result)

}
