#' Compare image matches in the viewer window
#'
#' \code{compare_images} TKTK
#'
#' TKTK
#'
#' @param result TKTK
#' @param remove_duplicates TKTK
#' @param previous TKTK
#' @return TKTK
#' @export

compare_images <- function(result, remove_duplicates = TRUE, previous = NULL) {
  
  # Error checking and object initialization
  stopifnot(is.data.frame(result), is.logical(remove_duplicates))
  result$.UID <- paste0("id-", formatC(seq_len(nrow(
    result)), width = floor(log10(nrow(result))) + 1, flag = "0"))
  result_full <- result
  
  # Create temp objects and subfolders
  temp_dir <- tempfile()
  dir.create(temp_dir)
  x_dir <- paste0(temp_dir, "/x")
  dir.create(x_dir)
  y_dir <- paste0(temp_dir, "/y")
  dir.create(y_dir)
  
  # Remove duplicates
  if (remove_duplicates) {
    
    # Helpers
    can_merge <- function(x, y) length(intersect(x, y)) > 0
    merge_fun <- function(x, y) sort(union(x, y))
    reduce_fun <- function(img_list) {
      Reduce(function(acc, curr) {
        curr_vec <- curr[[1]]
        to_merge_id_x <- Position(f = function(x) can_merge(x, curr_vec), acc)
        if (is.na(to_merge_id_x)) acc[[length(acc) + 1]] <- curr_vec else {
          acc[[to_merge_id_x]] <- merge_fun(acc[[to_merge_id_x]], curr_vec)
        }
        return(acc)
      }, img_list)
    }
    
    # Identify x images with correlation ~= 1
    x_sig <- result$x_sig[!duplicated(field(result$x_sig, "file"))]
    x_matches <- match_signatures(x_sig)
    x_matches <- identify_matches(x_matches)
    x_matches <- x_matches[x_matches$correlation >= 0.9995,]
    
    # Group x images together by correlation
    x_matches <- mapply(function(x, y) c(x, y), field(x_matches$x_sig, "file"),
                      field(x_matches$y_sig, "file"), SIMPLIFY = FALSE, 
                      USE.NAMES = FALSE)
    x_matches <- reduce_fun(Map(list, x_matches))
    
    # Check for duplication
    while (length(unique(unlist(x_matches))) != 
           length(unlist(lapply(x_matches, unique)))) {
      x_all <- unlist(lapply(x_matches, unique))
      x_pos <- x_matches[sapply(x_matches, function(x) 
        any(x_all[which(duplicated(x_all))] %in% x))]
      x_neg <- x_matches[!sapply(x_matches, function(x) 
        any(x_all[which(duplicated(x_all))] %in% x))]
      x_matches <- reduce_fun(c(list(x_neg), lapply(x_pos, list)))
    }
    
    # Create x table
    if (requireNamespace("dplyr", quietly = TRUE)) {
      x_table <- lapply(seq_along(x_matches), function(n) 
        dplyr::tibble(x_id = n, x_name = x_matches[[n]]))
      x_table <- dplyr::bind_rows(x_table)
      } else {
        x_table <- lapply(seq_along(x_matches), function(n) 
          data.frame(x_id = n, x_name = x_matches[[n]]))
        x_table <- do.call(rbind, x_table)
        }
    x_table <- x_table[!duplicated(x_table),]
    
    # Join IDs to result table
    result$x_name <- field(result$x_sig, "file")
    result <- merge(result, x_table, all = TRUE)
    
    # Identify y images with correlation ~= 1 with counterparts in x_table
    y_matches <- result[!is.na(result$x_id),]$y_sig
    y_matches <- match_signatures(y_matches)
    y_matches <- identify_matches(y_matches)
    y_matches <- y_matches[y_matches$correlation >= 0.9995,]
    
    # Group y images together by correlation
    y_matches <- mapply(function(x, y) c(x, y), field(y_matches$x_sig, "file"),
                        field(y_matches$y_sig, "file"), SIMPLIFY = FALSE, 
                        USE.NAMES = FALSE)
    y_matches <- reduce_fun(Map(list, y_matches))
    
    # Check for duplication
    while (length(unique(unlist(y_matches))) !=
           length(unlist(lapply(y_matches, unique)))) {
      y_all <- unlist(lapply(y_matches, unique))
      y_pos <- y_matches[sapply(y_matches, function(x) 
        any(y_all[which(duplicated(y_all))] %in% x))]
      y_neg <- y_matches[!sapply(y_matches, function(x) 
        any(y_all[which(duplicated(y_all))] %in% x))]
      y_matches <- reduce_fun(c(list(y_neg), lapply(y_pos, list)))
    }
    
    # Create y table
    if (requireNamespace("dplyr", quietly = TRUE)) {
      y_table <- lapply(seq_along(y_matches), function(n) 
        dplyr::tibble(y_id = n, y_name = y_matches[[n]]))
      y_table <- dplyr::bind_rows(y_table)
    } else {
      y_table <- lapply(seq_along(y_matches), function(n) 
        data.frame(y_id = n, y_name = y_matches[[n]]))
      y_table <- do.call(rbind, y_table)
      }
    y_table <- y_table[!duplicated(y_table),]
    
    # Join IDs to result table
    result$y_name <- field(result$y_sig, "file")
    result <- merge(result, y_table, all = TRUE)
    
    # Create trimmed result table
    result_b <- result[!is.na(result$x_id) & !is.na(result$y_id),]
    result_b <- result_b[order(result_b$x_id, result_b$y_id, 
                               -1 * result_b$correlation),]
    result <- dplyr::bind_rows(result_b[!duplicated(result_b[c("x_id", "y_id")]),], 
                               result[is.na(result$x_id) | is.na(result$y_id),])
    result <- result[order(result$.UID),]
    if (requireNamespace("dplyr", quietly = TRUE)) {
      result <- dplyr::as_tibble(result)}
  }
  
  # Remove results with perfect correlation
  result_corr <- result[result$correlation >= 0.9995,]
  result <- result[result$correlation < 0.9995,]
  
  # Launch Shiny app if Shiny is present
  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::shinyOptions(result = result, result_full = result_full, 
                        result_corr = result_corr, x_dir = x_dir, y_dir = y_dir,
                        remove_duplicates = remove_duplicates)
    if (remove_duplicates) shiny::shinyOptions(result_b = result_b)
    output <- shiny::runApp(appDir = system.file("compare_images",
                                                 package = "matchr"))
    return(output)
  }
  
  # Issue warning and prepare viewer
  warning("For interactive image comparison, install the \"shiny\" package. ",
          "A maximum of 1000 image pairs will be shown in the Viewer.",
          call. = FALSE)
  viewer <- getOption("viewer")
  
  # Copy images to temp folders
  result <- result[1:1000,]
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
  image_elements <- mapply(function(x, y, n) {
    paste0(
      '<tr> \n <th></th> \n <th>', x,
      '</th> \n <th>', y,
      '</th> \n </tr> \n <tr> \n <td style="padding:15px"><h1>', n,
      '</h1></td> <td style="vertical-align:top;padding:5px"><img src="', x,
      '", style="width:200px"></td> \n ',
      '<td style="vertical-align:top;padding:5px"><img src="', y,
      '", style="width:200px"> </td> \n </tr> \n',
      '<tr><td colspan=3><hr></td></tr> \n'
    )
  }, x_paths, y_paths, seq_along(x_paths))
  
  image_elements <- paste0(image_elements, collapse = " \n ")
  
  # Combine output and display in viewer
  html_file <- file.path(temp_dir, "index.html")
  code <- paste(code_head, image_elements, code_foot, sep = "\n")
  writeLines(text = code, con = html_file)
  if (!is.null(viewer)) viewer(html_file) else utils::browseURL(html_file)
  # unlink(temp_dir, recursive = TRUE)
  invisible(result)
  
}
