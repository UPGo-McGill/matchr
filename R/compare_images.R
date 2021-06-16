#' Compare image matches in a Shiny app
#'
#' \code{compare_images} takes the image matches produced by 
#' \code{\link{identify_matches}} and \code{\link{confirm_matches}} and displays
#' them in an interactive Shiny app for visual inspection and confirmation. 
#' Image matches with extremely high correlation can be optionally excluded, and
#' pairwise duplicates can be detected and excluded as well.
#' 
#' The interface presents pairs of images alongside a best guess as to the match
#' status ("Match" or "No match"). For matches which are correctly identified,
#' no further action is necessary, while incorrect identifications can be
#' corrected by clicking "Match" or "No match" next to the image pair. Images
#' are presented in batches, and at any point the user can click the "Save and
#' exit" button to close the comparison app and retrieve the results up through
#' the last batch which was viewed. This means that even extremely large sets of
#' potential matches can be manually verified over the course of several
#' sessions.
#' 
#' Through the "Enable highlighting" button, specific matches can be highlighted
#' for further follow-up after image comparison is finished.
#'
#' @param result A data frame produced by \code{\link{identify_matches}} (with
#' `confirm = TRUE` or with \code{\link{confirm_matches}} subsequently run on 
#' the results).
#' @param remove_duplicates A logical scalar. Should x-y pairs which are
#' identical to other x-y pairs be reduced to a single x-y pair? This step can
#' be computationally expensive for large datasets, but can dramatically reduce 
#' the number of matches to be verified.
#' @param batch_size An integer scalar. The number of images to display at a 
#' time in the Shiny app (default 100).
#' @param corr_thresh A numeric scalar between 0 and 1. Matches with signature 
#' correlations above this value (default 0.9995) will be pre-emptively marked 
#' as "match". If `remove_duplicates` is TRUE, this same value will be used to
#' identify duplicated images. (I.e. if the correlation between two x or two y
#' images is >= `corr_thresh`, the images will be considered duplicates.) If
#' `corr_thresh` is set to 1, all image matches will be presented for manual 
#' verification unless they are bit-for-bit identical.
#' @param previous A logical scalar. Should the results of previous runs of
#' `compare_images` be incorporated into the new results (default TRUE), or 
#' should previously compared matches be compared again? If this argument is
#' TRUE, then any rows in `result` with a `confirmed` value of TRUE will be
#' removed from the data frame before processing (and so will not be present
#' in the comparison interface) and then re-added unchanged to the output.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A data frame with the following fields: `matrix`, `x_index` and
#' `y_index` from the original `result` data frame; `new_match_status`, which is 
#' a character vector with possible entries "match" and "no match"; and a 
#' logical vector `new_highlight` which is TRUE for any matches which were 
#' highlighted using the in-app interface. The output will have one row for each 
#' image pairing that was confirmed, which is determined by how many pages into 
#' the Shiny app the user proceeded, and thus how many pairings were viewed. If 
#' all pages are viewed, then the output will have the same number of rows as 
#' the input.
#' @examples
#' \dontrun{
#' # Setup
#' sigs <- create_signature(test_urls)
#' matches <- match_signatures(sigs)
#' result <- identify_matches(matches)
#' 
#' # Assign the output of compare_images to retrieve results
#' change_table <- compare_images(result)
#' }
#' @export

compare_images <- function(result, remove_duplicates = TRUE, 
                           batch_size = 100L, corr_thresh = 0.9995, 
                           previous = TRUE, quiet = FALSE) {
  
  ## Setup ---------------------------------------------------------------------
  
  # Check if necessary packages are installed
  if (!requireNamespace("shiny", quietly = TRUE)) stop(
    "For interactive image comparison, install the \"shiny\" package.",
    call. = FALSE)
  
  if (!requireNamespace("shinyjs", quietly = TRUE)) stop(
      "For interactive image comparison, install the \"shinyjs\" package.",
      call. = FALSE)
    
  # Error checking and object initialization
  stopifnot(is.data.frame(result), is.numeric(c(batch_size, corr_thresh)),
            is.logical(c(remove_duplicates, previous)))
  batch_size <- floor(batch_size)
  x_id <- y_id <- .UID <- NULL
  
  # Exit early for zero-row input
  if (nrow(result) == 0) {
    output <- data.frame(matrix = integer(), x_index = integer(), 
                         y_index = integer(), new_match_status = character(),
                         new_highlight = logical())
    if (requireNamespace("dplyr", quietly = TRUE)) {
      output <- dplyr::as_tibble(output)
    }
    return(output)
  }
  # Get list of files, paths and folders
  paths <- compare_get_paths(result)
  
  
  ## Initialize df -------------------------------------------------------------
  
  output <- compare_prepare_table(result, previous, corr_thresh)
  df <- output[[1]]
  df_all <- output[[2]]
  df_prev <- output[[3]]
  df_cor <- output[[4]]
  
  
  ## Remove duplicates ---------------------------------------------------------
  
  if (remove_duplicates) {
    output <- compare_remove_duplicates(df, corr_thresh, quiet)
    df <- output[[1]]
    df_dups <- output[[2]]
    df_full <- output[[3]]
    remove_duplicates <- output[[4]]
  } else {
    df_dups <- df[0,]
    df$x_name <- get_path(df$x_sig)
    df$y_name <- get_path(df$y_sig)
    df_full <- df
    df$duplicates <- 0L
  }
  
  # Update paths
  if (length(paths[[2]]) > 0) df$x_name <- as.vector(mapply(
    sub, paths[[2]], paths[[1]], MoreArgs = list(df$x_name), USE.NAMES = FALSE))
  if (length(paths[[4]]) > 0) df$y_name <- as.vector(mapply(
    sub, paths[[4]], paths[[3]], MoreArgs = list(df$y_name), USE.NAMES = FALSE))
  
  # Make change table
  if (remove_duplicates) {
    change_table <- df[c(".UID", "x_name", "y_name", "match", "x_id", "y_id")]
  } else change_table <- df[c(".UID", "x_name", "y_name", "match")]
  colnames(change_table)[colnames(change_table) == "match"] <- 
    "new_match_status"
  change_table$new_match_status <- ifelse(change_table$new_match_status %in% c(
    "match", "likely match"), "match", "no match")
  
  # Need one table_n row per batch_size entries in each match status
  table_n <- table(df$match)
  table_n <- table_n[order(match(names(table_n), c(
    "match", "likely match", "possible match", "no match")))]
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
  
  # Make summary table
  summary_table <-
    data.frame(
      category = c("Total matches", "Matches previously checked", 
                   "Matches with correlation ~= 1", 
                   "Matches identified as duplicates", "Matches to check", 
                   "Matches", "Likely matches", "Possible matches", 
                   "Non-matches"),
      value = prettyNum(
        c(nrow(result), nrow(df_prev), nrow(df_cor), nrow(df_dups),  
          nrow(result) - nrow(df_prev) - nrow(df_dups) - nrow(df_cor),
          max(c(table_n[table_n$name == "match",]$i_2, 0)),
          max(c(table_n[table_n$name == "likely match",]$i_2, 0)),
          max(c(table_n[table_n$name == "possible match",]$i_2, 0)),
          max(c(table_n[table_n$name == "no match",]$i_2, 0))), big.mark = ","))
  
  # Set Shiny options
  shiny::shinyOptions(df = df, change_table = change_table, table_n = table_n, 
                      summary_table = summary_table, x_paths = paths[[1]], 
                      y_paths = paths[[3]], x_dirs = paths[[2]], 
                      y_dirs = paths[[4]], batch_size = batch_size)
  
  # Return early if not interactive
  if (!interactive()) {
    warning("The `compare_images` tool only runs in interactive mode.")
    output <- data.frame(matrix = integer(), x_index = integer(), 
                         y_index = integer(), new_match_status = character(),
                         new_highlight = logical())
    if (requireNamespace("dplyr", quietly = TRUE)) {
      output <- dplyr::as_tibble(output)
    }
    return(output)
  }
  
  # Launch Shiny app then return results
  out <- shiny::runApp(system.file("compare_images", package = "matchr"))
  output <- compare_finish(out, remove_duplicates, df_full, df_prev, df_cor,
                           df_all)
  return(output)
  
}

# ------------------------------------------------------------------------------

compare_get_paths <- function(result) {
  x_paths <- get_path(result$x_sig)
  x_dirs <- x_paths[!is_url(x_paths)]
  x_dirs <- sub("[^/]+$", "", x_dirs)
  x_dirs <- sort(unique(x_dirs))
  x_dirs <- sub("/$", "", x_dirs)
  x_paths <- paste("x", seq_along(x_dirs), sep = "_")
  
  y_paths <- get_path(result$y_sig)
  y_dirs <- y_paths[!is_url(y_paths)]
  y_dirs <- sub("[^/]+$", "", y_dirs)
  y_dirs <- sort(unique(y_dirs))
  y_dirs <- sub("/$", "", y_dirs)
  y_paths <- paste("y", seq_along(y_dirs), sep = "_")
  
  return(list(x_paths, x_dirs, y_paths, y_dirs))
}

# ------------------------------------------------------------------------------

compare_prepare_table <- function(result, previous, corr_thresh) {
  
  # Prepare result table for processing
  df <- result
  df$.UID <- paste0("id-", formatC(seq_len(nrow(df)), width = floor(log10(
    nrow(df))) + 1, flag = "0"))
  df_all <- df
  
  # Subset table if previous is TRUE
  if (previous && suppressWarnings(!is.null(df$confirmed))) {
    df_prev <- df[df$confirmed == TRUE,]
    df <- df[df$confirmed == FALSE,]
    df$confirmed <- NULL
  } else df_prev <- df[0,]
  
  # Remove results with perfect correlation
  df_cor <- df[df$correlation >= corr_thresh,]
  df <- df[df$correlation < corr_thresh,]
  
  return(list(df, df_all, df_prev, df_cor))
}

# ------------------------------------------------------------------------------

compare_remove_duplicates <- function(df, corr_thresh, quiet = FALSE) {
  
  # Identify x images with correlation ~= 1
  x_matches <- 
    df$x_sig[!duplicated(get_path(df$x_sig))] |> 
    match_signatures() |> 
    identify_matches(quiet = TRUE)
  x_matches <- x_matches[x_matches$correlation >= corr_thresh,]
  
  # Exit early if no matches
  if (nrow(x_matches) == 0) {
    df_dups <- df[0,]
    df$x_name <- get_path(df$x_sig)
    df$y_name <- get_path(df$y_sig)
    df_full <- df
    df$duplicates <- 0L
    # Change value of remove_duplicates for subsequent steps
    return(list(df, df_dups, df_full, FALSE))
  }
  
  # Group x images together by correlation
  x_matches <- mapply(function(x, y) c(x, y), get_path(x_matches$x_sig),
                      get_path(x_matches$y_sig), SIMPLIFY = FALSE, 
                      USE.NAMES = FALSE)
  
  # Add duplicates
  dup_x <- table(get_path(df$x_sig))
  dup_x <- dup_x[dup_x >= 2]
  dup_x <- lapply(names(dup_x), function(x) x)
  x_matches <- c(x_matches, dup_x)
  
  # Reduce x_matches
  x_reduced <- reduce(x_matches, "Identifying x duplicate", quiet)
  
  # Create x table
  if (requireNamespace("dplyr", quietly = TRUE)) {
    x_table <- lapply(seq_along(x_reduced), function(n) 
      dplyr::tibble(x_id = n, x_name = x_reduced[[n]]))
    x_table <- dplyr::bind_rows(x_table)
  } else {
    x_table <- lapply(seq_along(x_reduced), function(n) 
      data.frame(x_id = n, x_name = x_reduced[[n]]))
    x_table <- do.call(rbind, x_table)
  }
  x_table <- x_table[!duplicated(x_table),]
  
  # Join IDs to df table
  df$x_name <- get_path(df$x_sig)
  df <- merge(df, x_table, all = TRUE)
  
  # Identify y images with correlation ~= 1 with counterparts in x_table
  y_sig <- df[!is.na(df$x_id),]$y_sig
  y_sig <- y_sig[!duplicated(get_path(y_sig))]
  y_matches <- 
    y_sig |> 
    match_signatures() |> 
    identify_matches(quiet = TRUE)
  y_matches <- y_matches[y_matches$correlation >= corr_thresh,]
  
  # Group y images together by correlation
  y_matches <- mapply(function(x, y) c(x, y), get_path(y_matches$x_sig),
                      get_path(y_matches$y_sig), SIMPLIFY = FALSE, 
                      USE.NAMES = FALSE)
  
  # Add duplicates
  dup_y <- df[!is.na(df$x_id),]$y_sig
  dup_y <- table(get_path(dup_y))
  dup_y <- dup_y[dup_y >= 2]
  dup_y <- lapply(names(dup_y), function(x) x)
  y_matches <- c(y_matches, dup_y)
  
  # Reduce y_matches
  y_reduced <- reduce(y_matches, "Identifying y match", quiet)
  
  # Create y table
  if (requireNamespace("dplyr", quietly = TRUE)) {
    y_table <- lapply(seq_along(y_reduced), function(n) 
      dplyr::tibble(y_id = n, y_name = y_reduced[[n]]))
    y_table <- dplyr::bind_rows(y_table)
  } else {
    y_table <- lapply(seq_along(y_reduced), function(n) 
      data.frame(y_id = n, y_name = y_reduced[[n]]))
    y_table <- do.call(rbind, y_table)
  }
  y_table <- y_table[!duplicated(y_table),]
  
  # Join IDs to df table
  df$y_name <- get_path(df$y_sig)
  df <- merge(df, y_table, all = TRUE)
  
  # Get full df for later
  df_full <- df
  
  # Create trimmed df table
  df_b <- df[!is.na(df$x_id) & !is.na(df$y_id),]
  df_b <- df_b[order(df_b$x_id, df_b$y_id, -1 * df_b$correlation),]
  df_dups <- df_b[duplicated(df_b[c("x_id", "y_id")]),]
  df_b <- df_b[!duplicated(df_b[c("x_id", "y_id")]),]
  df_unique <- df[is.na(df$x_id) | is.na(df$y_id),]
  
  df <- rbind(df_b[!names(df_b) %in% c("x_sig", "y_sig")], 
              df_unique[!names(df_unique) %in% c("x_sig", "y_sig")])
  df <- df[order(df$.UID),]
  
  # Add duplicate counts
  dup_list <- stats::aggregate(df_full, by = list(df_full$x_id, df_full$y_id), 
                               length)
  dup_list <- dup_list[c("Group.1", "Group.2", "y_name")]
  names(dup_list) <- c("x_id", "y_id", "duplicates")
  dup_list <- dup_list[dup_list$duplicates >= 2,]
  dup_list$duplicates <- dup_list$duplicates - 1L
  df <- merge(df, dup_list, all = TRUE)
  df$duplicates <- ifelse(is.na(df$duplicates), 0L, df$duplicates)
  if (requireNamespace("dplyr", quietly = TRUE)) df <- dplyr::as_tibble(df)
  
  return(list(df, df_dups, df_full, TRUE))
}

# ------------------------------------------------------------------------------

compare_finish <- function(out, remove_duplicates, df_full, df_prev, df_cor,
                           df_all) {
  
  highlight <- out[[2]]
  out <- out[[1]]
  
  if (remove_duplicates) {
    
    change_groups <- out[c("x_id", "y_id", "new_match_status")]
    change_groups <- subset(change_groups, !is.na(x_id) & !is.na(y_id))
    
    # Get match status for de-duplicated matches
    out_b <- merge(df_full, change_groups)
    out_b <- out_b[c(".UID", "new_match_status")]
    
  } else out_b <- data.frame(.UID = character(), new_match_status = character())
  
  out_a <- subset(out, !.UID %in% out_b$.UID)
  out_a <- out_a[c(".UID", "new_match_status")]
  
  # Add previous results
  out_prev <- df_prev[c(".UID", "match")]
  names(out_prev) <- c(".UID", "new_match_status")
  
  # Add correlation == 1 results
  out_cor <- df_cor[".UID"]
  out_cor$new_match_status <- "match"
  
  # Combine results
  out_IDs <- rbind(out_a, out_b, out_prev, out_cor)
  output <- df_all[c("matrix", "x_index", "y_index", ".UID")]
  output <- merge(output, out_IDs)
  
  # Add highlights
  if (sum(highlight) > 0) {
    highlight <- data.frame(.UID = names(highlight), new_highlight = highlight,
                            row.names = NULL)
    output <- merge(output, highlight, all.x = TRUE, all.y = FALSE)
  } else output$new_highlight <- NA
  
  # Return output
  output$.UID <- NULL
  if (requireNamespace("dplyr", quietly = TRUE)) {
    output <- dplyr::as_tibble(output)
  }
  
  return(output)
  
}