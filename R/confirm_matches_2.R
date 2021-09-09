#' Confirm image matches in a Shiny app
#'
#' \code{confirm_matches_2} takes the image matches produced by 
#' \code{\link{identify_matches}} and displays them in an interactive Shiny app 
#' for visual inspection and confirmation. Image matches with extremely low 
#' Hamming distances can be optionally excluded, and pairwise duplicates can be 
#' detected and excluded as well.
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
#' @param thresholds A named integer vector. Which Hamming distances establish
#' thresholds for an "Identical" match (default 2L), a "Match" (default 4L), a 
#' "Likely match" (default 12L), a "Possible match" (default 15L), and "No 
#' match" (remaining values)? Image pairs with a distance equal to or less than 
#' the "Identical" threshold will be considered exact duplicates and will not be 
#' shown for verification in the comparison app. (Set "Identical" to -1L to 
#' force manual verification of all image pairs). Remaining image pairs will be 
#' grouped in the comparison app by these thresholds. Image pairs with distances 
#' equal to or under the "Likely match" value will be given a default value of 
#' "match" in the comparison app, while others will be given a default value of 
#' "no match". If `remove_duplicates` is TRUE, the "Identical" threshold will be 
#' used to identify duplicated images. (I.e. if the distance between two `x` or 
#' two `y` images is <= the "Identical" threshold value, the images will be 
#' considered duplicates.) If `thresholds` elements are not named, their names
#' will be inferred by ordering the values from smallest to largest. If
#' `thresholds` elements are not integers, they will be silently converted to 
#' integers by truncating all digits to the right of the decimal point.
#' @param previous A logical scalar. Should the results of previous runs of
#' `compare_images` be incorporated into the new results (default TRUE), or 
#' should previously compared matches be compared again? If this argument is
#' TRUE, then any rows in `result` with a `confirmed` value of TRUE will be
#' removed from the data frame before processing (and so will not be present
#' in the comparison interface) and then re-added unchanged to the output.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A data frame with the following fields: `index` from the original 
#' `result` data frame; a logical vector `new_match_status`, which is TRUE for
#' confirmed matches, FALSE for confirmed non-matches, and NA for matches which
#' were not confirmed; and a logical vector `new_highlight` which is TRUE for 
#' any matches which were highlighted using the in-app interface, FALSE for 
#' matches which were not highlighted, and NA for matches which were not 
#' confirmed. Confirmation is determined by how many pages into the Shiny app 
#' the user proceeded, and thus how many pairings were viewed. If all pages are 
#' viewed, then the output will have no NA values.
#' @examples
#' \dontrun{
#' # Setup
#' sigs <- create_signature(test_urls)
#' matches <- match_signatures(sigs)
#' result <- identify_matches(matches)
#' 
#' # Assign the output of compare_images to retrieve results
#' change_table <- confirm_matches(result)
#' }
#' @export

confirm_matches_2 <- function(result, remove_duplicates = TRUE, 
                              batch_size = 100L, 
                              thresholds = c("Identical" = 4L, "Match" = 6L, 
                                             "Likely match" = 8L, 
                                             "Possible match" = 10L), 
                              previous = TRUE, quiet = FALSE) {
  
  # Check if necessary packages are installed
  if (!requireNamespace("shiny", quietly = TRUE) || 
      !requireNamespace("shinyjs", quietly = TRUE)) stop(
        "`compare_images` requires the \"shiny\" and \"shinyjs\" packages.",
        call. = FALSE)
  
  # Error checking and object initialization
  stopifnot(is.data.frame(result), is.numeric(c(batch_size, thresholds)),
            is.logical(c(remove_duplicates, previous)))
  batch_size <- as.integer(batch_size)
  stopifnot("`thresholds` needs four values." = length(thresholds) == 4)
  thresholds <- thresholds[order(thresholds)]
  thresholds <- as.integer(thresholds)
  names(thresholds) <- c("Identical", "Match", "Likely match", "Possible match")
  
  # Exit early for zero-row input
  if (nrow(result) == 0) {
    output <- data.frame(index = list(), new_match_status = character(),
                         new_highlight = logical())
    if (requireNamespace("dplyr", quietly = TRUE)) {
      output <- dplyr::as_tibble(output)
    }
    return(output)
  }

  # Exit early if not interactive
  if (!interactive()) {
    warning("The `compare_images` tool only runs in interactive mode.")
    output <- data.frame(index = result$index, new_match_status = NA,
                         new_highlight = NA)
    if (requireNamespace("dplyr", quietly = TRUE)) {
      output <- dplyr::as_tibble(output)
    }
    return(output)
  }
  
  # Initialize df
  output <- compare_prepare_table_2(result, previous, thresholds)
  df <- output[[1]]
  df_all <- output[[2]]
  df_prev <- output[[3]]
  df_cor <- output[[4]]
  
  # Remove duplicates
  output <- compare_rm_dups(df, remove_duplicates, thresholds, quiet)
  df <- output[[1]]
  df_dups <- output[[2]]
  df_full <- output[[3]]
  remove_duplicates <- output[[4]]
  
  # Make summary table
  summary_table <-
    data.frame(
      category = c("Total matches", "Matches previously checked", 
                   "Matches with correlation ~= 1", 
                   "Matches identified as duplicates", "Matches to check"),
      value = prettyNum(
        c(nrow(result), nrow(df_prev), nrow(df_cor), nrow(df_dups),  
          nrow(result) - nrow(df_prev) - nrow(df_dups) - nrow(df_cor)), ","))
  
  # Prepare Shiny input
  match_vector <- rep("No match", nrow(df))
  match_vector[df$distance <= thresholds["Possible match"]] <- "Possible match"
  match_vector[df$distance <= thresholds["Likely match"]] <- "Likely match"
  match_vector[df$distance <= thresholds["Match"]] <- "Match"
  dist_vector <- paste("Distance:", df$distance)
  dup_vector <- paste(df$duplicates, "duplicates")
  
  # Launch Shiny app then return results
  out <- compare_images_2(x = df$x_name, y = df$y_name, match = match_vector,
                          info = summary_table, distance = dist_vector,
                          duplicates = dup_vector)
  
  out <- compare_finish_2(out, remove_duplicates, df, df_full, df_prev, df_cor, 
                          df_all)
  return(out)
  
}

# ------------------------------------------------------------------------------

compare_prepare_table_2 <- function(result, previous, thresholds) {
  
  # Prepare result table for processing
  df <- result
  df$.UID <- paste0("id-", formatC(seq_len(nrow(df)), width = floor(log10(
    nrow(df))) + 1, flag = "0"))
  df_all <- df
  
  # Subset table if previous is TRUE
  if (previous && suppressWarnings(!is.null(df$match))) {
    df_prev <- df[!is.na(df$match),]
    df <- df[is.na(df$match),]
  } else {
    df_prev <- df[0,]
    df_prev$match <- NA
    df_prev$highlight <- NA
  }
  
  # Remove results with very low distance
  df_cor <- df[df$distance <= thresholds["Identical"],]
  df <- df[df$distance > thresholds["Identical"],]
  
  return(list(df, df_all, df_prev, df_cor))
}

# ------------------------------------------------------------------------------

compare_rm_dups <- function(df, remove_duplicates, thresholds, quiet = FALSE) {
  
  # Add path names
  df$x_name <- get_path(df$x_sig)
  df$y_name <- get_path(df$y_sig)
  
  if (!remove_duplicates) {
    df$x_id <- NA_integer_
    df$y_id <- NA_integer_
    df <- df[c(".UID", "x_id", "y_id", "x_name", "y_name", "distance")]
    df_dups <- df[0,]
    df_full <- df
    df$duplicates <- 0L
    # Change value of remove_duplicates for subsequent steps
    return(list(df, df_dups, df_full, FALSE))
  }
  
  # Identify x images with correlation ~= 1
  x_matches <- 
    df$x_sig[!duplicated(get_path(df$x_sig))] |> 
    match_signatures_2() |> 
    identify_matches_2(threshold = thresholds["Identical"], quiet = TRUE)
  
  # Exit early if no matches
  if (nrow(x_matches) == 0) {
    df$x_id <- NA_integer_
    df$y_id <- NA_integer_
    df <- df[c(".UID", "x_id", "y_id", "x_name", "y_name", "distance")]
    df_dups <- df[0,]
    df_full <- df
    df$duplicates <- 0L
    return(list(df, df_dups, df_full, FALSE))
  }
  
  # Get list of x path pairs
  x_matches <- mapply(function(x, y) c(x, y), get_path(x_matches$x_sig),
                      get_path(x_matches$y_sig), SIMPLIFY = FALSE, 
                      USE.NAMES = FALSE)
  
  # Add duplicates
  dup_x <- table(get_path(df$x_sig))
  dup_x <- dup_x[dup_x >= 2]
  dup_x <- as.list(names(dup_x))
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
  
  # Join IDs to df table
  df <- merge(df, x_table, all = TRUE)
  
  # Identify y images with correlation ~= 1 with counterparts in x_table
  y_sig <- df[!is.na(df$x_id),]$y_sig
  y_sig <- y_sig[!duplicated(get_path(y_sig))]
  y_matches <- 
    y_sig |> 
    match_signatures_2() |> 
    identify_matches_2(threshold = thresholds["Identical"], quiet = TRUE)
  
  # Exit early if no matches
  if (nrow(y_matches) == 0) {
    df$x_id <- NA_integer_
    df$y_id <- NA_integer_
    df <- df[c(".UID", "x_id", "y_id", "x_name", "y_name", "distance")]
    df_dups <- df[0,]
    df_full <- df
    df$duplicates <- 0L
    return(list(df, df_dups, df_full, FALSE))
  }
  
  # Get list of y path pairs
  y_matches <- mapply(function(x, y) c(x, y), get_path(y_matches$x_sig),
                      get_path(y_matches$y_sig), SIMPLIFY = FALSE, 
                      USE.NAMES = FALSE)
  
  # Add duplicates
  dup_y <- df[!is.na(df$x_id),]$y_sig
  dup_y <- table(get_path(dup_y))
  dup_y <- dup_y[dup_y >= 2]
  dup_y <- as.list(names(dup_y))
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
  
  # Join IDs to df table
  df <- merge(df, y_table, all = TRUE)
  
  # Clean up and save full df for later
  df <- df[c(".UID", "x_id", "y_id", "x_name", "y_name", "distance")]
  df <- df[order(df$.UID),]
  df_full <- df
  
  # Create trimmed df table
  df_b <- df[!is.na(df$x_id) & !is.na(df$y_id),]
  df_b <- df_b[order(df_b$x_id, df_b$y_id, df_b$distance),]
  df_dups <- df_b[duplicated(df_b[c("x_id", "y_id")]),]
  df_b <- df_b[!duplicated(df_b[c("x_id", "y_id")]),]
  df_unique <- df[is.na(df$x_id) | is.na(df$y_id),]
  
  df <- rbind(df_b, df_unique)
  
  # Add duplicate counts
  dup_list <- 
    stats::aggregate(df_full, by = list(df_full$x_id, df_full$y_id), length)
  dup_list <- dup_list[c("Group.1", "Group.2", "y_name")]
  names(dup_list) <- c("x_id", "y_id", "duplicates")
  dup_list <- dup_list[dup_list$duplicates >= 2,]
  dup_list$duplicates <- dup_list$duplicates - 1L
  df <- merge(df, dup_list, all = TRUE)
  df$duplicates <- ifelse(is.na(df$duplicates), 0L, df$duplicates)
  df <- df[order(df$.UID),]
  df <- 
    df[c(".UID", "x_id", "y_id", "x_name", "y_name", "distance", "duplicates")]
  if (requireNamespace("dplyr", quietly = TRUE)) df <- dplyr::as_tibble(df)
  
  return(list(df, df_dups, df_full, TRUE))
}

# ------------------------------------------------------------------------------

compare_finish_2 <- function(out, remove_duplicates, df, df_full, df_prev, 
                             df_cor, df_all) {
  
  x_id <- y_id <- .UID <- NULL
  out$.UID <- df$.UID
  
  if (remove_duplicates) {
    
    out$x_id <- df$x_id
    out$y_id <- df$y_id
    
    change_groups <- out[c("x_id", "y_id", "new_match_status", "new_highlight")]
    change_groups <- subset(change_groups, !is.na(x_id) & !is.na(y_id))
    
    # Get match status for de-duplicated matches
    out_b <- merge(df_full, change_groups)
    out_b <- out_b[c(".UID", "new_match_status", "new_highlight")]
    
  } else out_b <- data.frame(.UID = character(), new_match_status = logical(),
                             new_highlight = logical())
  
  out_a <- subset(out, !.UID %in% out_b$.UID)
  out_a <- out_a[c(".UID", "new_match_status", "new_highlight")]
  
  # Add previous results
  out_prev <- df_prev[c(".UID", "match", "highlight")]
  names(out_prev) <- c(".UID", "new_match_status", "new_highlight")
  
  # Add correlation == 1 results
  out_cor <- df_cor[".UID"]
  out_cor$new_match_status <- TRUE
  out_cor$new_highlight <- FALSE
  
  # Combine results
  out_IDs <- rbind(out_a, out_b, out_prev, out_cor)
  out <- df_all[c("index", ".UID")]
  out <- merge(out, out_IDs)
  
  # Return output
  out$.UID <- NULL
  if (requireNamespace("dplyr", quietly = TRUE)) out <- dplyr::as_tibble(out)
  return(out)
  
}