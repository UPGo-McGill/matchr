#' Match images based on colour signatures
#'
#' \code{match_signatures} takes one or two lists of images and produces a
#' correlation matrix to identify matches.
#'
#' A function for identifying matching images. The function takes one or two
#' vectors of images signatures (class `matchr_signature`) and compares their
#' colour signatures to find matches.
#'
#' The comparison is done by creating colour signatures for each input image
#' using \code{\link{create_signature}} and then computing the Pearson
#' correlation coefficient between these signatures. In general, pairs of images
#' which were identical prior to arbitrary resampling and compression will have
#' correlation coefficients of at least 0.99.
#'
#' The function can optionally filter images by aspect ratio, so only images
#' with very similar aspect ratios will be compared. This can remove potential
#' false positives and possibly speed up function execution, if images are
#' relatively evenly split between aspect ratios.
#'
#' @param x,y Vectors of class `matchr_signature` to be matched. If `y` is 
#' missing (default), each object in `x` will be matched against each other 
#' object in `x.` If `y` is present, each object in `x` will be matched against 
#' each object in `y`.
#' @param method A character scalar. Should images be compared by their
#' greyscale signatures ("grey", "greyscale, "gray", or "grayscale", the 
#' default), their colour signatures ("colour", "color", "rgb", or "RGB), or 
#' both ("both")?
#' @param compare_aspect_ratios A logical scalar. Should signatures only be
#' compared for images with similar aspect ratios (default)? If TRUE, k-means
#' clustering is used to identify breakpoints between aspect ratios that
#' maximize between-group distance and minimize the total number of calculations
#' that the function needs to execute. (Values of k between 3 and 12 are
#' evaluated.) Image signatures from `x` are split into lists between these
#' break points. In order to catch matches that would fall across a break point,
#' image signatures from `y` are split into lists between the break point / 1.2
#' on the lower bound and the break point * 1.2 on the upper bound. This
#' argument is forced to FALSE if either `x` or `y` has fewer than 10 non-NA
#' elements.
#' @param backup A logical scalar. Should the function store an ongoing backup
#' of progress in a hidden object `.matchr_env$smatch_backup` in the package's
#' environment (default)? If TRUE, the function will attempt to resume progress
#' if it detects a previous backup, and it will remove the backup if the
#' function successfully completes. Backups can be removed with
#' \code{\link{remove_backups}}.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return TKTK A vector of class `matchr_matrix`. This object is a list
#' containing a number of correlation matrices (each of class `matchr_matrix`)
#' equal to the number of aspect-ratio categories. If `x` and `y` are both
#' present, each matrix will have `length(x)` rows and `length(y)` columns, and
#' for the matrix `Q` the cell `Q[i, j]` will be the Pearson correlation
#' coefficient between images `x[[i]]` and `y[[j]]`. If `y` is not present, each
#' matrix will be square, and the cell `Q[i, j]` will be the correlation between
#' images `x[[i]]` and `x[[j]]`.
#' @export

match_signatures <- function(x, y = NULL, method = "grey",
                             compare_aspect_ratios = TRUE, backup = TRUE,
                             quiet = FALSE) {

  # Error handling and object initialization
  stopifnot(is_signature(x), 
            method %in% c("grey", "gray", "colour", "color", "both"),
            is.logical(compare_aspect_ratios), is.logical(backup),
            is.logical(quiet))
  if (!missing(y)) stopifnot(is_signature(y))
  if (missing(y)) {x_only <- TRUE; y <- x} else x_only <- FALSE


  # Prepare method
  if (method %in% c("grey", "gray", "greyscale", "grayscale")) {
    method <- "grey"
    x <- trim_signature(x, 1:(sig_length(x) / 4)) 
    y <- trim_signature(y, 1:(sig_length(y) / 4)) 
  }

  if (method %in% c("colour", "color", "rgb", "RGB")) {
    method <- "colour"
    x <- trim_signature(x, (sig_length(x) / 4 + 1):sig_length(x))
    y <- trim_signature(y, (sig_length(y) / 4 + 1):sig_length(y)) 
  }

  # Prepare backup
  # if (backup) {
  #
  #   # Silence R CMD check re: function backup
  #   # sig_backup <- NULL
  #
  #   # Check for previous backup
  #   if (exists("match_backup", envir = .matchr_env)) {
  #     # Only proceed if old input size is identical to new input size
  #     if (.matchr_env$match_backup_size == object.size(x)) {
  #       result <- .matchr_env$match_backup
  #       resume_from <- length(result[!sapply(result, is.null)]) + 1 # TKTK NEED TO UPDATE
  #       if (!quiet) cat("Backup detected. Resuming from position ",
  #                       sum(sapply(.matchr_env$match_backup, length)) + 1,
  #                       ".\n", sep = "")
  #     } else {
  #       stop("The backup detected in .matchr_env$match_backup does not match ",
  #            "the input. Remove the previous backup with ",
  #            "`remove_backups()` to proceed.")
  #     }
  #   } else {
  #     assign("match_backup", result, envir = .matchr_env)
  #     assign("match_backup_size", object.size(x), envir = .matchr_env)
  #   }
  # }

  # Deal with NAs
  x_na <- x[is.na(x)]
  y_na <- y[is.na(y)]
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  # Split clusters for compare_aspect_ratios == TRUE
  if (vec_size(x) < 10 || vec_size(y) < 10) compare_aspect_ratios <- FALSE
  if (compare_aspect_ratios == TRUE) {
    clusters <- get_clusters(x, y)
    x_list <- clusters[[1]]
    y_list <- clusters[[2]]
  } else {
    x_list <- list(x)
    y_list <- list(y)
  }

  # Sort out parallelization options
  par_check_vec <- suppressMessages(sapply(lengths(x_list), function(x) 
    set_par("match_signatures", x = x)))
  
  # Calculate correlation matrices
  result <- vector("list", length(x_list))
  for (i in seq_along(x_list)) {
    if (par_check_vec[i]) {
      par_check <- TRUE
      x_matrix <- chunk(x_list[[i]], number_of_threads())
      x_matrix <- lapply(x_matrix, function(x) {
        matrix(unlist(field(x, "signature")), ncol = vec_size(x))})
      y_matrix <- matrix(unlist(field(y_list[[i]], "signature")), 
                         ncol = vec_size(y_list[[i]]))
      result[[i]] <- par_lapply(x_matrix, stats::cor, y_matrix)
      result[[i]] <- do.call(rbind, result[[i]])
    } else {
      par_check <- FALSE
      x_matrix <- matrix(unlist(field(x_list[[i]], "signature")), 
                         ncol = vec_size(x_list[[i]]))
      y_matrix <- matrix(unlist(field(y_list[[i]], "signature")), 
                         ncol = vec_size(y_list[[i]]))
      result[[i]] <- stats::cor(x_matrix, y_matrix)
      }
  }
  
  # Return result
  get_ratios <- function(x) c(min(field(x, "aspect_ratio"), na.rm = TRUE), 
                              max(field(x, "aspect_ratio"), na.rm = TRUE))
  new_matrix(
    x = result,
    x_ratios = lapply(x_list, get_ratios),
    y_ratios = lapply(y_list, get_ratios),
    x_files = lapply(x_list, field, "file"),
    y_files = lapply(y_list, field, "file"),
    x_total = vec_size(x) + vec_size(x_na),
    y_total = vec_size(y) + vec_size(y_na),
    x_na = field(x_na, "file"),
    y_na = field(y_na, "file")
  )

}

# ------------------------------------------------------------------------------

get_clusters <- function(x, y, stretch = 1.2) {
  
  x_ratios <- field(x, "aspect_ratio")
  y_ratios <- field(y, "aspect_ratio")
  
  # Set number of groups to evaluate
  unique_points <- unique(stats::na.omit(c(x_ratios, y_ratios)))
  sum_fun <- function(x, y) length(x_ratios[x_ratios >= x & x_ratios <= y]) * 
    length(y_ratios[y_ratios >= (x / stretch) & y_ratios <= (y * stretch)])
  set.seed(1)
  groups <- lapply(3:min(length(unique_points) - 1, 12), function(n) {
    means <- stats::kmeans(unique_points, n)
    mins <- sort(sapply(seq_len(n), function(n) 
      min(unique_points[means$cluster == n])))
    maxs <- sort(sapply(seq_len(n), function(n) 
      max(unique_points[means$cluster == n])))
    calcs <- mapply(sum_fun, mins, maxs)
    list(sum(calcs), mins, maxs)
  })
  
  # Choose k value which minimizes calculations
  k <- which.min(sapply(groups, `[[`, 1)) + 2
  k <- max(min(k, floor(vec_size(x) ^ (1/3)), floor(vec_size(y) ^ (1/3))), 3)
  cut_min <- groups[[k - 2]][[2]]
  cut_max <- groups[[k - 2]][[3]]
  
  # Collapse lists if any are empty
  zero_fun <- 
    function(x, y) c(sum(x_ratios >= x & x_ratios <= y, na.rm = TRUE), 
                     sum(y_ratios >= (x / stretch) & y_ratios <= (y * stretch), na.rm = TRUE))
  vec_lens <- t(mapply(zero_fun, cut_min, cut_max, SIMPLIFY = "matrix"))
  
  while (prod(vec_lens) == 0) {
    for (i in seq_len(nrow(vec_lens) - 1)) {
      if (prod(vec_lens[i:i + 1,]) == 0) {
        cut_min[i + 1] <- cut_min[i]
        cut_min <- cut_min[-i]
        cut_max <- cut_max[-i]
        break
      }
    }
    vec_lens <- t(mapply(zero_fun, cut_min, cut_max, SIMPLIFY = "matrix"))
  }
  
  # Process and return lists
  cut_fun <- function(x, y, vec_1, vec_2) vec_1[vec_2 >= x & vec_2 <= y]
  x_list <- mapply(cut_fun, cut_min, cut_max, 
                   MoreArgs = list(vec_1 = x, vec_2 = x_ratios))
  y_list <- mapply(cut_fun, cut_min / 1.2, cut_max * 1.2, 
                   MoreArgs = list(vec_1 = y, vec_2 = y_ratios))
  return(list(x_list, y_list))
}
