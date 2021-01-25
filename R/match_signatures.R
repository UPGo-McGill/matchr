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
#' @param compare_ar A logical scalar. Should signatures only be compared for 
#' images with similar aspect ratios (default)? If TRUE, k-means clustering is 
#' used to identify breakpoints between aspect ratios that maximize 
#' between-group distance and minimize the total number of calculations that the 
#' function needs to execute. (Values of k between 3 and 8 are evaluated.) Image 
#' signatures from `x` are split into lists between these break points. This 
#' argument is forced to FALSE if either `x` or `y` has fewer than 10 non-NA 
#' elements.
#' @param stretch A numeric scalar. When `compare_ar` is TRUE, in order to catch 
#' matches that would fall across a break point, image signatures from `y` are 
#' split into lists between the break point / `stretch` (default 1.2) on the 
#' lower bound and the break point * `stretch` on the upper bound. Increasing 
#' this value will possibly catch matches between extremely distorted images,
#' but at the cost of potentially larger numbers of false positives, and 
#' substantially increased processing time. 
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A vector of class `matchr_matrix`, each element of which is a
#' correlation matrix for the `x` and `y` signatures falling in a given aspect
#' ratio range. If `x` and `y` are both present, each matrix will have 
#' `length(x)` rows and `length(y)` columns, and for the matrix `Q` the cell 
#' `Q[i, j]` will be the Pearson correlation coefficient between images `x[[i]]` 
#' and `y[[j]]`. If `y` is not present, each matrix will be square, and the cell 
#' `Q[i, j]` will be the correlation between images `x[[i]]` and `x[[j]]`.
#' @export

match_signatures <- function(x, y = NULL, method = "grey",
                             compare_ar = TRUE, stretch = 1.2, 
                             quiet = FALSE) {
  
  # Error handling and object initialization
  stopifnot(is_signature(x), is.logical(c(compare_ar, quiet)),
            method %in% c("grey", "gray", "colour", "color", "rgb", "RGB", 
                          "both"))
  if (missing(y)) y <- x else stopifnot(is_signature(y))
  
  # Deal with NAs
  x_na <- x[is.na(x)]
  y_na <- y[is.na(y)]
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  # Split clusters for compare_ar == TRUE
  if (vec_size(x) < 10 || vec_size(y) < 10) compare_ar <- FALSE
  if (compare_ar == TRUE) {
    clusters <- get_clusters(x, y, stretch = stretch)
    x_list <- clusters[[1]]
    y_list <- clusters[[2]]
  } else {
    x_list <- list(x)
    y_list <- list(y)
  }
  
  # Get full signatures for final result
  x_sig <- x_list
  y_sig <- y_list
  
  # Prepare method
  if (method %in% c("grey", "gray", "greyscale", "grayscale")) {
    x_list <- lapply(x_list, trim_signature, 1:(sig_length(x) / 4))
    y_list <- lapply(y_list, trim_signature, 1:(sig_length(x) / 4))
  }
  
  if (method %in% c("colour", "color", "rgb", "RGB")) {
    x_list <- lapply(x_list, trim_signature, 
                     (sig_length(x) / 4 + 1):sig_length(x))
    y_list <- lapply(y_list, trim_signature, 
                     (sig_length(x) / 4 + 1):sig_length(x))
  }
  
  # Initialize progress reporting
  handler_matchr("Matching signature")
  prog_bar <- as.logical((vec_size(x) >= 5000) * as.numeric(!quiet) *
                           progressr::handlers(global = NA))
  pb <- progressr::progressor(steps = vec_size(x), enable = prog_bar)
  
  # Prepare parallel processing
  par_check_vec <- 
    mapply(function(x, y) set_par("match_signatures", x = x, y = y),
           lengths(x_list), lengths(y_list), USE.NAMES = FALSE)
  
  # Calculate correlation matrices
  result <- vector("list", length(x_list))
  for (i in seq_along(x_list)) {
    if (par_check_vec[i]) {
      par_check <- TRUE
      x_matrix <- chunk(x_list[[i]], number_of_threads() * 4)
      x_matrix <- lapply(x_matrix, function(x) {
        matrix(unlist(field(x, "signature")), ncol = vec_size(x))})
      y_matrix <- matrix(unlist(field(y_list[[i]], "signature")), 
                         ncol = vec_size(y_list[[i]]))
      result[[i]] <- suppressWarnings(par_lapply(x_matrix, stats::cor, 
                                                 y_matrix))
      result[[i]] <- do.call(rbind, result[[i]])
      pb(amount = vec_size(x_list[[i]]))
    } else {
      par_check <- FALSE
      x_matrix <- matrix(unlist(field(x_list[[i]], "signature")), 
                         ncol = vec_size(x_list[[i]]))
      y_matrix <- matrix(unlist(field(y_list[[i]], "signature")), 
                         ncol = vec_size(y_list[[i]]))
      result[[i]] <- suppressWarnings(stats::cor(x_matrix, y_matrix))
      pb(amount = vec_size(x_list[[i]]))
    }
  }
  
  # Return result
  get_ratios <- function(x) c(min(field(x, "aspect_ratio"), na.rm = TRUE), 
                              max(field(x, "aspect_ratio"), na.rm = TRUE))
  new_matrix(
    matrix = result,
    x_ratios = lapply(x_list, get_ratios),
    y_ratios = lapply(y_list, get_ratios),
    x_sig = x_sig,
    y_sig = y_sig,
    x_total = length(unique(c(field(x, "file"), field(x_na, "file")))),
    y_total = length(unique(c(field(y, "file"), field(y_na, "file")))),
    x_na = field(x_na, "file"),
    y_na = field(y_na, "file")
  )
  
}

# ------------------------------------------------------------------------------

get_clusters <- function(x, y, stretch = 1.2, max_clust = 10) {
  
  x_ratios <- field(x, "aspect_ratio")
  y_ratios <- field(y, "aspect_ratio")
  
  # Set number of groups to evaluate
  unique_points <- unique(stats::na.omit(c(x_ratios, y_ratios)))
  if (length(unique_points) < 4) return(list(list(x), list(y)))
  sum_fun <- function(x, y) {
    x_length <- as.numeric(length(x_ratios[x_ratios >= x & x_ratios <= y]))
    y_length <- as.numeric(length(y_ratios[y_ratios >= (x / stretch) & 
                                             y_ratios <= (y * stretch)]))
    x_length * y_length
  }
  set.seed(1)
  groups <- lapply(3:min(length(unique_points) - 1, max_clust), function(n) {
    cl <- stats::kmeans(x_ratios, n)
    mins <- sort(sapply(seq_len(n), function(n) min(x_ratios[cl$cluster == n])))
    mins[1] <- min(c(x_ratios, y_ratios))
    maxs <- sort(sapply(seq_len(n), function(n) max(x_ratios[cl$cluster == n])))
    maxs[n] <- max(c(x_ratios, y_ratios))
    calcs <- mapply(sum_fun, mins, maxs)
    list(max(calcs), mins, maxs)
  })
  
  # Choose k value which minimizes calculations
  k <- which.min(sapply(groups, `[[`, 1)) + 2
  k <- max(min(k, floor(vec_size(x) ^ (1/3)), floor(vec_size(y) ^ (1/3))), 3)
  cut_min <- groups[[k - 2]][[2]]
  cut_max <- groups[[k - 2]][[3]]
  
  # Collapse lists if any are empty
  zero_fun <- 
    function(x, y) c(sum(x_ratios >= x & x_ratios <= y), 
                     sum(y_ratios >= (x / stretch) & y_ratios <= (y * stretch)))
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

# ------------------------------------------------------------------------------

match_signatures_pairwise <- function(x, y, method = "colour", par_check = TRUE,
                                      quiet = FALSE) {
  
  # Prepare method
  if (method %in% c("grey", "gray", "greyscale", "grayscale")) {
    x <- trim_signature(x, 1:(sig_length(x) / 4)) 
    y <- trim_signature(y, 1:(sig_length(y) / 4)) 
  }
  
  if (method %in% c("colour", "color", "rgb", "RGB")) {
    x <- trim_signature(x, (sig_length(x) / 4 + 1):sig_length(x))
    y <- trim_signature(y, (sig_length(y) / 4 + 1):sig_length(y)) 
  }
  
  par_mapply(stats::cor, field(x, "signature"), field(y, "signature"), 
             SIMPLIFY = TRUE)
}
