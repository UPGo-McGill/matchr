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
#' @param mem_scale A numeric scalar between 0 and 1. What portion of total
#' system memory should be made available for a single correlation matrix
#' calculation (default 0.2)? Increasing this value might speed up function
#' execution, but at the cost of significantly increased system instability.
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

match_signatures <- function(x, y = NULL, method = "grey", compare_ar = TRUE, 
                             stretch = 1.2, mem_scale = 0.2, quiet = FALSE) {
  
  # Error handling and object initialization
  stopifnot(is_signature(x), is.logical(c(compare_ar, quiet)),
            method %in% c("grey", "gray", "colour", "color", "rgb", "RGB", 
                          "both"))
  if (missing(y)) y <- x else stopifnot(is_signature(y))
  par_check <- TRUE
  
  # Prepare objects for processing
  output <- match_signatures_prep(x, y, method, compare_ar, stretch, mem_scale)
  x <- output[[1]]
  y <- output[[2]]
  x_na <- output[[3]]
  y_na <- output[[4]]
  x_list <- output[[5]]
  y_list <- output[[6]]
  x_sig <- output[[7]]
  y_sig <- output[[8]]
  rm(output)
    
  # Initialize progress reporting
  handler_matchr("Matching signature")
  prog_bar <- as.logical((vec_size(x) >= 5000) * as.numeric(!quiet) *
                           progressr::handlers(global = NA))
  pb <- progressr::progressor(steps = vec_size(x), enable = prog_bar)
  
  # Calculate correlation matrices
  result <- vector("list", length(x_list))
  for (i in seq_along(x_list)) {
    result[[i]] <- match_signatures_internal(x_list[[i]], y_list[[i]])
    pb(amount = sum(sapply(x_list[[i]], vec_size)))
    }
  
  # Return result
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

get_mem_limit <- function(x_list, y_list, mem_scale) {
  
  sys_mem <- memuse::Sys.meminfo()
  max_mem <- as.numeric(sys_mem$totalram * mem_scale) / 1e6
  out <- mapply(function(x, y) ceiling(7.89e-06 * x * y),
                as.numeric(lengths(x_list)), as.numeric(lengths(y_list)))
  out <- ceiling(out / max_mem)
  out
  
}

# ------------------------------------------------------------------------------

get_ratios <- function(x) c(min(field(x, "aspect_ratio"), na.rm = TRUE), 
                            max(field(x, "aspect_ratio"), na.rm = TRUE))

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

# ------------------------------------------------------------------------------

match_signatures_prep <- function(x, y, method, compare_ar, stretch, 
                                  mem_scale) {
  
  # Deal with NAs
  x_na <- x[is.na(x)]
  y_na <- y[is.na(y)]
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  # Split clusters for compare_ar == TRUE
  if (vec_size(x) < 10 || vec_size(y) < 10) compare_ar <- FALSE
  if (compare_ar == TRUE) {
    clusters <- get_clusters(x, y, stretch = stretch, max_clust = 8)
    x_list <- clusters[[1]]
    y_list <- clusters[[2]]
    x_sig <- clusters[[1]]
    y_sig <- clusters[[2]]
    rm(clusters)
  } else {
    x_list <- list(x)
    y_list <- list(y)
    x_sig <- list(x)
    y_sig <- list(y)
  }
  
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
  
  # Establish memory limits
  mem_limits <- get_mem_limit(x_list, y_list, mem_scale)
  
  # Split lists to stay within memory limits
  if (sum(mem_limits > 1) > 0) {
    x_list <- mapply(chunk, x_list, mem_limits)
    x_list <- unlist(x_list, recursive = FALSE)
    y_list <- mapply(rep, y_list, mem_limits)
    y_list <- mapply(chunk, y_list, mem_limits)
    y_list <- unlist(y_list, recursive = FALSE)
    x_sig <- mapply(chunk, x_sig, mem_limits)
    x_sig <- unlist(x_sig, recursive = FALSE)
    y_sig <- mapply(rep, y_sig, mem_limits)
    y_sig <- mapply(chunk, y_sig, mem_limits)
    y_sig <- unlist(y_sig, recursive = FALSE)
  }
  
  x_sig <- lapply(x_sig, chunk, number_of_threads() * 10)
  
  list(x, y, x_na, y_na, x_list, y_list, x_sig, y_sig)
  
}

# ------------------------------------------------------------------------------

match_signatures_internal <- function(x, y) {
  
  x_matrix <- chunk(x, number_of_threads() * 10)
  x_matrix <- lapply(x_matrix, function(x) {
    matrix(unlist(field(x, "signature")), ncol = vec_size(x))})
  y_matrix <- matrix(unlist(field(y, "signature")), ncol = vec_size(y))
  suppressWarnings(par_lapply(x_matrix, stats::cor, y_matrix))

}
  