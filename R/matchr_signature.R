#' Create a new matchr_signature object
#'
#' @param x A list of numeric vectors.
#' @param path A character string, corresponding to the path or URL of the files
#' from which the signatures have been generated.
#' @param ar A numeric vector, giving the aspect ratio of the images.
#' @return An object of class `matchr_signature`.

new_signature <- function(x = list(), path = character(), 
                          ar = numeric()) {
  vec_assert(x, list())
  vec_assert(path, character())
  vec_assert(ar, numeric())
  new_rcrd(fields = list(signature = x, path = path, ar = ar), 
           class = "matchr_signature")
}

# ------------------------------------------------------------------------------

#' Test if the object is a matchr_signature
#' 
#' This function returns TRUE for `matchr_signature` objects and FALSE for all 
#' other objects.
#' 
#' @param x An object to test
#' @return A logical scalar, TRUE if `x` inherits from class "matchr_signature" 
#' and FALSE otherwise.
#' @examples
#' \dontrun{
#' # Setup
#' sigs <- create_signature(test_urls)
#' 
#' # TRUE
#' is_signature(sigs)
#' 
#' # FALSE
#' is_signature("text")
#' }
#' @export

is_signature <- function(x) {
  inherits(x, "matchr_signature")
}

# ------------------------------------------------------------------------------

#' @export

format.matchr_signature <- function(x, formatter = num_format, ...) {

  x_empty <- which(lengths(get_signature(x)) == 0)
  x_valid <- setdiff(which(!is.na(x)), x_empty)
  values <- formatter(get_signature(x[x_valid]))
    
  out <- rep(NA_character_, vec_size(x))
  out[x_empty] <- rep("<Empty>", vec_size(x_empty))
  out[x_valid] <- values
  out
  
}

num_format <- function(x) {
  
  values <- lapply(x, chunk, 4)
  values <- lapply(values, lapply, mean)
  values <- lapply(values, unlist)
  values <- lapply(values, function(x) sprintf("%.2f", x))
  values <- sapply(values, paste0, collapse = ", ")
  values
  
}

col_format <- function(x, width = 22) {
  
  grey_len <- floor(width / 4) + width %% 4
  col_len <- floor(width / 4)
  
  g_values <- x
  g_values <- lapply(g_values, function(x) x[1:(length(x) / 4)])
  g_values <- lapply(g_values, chunk, grey_len)
  g_values <- lapply(g_values, sapply, mean)
  g_values <- lapply(g_values, function(x) c(x, x, x))
  g_values <- lapply(g_values, matrix, ncol = 3)
  g_values <- lapply(g_values, grDevices::rgb)
  g_values <- lapply(g_values, sapply, function(x) 
    crayon::style("\u25a0", crayon::make_style(x, colors = 256)), 
    USE.NAMES = FALSE)
  g_values <- sapply(g_values, paste, collapse = "")
  
  c_values <- x
  c_values <- lapply(c_values, function(x) x[(length(x) / 4 + 1):length(x)])
  c_values <- lapply(c_values, chunk, col_len * 3)
  c_values <- lapply(c_values, sapply, mean)
  c_values <- lapply(c_values, function(x) 
    c(x[1:col_len], rep(0, 3 * col_len), x[(col_len + 1):(2 * col_len)], 
      rep(0, 3 * col_len), x[(2 * col_len + 1):(3 * col_len)]))
  c_values <- lapply(c_values, matrix, ncol = 3)
  c_values <- lapply(c_values, grDevices::rgb)
  c_values <- lapply(c_values, sapply, function(x) 
    crayon::style("\u25a0", crayon::make_style(x, colors = 256)), 
    USE.NAMES = FALSE)
  c_values <- sapply(c_values, paste, collapse = "")
  
  values <- paste0(g_values, c_values)
  values
  
}

# ------------------------------------------------------------------------------

#' @export

vec_ptype_abbr.matchr_signature <- function(x, ...) "sig"

# ------------------------------------------------------------------------------

#' @export

is.na.matchr_signature <- function(x, ...) {

  as.logical(sapply(lapply(get_signature(x), is.na), sum))

}

# ------------------------------------------------------------------------------

#' @export

obj_print_data.matchr_signature <- function(x, width = getOption("width"), ...) {
  
  # Setup
  if (vec_size(x) > 20) x <- x[1:10]
  x_valid <- which(!is.na(x))
  x_invalid <- which(is.na(x))
  bracket_chars <- width - nchar(vec_size(x)) - 24
  
  # Leading number
  lead_n <- formatC(seq_along(x), width = nchar(vec_size(x)), format = "fg")
  lead_n <- pillar::style_subtle(lead_n)
  
  # Image signature
  if (requireNamespace("crayon", quietly = TRUE) && crayon::has_color()) {
   sigs <- format(x, col_format)
  } else sigs <- format(x)
  sigs[x_invalid] <- paste0(pillar::style_na(NA), strrep(" ", 20))
  
  # Smallest bracket: only sigs
  if (bracket_chars < 11) {
    bracket <- ""
    if (all(is.na(x))) sigs <- rep(pillar::style_na(NA), vec_size(x))
  
  # Next size: only a.r.
  } else if (bracket_chars < 19) {
    bracket <- rep("", vec_size(x))
    bracket[x_valid] <- sprintf(' (a.r. %.2f)', get_ar(x[x_valid]))
    if (all(is.na(x))) sigs <- rep(pillar::style_na(NA), vec_size(x))
  
  # Next size: only aspect ratio
  } else if (bracket_chars < 25) {
    bracket <- rep("", vec_size(x))
    bracket[x_valid] <- sprintf(' (aspect ratio %.2f)', get_ar(x[x_valid]))
    if (all(is.na(x))) sigs <- rep(pillar::style_na(NA), vec_size(x))
    
  # Next size: a.r. plus path
  } else if (bracket_chars < 50) {
    file_max <- rep(bracket_chars - 2, vec_size(x))
    file_max[x_valid] <- bracket_chars - 13
    file_length <- nchar(get_path(x))
    file <- ifelse(
      file_length > file_max,
      paste0("...", substr(get_path(x), file_length - file_max + 4, 
                           file_length)),
      get_path(x))
    bracket <- sprintf(' (a.r. %.2f, %s)', 
                       get_ar(x),
                       file)
    bracket[x_invalid] <- sprintf(' (%s)', file[x_invalid])
    if (all(is.na(x))) {
      sigs <- rep(pillar::style_na(NA), vec_size(x))
      file_max <- rep(bracket_chars + 18, vec_size(x))
      file_length <- nchar(get_path(x))
      file <- ifelse(
        file_length > file_max,
        paste0("...", substr(get_path(x), file_length - file_max + 4, 
                             file_length)),
        get_path(x))
      bracket <- sprintf(' (%s)', file)
      
    }
    
  # Final size: aspect ratio plus path
  } else {
    file_max <- rep(bracket_chars - 2, vec_size(x))
    file_max[x_valid] <- bracket_chars - 21
    file_length <- nchar(get_path(x))
    file <- ifelse(file_length > file_max, paste0(
      "...", substr(get_path(x), file_length - file_max + 4, file_length)),
      get_path(x))
    bracket <- sprintf(' (aspect ratio %.2f, %s)', get_ar(x), file)
    bracket[x_invalid] <- sprintf(' (%s)', file[x_invalid])
    bracket[x_invalid] <- 
      ifelse(nchar(bracket[x_invalid]) < bracket_chars + 1, paste0(
        strrep(" ", pmin(19, bracket_chars + 1 - nchar(bracket[x_invalid]))), 
        bracket[x_invalid]), bracket[x_invalid])
    if (all(is.na(x))) {
      sigs <- rep(pillar::style_na(NA), vec_size(x))
      file_max <- rep(bracket_chars + 18, vec_size(x))
      file_length <- nchar(get_path(x))
      file <- ifelse(
        file_length > file_max,
        paste0("...", substr(get_path(x), file_length - file_max + 4, 
                             file_length)),
        get_path(x))
      bracket <- sprintf(' (%s)', file)
    }
  }

  bracket <- pillar::style_subtle(bracket)
  
  # Return output
  cat(paste0(lead_n, " ", sigs, bracket), sep = "\n")
}

# ------------------------------------------------------------------------------

#' @export

obj_print_header.matchr_signature <- function(x, ...) {
  
  if (vec_size(x) == 1) plural <- " signature" else plural <- " signatures"
  first_part <- paste0('# An image signature vector: ', 
                       prettyNum(length(x), ","), plural)
  if (nchar(first_part) + 11 < getOption("width")) {
    bands <- sig_length(x) / 8
    bands <- paste0(" (", bands, " bands)\n")  
  } else bands <- "\n"
  
  header <- pillar::style_subtle(paste0(first_part, bands))
  cat(header)
  
}

# ------------------------------------------------------------------------------

#' @export

obj_print_footer.matchr_signature <- function(x, ...) {
  
  if (vec_size(x) > 20) {
    footer <- pillar::style_subtle(paste0(
      "# \u2026 with ", prettyNum(vec_size(x) - 10, ","), " more signatures\n"))
    cat(footer)
  } 
}

# ------------------------------------------------------------------------------

#' @importFrom pillar pillar_shaft
#' @export

pillar_shaft.matchr_signature <- function(x, ...) {

  pillar::new_pillar_shaft(x, ..., width = 22, min_width = 4, 
                           class = "pillar_shaft_signature")
}

# ------------------------------------------------------------------------------

#' @export

format.pillar_shaft_signature <- function(x, width, ...) {
  
  data <- get_signature(x)
  x_valid <- which(!is.na(data))
  
  if (requireNamespace("crayon", quietly = TRUE) && crayon::has_color()) {
    
    values <- rep(pillar::style_na(NA_character_), length(data))
    values[x_valid] <- col_format(data[x_valid], width = width)
    
  } else {
    
    values <- rep(pillar::style_na(NA_character_), length(data))
    values[x_valid] <- strrep("\u25a0", width)
    
  }
  
  pillar::new_ornament(values, align = "right")
}

# ------------------------------------------------------------------------------

#' @export

sum.matchr_signature <- function(..., na.rm = FALSE) {
  
  args <- list(...)
  sums <- lapply(args, get_signature)
  sums <- lapply(sums, sapply, sum)
  sums <- unlist(sums)
  sums <- sum(sums, na.rm = na.rm)
  sums
 
}

# ------------------------------------------------------------------------------

#' @export

length.matchr_signature <- function(x) vctrs::vec_size(x)


# ------------------------------------------------------------------------------

#' @export

c.matchr_signature <- function(..., recursive = FALSE, use.names = TRUE) {
  if (recursive) {
    stop("`recursive` must be `FALSE` when concatenating matchr classes.")
  }
  if (!use.names) {
    stop("`use.names` must be `TRUE` when concatenating matchr classes.")
  }
  args <- list(...)
  sig_lengths <- sapply(args, sig_length)
  if(length(unique(sig_lengths)) != 1) {
    stop("Signatures must have the same number of bands to be concatenated.")
  }
  vctrs::vec_c(...)
}

# ------------------------------------------------------------------------------

#' Plot a matchr_signature vector
#' 
#' @param x Vector of class `matchr_signature`
#' @param max_plot Positive integer. The maximum number of signatures to plot at 
#' once. 
#' @param with_image Logical scalar. Should the underlying image be plotted
#' alongside its signature (default TRUE)? If TRUE, \code{\link{load_image}} 
#' will be called on the paths in `x`.
#' @param ... Not used.
#' @export

plot.matchr_signature <- function(x, max_plot = 4, with_image = TRUE, ...) {
 
  # Check arguments
  stopifnot(is.numeric(max_plot), is.logical(with_image))
  
  # Exit early if there's nothing to plot
  if (sum(is.na(x)) == vctrs::vec_size(x)) {
    warning("No non-NA signatures to plot")
    return(invisible(x))
  }
  
  # Trim to the first {max_plot} valid images
  y <- x[!is.na(x)]
  if (sum(is.na(x)) > 0 || length(y) > max_plot) {
    message("Only the first ", max_plot, " valid signatures will be plotted.")
    y <- y[seq_len(min(max_plot, length(y)))]
  }
  
  # Load images if with_image = TRUE
  if (with_image) {
    img <- suppressWarnings(load_image(get_path(y)))
    # Replace NAs with white 10x10 squares
    NAs <- which(is.na(img))
    if (length(NAs) > 0) {
      warning(length(NAs), " image", if (length(NAs) > 1) "s", 
              " could not be loaded.")
      get_array(img[is.na(img)]) <- rep(list(array(
        rep(1, 300), dim = c(10, 10, 3))),  length(img[is.na(img)]))
      }
    } else img <- NULL
  
  if (length(y) == 1) {
    max_plot <- 9
    n_rows <- 3
    out <- sig_img(y, img, TRUE)
  } else {
    max_plot <- length(y) * 3
    n_rows <- length(y)
    out <- do.call(c, mapply(sig_img, y, img, MoreArgs = list(colour = FALSE), 
                             SIMPLIFY = FALSE))
  }
  
  plot(out, max_plot = max_plot, n_rows = n_rows)
  return(invisible(x))
  
}
  
# ------------------------------------------------------------------------------

sig_img <- function(x, img, colour) {
  
  bands <- sig_length(x) / 8
  h_scale <- round(bands * get_ar(x))
  v_scale <- round(bands / get_ar(x))
  
  # Process greyscale
  grey_h <- get_signature(x)[[1]][seq_len(bands)]
  grey_v <- get_signature(x)[[1]][(bands + 1):(bands * 2)]
  grey_h_img <- new_image(
    x = list(array(rep(grey_h, h_scale * 3), dim = c(bands, h_scale, 3))),
    path = "grey horizontal")
  grey_v_img <- new_image(
    x = list(array(c(
      do.call(rbind, rep(list(grey_v), v_scale)),
      do.call(rbind, rep(list(grey_v), v_scale)),
      do.call(rbind, rep(list(grey_v), v_scale))), dim = c(v_scale, bands, 3))),
    path = "grey vertical")
  out <- c(grey_h_img, grey_v_img)
  
  # Add img
  if (is.null(img)) img <- new_image(x = rep(list(
    array(rep(1, 300), dim = c(10, 10, 3))), length(img[is.na(img)])),
    path = get_path(x))
  out <- c(img, grey_h_img, grey_v_img)
  
  # Process colour
  if (colour) {
    # Red
    r_h <- get_signature(x)[[1]][(bands * 2 + 1):(bands * 3)]
    r_v <- get_signature(x)[[1]][(bands * 3 + 1):(bands * 4)]
    r_h_img <- new_image(
      x = list(array(c(do.call(cbind, rep(list(r_h), h_scale)), rep(
        0, bands * h_scale * 2)), dim = c(bands, h_scale, 3))),
      path = "red horizontal")
    r_v_img <- new_image(
      x = list(array(c(do.call(rbind, rep(list(r_v), v_scale)), rep(
        0, bands * v_scale * 2)), dim = c(v_scale, bands, 3))),
      path = "red vertical")
    
    # Green
    g_h <- get_signature(x)[[1]][(bands * 4 + 1):(bands * 5)]
    g_v <- get_signature(x)[[1]][(bands * 5 + 1):(bands * 6)]
    g_h_img <- new_image(
      x = list(array(c(rep(0, bands * h_scale), 
                       do.call(cbind, rep(list(g_h), h_scale)), 
                       rep(0, bands * h_scale)), dim = c(bands, h_scale, 3))),
      path = "green horizontal")
    g_v_img <- new_image(
      x = list(array(c(rep(0, bands * v_scale), 
                       do.call(rbind, rep(list(g_v), v_scale)), 
                       rep(0, bands * v_scale)), dim = c(v_scale, bands, 3))),
      path = "green vertical")
    
    # Blue
    b_h <- get_signature(x)[[1]][(bands * 6 + 1):(bands * 7)]
    b_v <- get_signature(x)[[1]][(bands * 7 + 1):(bands * 8)]
    b_h_img <- new_image(
      x = list(array(c(rep(0, bands * h_scale * 2), 
                       do.call(cbind, rep(list(b_h), h_scale))),
                     dim = c(bands, h_scale, 3))),
      path = "blue horizontal")
    b_v_img <- new_image(
      x = list(array(c(rep(0, bands * v_scale * 2), 
                       do.call(rbind, rep(list(b_v), v_scale))), 
                     dim = c(v_scale, bands, 3))),
      path = "blue vertical")
    
    out <- c(out, r_h_img, r_v_img, g_h_img, g_v_img, b_h_img, b_v_img)
    
  }
  
  out
  
}
