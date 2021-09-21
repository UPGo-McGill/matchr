#' Create a new matchr_signature object
#'
#' @param x A list of numeric vectors.
#' @param path A character string, corresponding to the path or URL of the files
#' from which the signatures have been generated.
#' @param ar A numeric vector, giving the aspect ratio of the images.
#' @return An object of class `matchr_signature_2`.

new_signature_2 <- function(x = list(), path = character(), ar = numeric()) {
  vec_assert(x, list())
  vec_assert(path, character())
  vec_assert(ar, numeric())
  new_rcrd(fields = list(hash = x, path = path, ar = ar), 
           class = "matchr_signature_2")
}

# ------------------------------------------------------------------------------

#' Test if the object is a matchr_signature
#' 
#' This function returns TRUE for `matchr_signature_2` objects and FALSE for all 
#' other objects.
#' 
#' @param x An object to test
#' @return A logical scalar, TRUE if `x` inherits from class "matchr_signature_2" 
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

is_signature_2 <- function(x) {
  inherits(x, "matchr_signature_2")
}

# ------------------------------------------------------------------------------

#' @export

format.matchr_signature_2 <- function(x, ...) {
  
  x_empty <- which(lengths(get_hash(x)) == 0)
  x_valid <- setdiff(which(!is.na(x)), x_empty)
  hashes <- field(x, "hash")
  hashes <- sapply(hashes, \(x) {
    # Exit early with NA or NULL
    if (is.logical(x)) return(NA_character_)
    if (length(x) == 0) return("NULL            ")
    
    # Otherwise construct hex text string out of binary inputs
    string <- vector("character", 4)
    for (i in 1:4) {
      str_x <-
        x[((i - 1) * 16 + 1):(i * 16)] |> 
        paste0(collapse = "") |> 
        strtoi(2) |> 
        as.hexmode() |> 
        as.character()
      insert <- paste0(rep("0", 4 - nchar(str_x)), collapse = "")
      string[[i]] <- paste0(insert, str_x)
    }
    string <- paste0(string, collapse = "")
  })
  hashes
  
}

# ------------------------------------------------------------------------------

#' @export

vec_ptype_abbr.matchr_signature_2 <- function(x, ...) "sig"

# ------------------------------------------------------------------------------

#' @export

is.na.matchr_signature_2 <- function(x, ...) {
  
  as.logical(sapply(lapply(get_hash(x), is.na), sum))
  
}

# ------------------------------------------------------------------------------

#' @export

obj_print_data.matchr_signature_2 <- function(x, width = getOption("width"), 
                                              ...) {

  # Setup
  if (vec_size(x) > 20) x <- x[1:10]
  x_valid <- which(!is.na(x))
  x_invalid <- which(is.na(x))

  # Leading number
  lead_n <- formatC(seq_along(x), width = nchar(vec_size(x)), format = "fg")
  lead_n <- pillar::style_subtle(lead_n)

  # Hashes
  sigs <- format(x)
  sigs[x_invalid] <- paste0(pillar::style_na(NA), strrep(" ", 14))

  # Smallest bracket: only sigs
  if (width <= 30) {
    bracket <- ""
    if (all(is.na(x))) sigs <- rep(pillar::style_na(NA), vec_size(x))

    # Next size: sigs + a.r.
  } else if (width <= 38) {
    bracket <- rep(" (a.r.   NA)", vec_size(x))
    bracket[x_valid] <- sprintf(' (a.r. %.2f)', get_ar(x[x_valid]))
    if (all(is.na(x))) {
      sigs <- rep(pillar::style_na(NA), vec_size(x))
      bracket <- rep(" (a.r. NA)", vec_size(x))}

    # Next size: sigs + aspect ratio
  } else if (width <= 45) {
    bracket <- rep(" (aspect ratio   NA)", vec_size(x))
    bracket[x_valid] <- sprintf(' (aspect ratio %.2f)', get_ar(x[x_valid]))
    if (all(is.na(x))) {
      sigs <- rep(pillar::style_na(NA), vec_size(x))
      bracket <- rep(" (aspect ratio NA)", vec_size(x))}

    # Next size: sigs + a.r. + path
  } else if (width <= 70) {
    file_max <- width - 31
    file_length <- nchar(get_path(x))
    file <- ifelse(
      file_length > file_max,
      paste0("\u2026", substr(get_path(x), file_length - file_max + 2,
                           file_length)),
      get_path(x))
    bracket <- sprintf(' (a.r. %.2f, %s)',
                       get_ar(x),
                       file)
    bracket[x_invalid] <- sprintf(' (a.r.   NA, %s)', file[x_invalid])
    if (all(is.na(x))) {
      sigs <- rep(pillar::style_na(NA), vec_size(x))
      file_max <- rep(width - 15, vec_size(x))
      file_length <- nchar(get_path(x))
      file <- ifelse(
        file_length > file_max,
        paste0("\u2026", substr(get_path(x), file_length - file_max + 4,
                             file_length)),
        get_path(x))
      bracket <- sprintf(' (a.r. NA, %s)', file)

    }

    # Final size: sigs + aspect ratio + path
  } else {
    file_max <- width - 37
    file_length <- nchar(get_path(x))
    file <- ifelse(file_length > file_max, paste0(
      "\u2026", substr(get_path(x), file_length - file_max + 4, file_length)),
      get_path(x))
    bracket <- sprintf(' (aspect ratio %.2f, %s)', get_ar(x), file)
    bracket[x_invalid] <- sprintf(' (aspect ratio   NA, %s)', file[x_invalid])
    if (all(is.na(x))) {
      sigs <- rep(pillar::style_na(NA), vec_size(x))
      file_max <- rep(width - 20, vec_size(x))
      file_length <- nchar(get_path(x))
      file <- ifelse(file_length > file_max, paste0(
        "\u2026", substr(get_path(x), file_length - file_max + 4, file_length)),
        get_path(x))
      bracket <- sprintf(' (aspect ratio NA, %s)', file)
    }
  }

  bracket <- pillar::style_subtle(bracket)

  # Return output
  cat(paste0(lead_n, " ", sigs, bracket), sep = "\n")
}

# ------------------------------------------------------------------------------

#' @export

obj_print_header.matchr_signature_2 <- function(x, ...) {

  if (vec_size(x) == 0) plural <- " signatures" else
    if (vec_size(x) == 1) plural <- " signature\n" else 
      plural <- " signatures\n"
  header <- paste0('# An image signature vector: ', 
                   prettyNum(length(x), ","), plural)
  header <- pillar::style_subtle(header)
  cat(header)

}

# ------------------------------------------------------------------------------

#' @export

obj_print_footer.matchr_signature_2 <- function(x, ...) {

  if (vec_size(x) > 20) {
    footer <- pillar::style_subtle(paste0(
      "# \u2026 with ", prettyNum(vec_size(x) - 10, ","), " more signatures\n"))
    cat(footer)
  }
}

# ------------------------------------------------------------------------------

#' @importFrom pillar pillar_shaft
#' @export

pillar_shaft.matchr_signature_2 <- function(x, ...) {
  x_valid <- which(!is.na(x))
  values <- rep(pillar::style_na(NA_character_), length(x))
  values[x_valid] <- format(x[x_valid])
  pillar::new_pillar_shaft(values, ..., width = 16, min_width = 4,
                           class = "pillar_shaft_signature_2")
}

# ------------------------------------------------------------------------------

#' @export

format.pillar_shaft_signature_2 <- function(x, width = width, ...) {
  if (width < 16) {
    x_valid <- which(!grepl("NA", x))
    x[x_valid] <- substr(x[x_valid], 1, width - 1)
    x[x_valid] <- paste0(x[x_valid], "\u2026")
  }
  pillar::new_ornament(x, align = "right")
}


# ------------------------------------------------------------------------------

#' @export

length.matchr_signature_2 <- function(x) vctrs::vec_size(x)


# ------------------------------------------------------------------------------

#' Plot a matchr_signature_2 vector
#' 
#' @param x Vector of class `matchr_signature_2`
#' @param max_plot Positive integer. The maximum number of signatures to plot at 
#' once (default 20). 
#' @param n_rows Either "auto" or a positive integer. The number of rows with 
#' which to plot images. If "auto", nine or fewer images will be arranged in an 
#' adaptive layout which maximizes legibility, while ten or more images will 
#' always be arranged in a four-column grid.
#' @param ... Not used.
#' @export

plot.matchr_signature_2 <- function(x, max_plot = 20, n_rows = "auto", ...) {
  
  # Check arguments
  stopifnot(is.numeric(max_plot))
  stopifnot(n_rows == "auto" || is.numeric(n_rows))
  
  # Exit early if there's nothing to plot
  if (sum(is.na(x)) == vctrs::vec_size(x)) {
    warning("No non-NA signatures to plot")
    return(invisible(x))
  }
  
  # Trim to the first {max_plot} valid signatures
  y <- x[!is.na(x)]
  if (sum(is.na(x)) > 0 || length(y) > max_plot) {
    message("Only the first ", max_plot, " valid signatures will be plotted.")
    y <- y[seq_len(min(max_plot, length(y)))]
  }
  
  # Create rasters
  rasters <- 
    y |> 
    get_hash() |> 
    lapply(matrix, nrow = 8) |> 
    lapply(grDevices::as.raster)
  
  
  # Plot function
  img_plot <- function(y, img) {
    plot(img)
    t <- sub("^.*/", "", get_path(y))
    graphics::title(t)
  }
  
  # Magic formula for grid cells
  n <- length(y)
  if (n_rows != "auto") {
    dims <- c(n_rows, ceiling(n / n_rows))
  } else dims <- c(ceiling(n / 4)  + n %in% c(3, 4, 7, 8), 
                   min(ceiling((n + 3) / 4) + (n == 5), 4))
  
  # Plot images
  old_par <- graphics::par(mfrow = dims, mai = c(0.25, 0.1, 0.25, 0.1))
  mapply(img_plot, y, rasters)
  graphics::par(old_par)
  invisible(x)
  
}

# ------------------------------------------------------------------------------

sig_img_2 <- function(x, img, colour) {
  
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
