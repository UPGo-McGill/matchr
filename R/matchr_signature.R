#' Create a new matchr_signature object
#'
#' @param x A list of numeric vectors.
#' @param file A character string, corresponding to the path or URL of the files
#' from which the signatures have been generated.
#' @param aspect_ratio A numeric vector, giving the aspect ratio of the images.
#' @return An object of class `matchr_signature`.

new_signature <- function(x = list(), file = character(), 
                          aspect_ratio = numeric()) {
  vec_assert(x, list())
  vec_assert(file, character())
  vec_assert(aspect_ratio, numeric())
  new_rcrd(fields = list(signature = x, file = file, 
                         aspect_ratio = aspect_ratio), 
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
#' @export

is_signature <- function(x) {
  inherits(x, "matchr_signature")
}

# ------------------------------------------------------------------------------

#' @export

format.matchr_signature <- function(x, formatter = num_format, ...) {

  x_empty <- which(lengths(field(x, "signature")) == 0)
  x_valid <- setdiff(which(!is.na(x)), x_empty)
  values <- formatter(field(x[x_valid], "signature"))
    
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

  as.logical(sapply(lapply(field(x, "signature"), is.na), sum))

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
    bracket[x_valid] <- sprintf(' (a.r. %.2f)', field(x[x_valid], 
                                                      "aspect_ratio"))
    if (all(is.na(x))) sigs <- rep(pillar::style_na(NA), vec_size(x))
  
  # Next size: only aspect ratio
  } else if (bracket_chars < 25) {
    bracket <- rep("", vec_size(x))
    bracket[x_valid] <- sprintf(' (aspect ratio %.2f)', field(x[x_valid], 
                                                              "aspect_ratio"))
    if (all(is.na(x))) sigs <- rep(pillar::style_na(NA), vec_size(x))
    
  # Next size: a.r. plus path
  } else if (bracket_chars < 50) {
    file_max <- rep(bracket_chars - 2, vec_size(x))
    file_max[x_valid] <- bracket_chars - 13
    file_length <- nchar(field(x, "file"))
    file <- ifelse(
      file_length > file_max,
      paste0("...", substr(field(x, "file"), file_length - file_max + 4, 
                           file_length)),
      field(x, "file"))
    bracket <- sprintf(' (a.r. %.2f, %s)', 
                       field(x, "aspect_ratio"),
                       file)
    bracket[x_invalid] <- sprintf(' (%s)', file[x_invalid])
    if (all(is.na(x))) {
      sigs <- rep(pillar::style_na(NA), vec_size(x))
      file_max <- rep(bracket_chars + 18, vec_size(x))
      file_length <- nchar(field(x, "file"))
      file <- ifelse(
        file_length > file_max,
        paste0("...", substr(field(x, "file"), file_length - file_max + 4, 
                             file_length)),
        field(x, "file"))
      bracket <- sprintf(' (%s)', file)
      
    }
    
  # Final size: aspect ratio plus path
  } else {
    file_max <- rep(bracket_chars - 2, vec_size(x))
    file_max[x_valid] <- bracket_chars - 21
    file_length <- nchar(field(x, "file"))
    file <- ifelse(file_length > file_max, paste0(
      "...", substr(field(x, "file"), file_length - file_max + 4, file_length)),
      field(x, "file"))
    bracket <- sprintf(' (aspect ratio %.2f, %s)', 
                       field(x, "aspect_ratio"),
                       file)
    bracket[x_invalid] <- sprintf(' (%s)', file[x_invalid])
    bracket[x_invalid] <- 
      ifelse(nchar(bracket[x_invalid]) < bracket_chars + 1, paste0(
        strrep(" ", pmin(19, bracket_chars + 1 - nchar(bracket[x_invalid]))), 
        bracket[x_invalid]), bracket[x_invalid])
    if (all(is.na(x))) {
      sigs <- rep(pillar::style_na(NA), vec_size(x))
      file_max <- rep(bracket_chars + 18, vec_size(x))
      file_length <- nchar(field(x, "file"))
      file <- ifelse(
        file_length > file_max,
        paste0("...", substr(field(x, "file"), file_length - file_max + 4, 
                             file_length)),
        field(x, "file"))
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
  
  if (vec_size(x) == 1) plural <- " signature\n" else plural <- " signatures\n"
  header <- pillar::style_subtle(paste0('# An image signature vector: ', 
                                        prettyNum(length(x), ","), plural))
  cat(header)
  
}


# ------------------------------------------------------------------------------

#' @export

obj_print_footer.matchr_signature <- function(x, ...) {
  
  if (vec_size(x) > 20) {
    footer <- pillar::style_subtle(paste0("# \u2026 with ", vec_size(x) - 10, 
                                          " more signatures\n"))
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
  
  data <- field(x, "signature")
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
  sums <- lapply(args, field, "signature")
  sums <- lapply(sums, sapply, sum)
  sums <- unlist(sums)
  sums <- sum(sums, na.rm = na.rm)
  sums
 
}
