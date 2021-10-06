#' Create a new matchr_signature object
#'
#' @param x A list of numeric vectors.
#' @param path A character string, corresponding to the path or URL of the files
#' from which the signatures have been generated.
#' @param ar A numeric vector, giving the aspect ratio of the images.
#' @return An object of class `matchr_signature`.

new_signature <- function(x = list(), path = character(), ar = numeric()) {
  vec_assert(x, list())
  vec_assert(path, character())
  vec_assert(ar, numeric())
  new_rcrd(fields = list(hash = x, path = path, ar = ar), 
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

format.matchr_signature <- function(x, ...) {
  
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

vec_ptype_abbr.matchr_signature <- function(x, ...) "sig"

# ------------------------------------------------------------------------------

#' @export

is.na.matchr_signature <- function(x, ...) {
  
  as.logical(sapply(lapply(get_hash(x), is.na), sum))
  
}

# ------------------------------------------------------------------------------

#' @export

obj_print_data.matchr_signature <- function(x, width = getOption("width"), 
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

obj_print_header.matchr_signature <- function(x, ...) {

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
  x_valid <- which(!is.na(x))
  values <- rep(pillar::style_na(NA_character_), length(x))
  values[x_valid] <- format(x[x_valid])
  pillar::new_pillar_shaft(values, ..., width = 16, min_width = 4,
                           class = "pillar_shaft_signature")
}

# ------------------------------------------------------------------------------

#' @export

format.pillar_shaft_signature <- function(x, width = width, ...) {
  if (width < 16) {
    x_valid <- which(!grepl("NA", x))
    x[x_valid] <- substr(x[x_valid], 1, width - 1)
    x[x_valid] <- paste0(x[x_valid], "\u2026")
  }
  pillar::new_ornament(x, align = "right")
}


# ------------------------------------------------------------------------------

#' @export

length.matchr_signature <- function(x) vctrs::vec_size(x)
