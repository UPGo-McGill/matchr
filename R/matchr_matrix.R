#' Create a new matchr_matrix object
#'
#' @param array A list of correlation matrices.
#' @param x_ar A list of numeric vectors: the highest and lowest aspect ratio in 
#' the x vector.
#' @param y_ar A list of numeric vectors: the highest and lowest aspect ratio in 
#' the y vector.
#' @param x_sig A list of `matchr_signature` vectors: the signatures of the 
#' files in the x vector.
#' @param y_sig A list of `matchr_signature` vectors: the signatures of the 
#' files in the y vector.
#' @param x_total An integer scalar: the total number of x signatures analyzed.
#' @param y_total An integer scalar: the total number of y signatures analyzed.
#' @param x_na A character vector: the paths of x signatures which are NA.
#' @param y_na A character vector: the paths of y signatures which are NA.
#' @return An object of class `matchr_matrix`.

new_matrix <- function(array = list(), x_ar = list(), y_ar = list(),
                       x_sig = list(), y_sig = list(), 
                       x_total = integer(length = 1L), 
                       y_total = integer(length = 1L),
                       x_na = character(), y_na = character()) {
  vec_assert(array, list())
  vec_assert(x_ar, list())
  vec_assert(y_ar, list())
  vec_assert(x_sig, list())
  vec_assert(y_sig, list())
  vec_assert(x_total, integer())
  vec_assert(y_total, integer())
  vec_assert(x_na, character())
  vec_assert(y_na, character())
  new_rcrd(fields = list(array = array, x_ar = x_ar, y_ar = y_ar, x_sig = x_sig, 
                         y_sig = y_sig), 
           x_total = x_total, y_total = y_total, x_na = x_na, y_na = y_na,
           class = "matchr_matrix")
}

# ------------------------------------------------------------------------------

#' Test if the object is a matchr_matrix
#' 
#' This function returns TRUE for `matchr_matrix` objects and FALSE for all 
#' other objects.
#' 
#' @param x An object to test
#' @return A logical scalar, TRUE if `x` inherits from class "matchr_matrix" and 
#' FALSE otherwise.
#' @examples
#' \dontrun{
#' # Setup
#' sigs <- create_signature(test_urls)
#' matches <- match_signatures(sigs)
#' 
#' # TRUE
#' is_matrix(matches)
#' 
#' # FALSE
#' is_matrix("text")
#' }
#' @export

is_matrix <- function(x) inherits(x, "matchr_matrix")

# ------------------------------------------------------------------------------

#' @export

format.matchr_matrix <- function(x, ...) {
  
  paste(prettyNum(lengths(get_x_sig(x)), ","), "x", 
        prettyNum(lengths(get_y_sig(x)), ","))
  
}

# ------------------------------------------------------------------------------

#' @export

vec_ptype_abbr.matchr_matrix <- function(x, ...) "matrix"

# ------------------------------------------------------------------------------

#' @export

obj_print_header.matchr_matrix <- function(x, ...) {
  
  if (vec_size(x) > 1) plural <- " matrices\n" else plural <- " matrix\n"
  header <- paste0(
    '# An image matrix vector: ', 
    prettyNum(attr(x, "x_total"), ","), " x ",
    prettyNum(attr(x, "y_total"), ","), " in ",
    vec_size(x), plural)
  header <- pillar::style_subtle(header)
  cat(header)
  
}

# ------------------------------------------------------------------------------

#' @export

obj_print_data.matchr_matrix <- function(x, width = getOption("width"), ...) {
  
  # Leading number
  lead_n <- formatC(seq_along(x), width = nchar(vec_size(x)), format = "fg")
  lead_n <- pillar::style_subtle(lead_n)
  
  # Aspect ratios
  ratios <- get_x_ar(x)
  ratios <- sapply(ratios, function(x) 
    sprintf(" Aspect ratios %4.2f - %4.2f: ", x[1], x[2]))
  
  # Dimensions
  dims <- format(x)
  
  # Return output
  cat(paste0(lead_n, ratios, dims), sep = "\n")
}

# ------------------------------------------------------------------------------

#' @export

obj_print_footer.matchr_matrix <- function(x, ...) {
  
  if (sum(length(attr(x, "x_na")), length(attr(x, "y_na"))) > 0) {
    footer <- pillar::style_subtle(paste0("# \u2026 with ", 
                                          length(attr(x, "x_na")), " x ",
                                          length(attr(x, "y_na")), " NAs\n"))
    cat(footer)
  } 
}

# ------------------------------------------------------------------------------

#' @export
#' 
vec_restore.matchr_matrix <- function(x, to, ..., n = NULL) {
  
  new_matrix(
    array = get_array(x),
    x_ar = get_x_ar(x),
    y_ar = get_y_ar(x),
    x_sig = get_x_sig(x),
    y_sig = get_y_sig(x),
    x_total = length(unique(c(get_path(do.call("c", get_x_sig(x))), 
                              attr(to, "x_na")))),
    y_total = length(unique(c(get_path(do.call("c", get_y_sig(x))), 
                              attr(to, "y_na")))),
    x_na = attr(to, "x_na"),
    y_na = attr(to, "y_na")
  )
  
}

# ------------------------------------------------------------------------------

#' @export
#' 

as.matrix.matchr_matrix <- function(x, ...) {
  if (vec_size(x) > 1) warning("Only the first element will be converted.", 
                               call. = FALSE)
  out <- get_array(x)[[1]]
  dimnames(out) <- list(
    get_path(get_x_sig(x)[[1]]),
    get_path(get_y_sig(x)[[1]])
  )
  out
}

# ------------------------------------------------------------------------------

#' @export

length.matchr_matrix <- function(x) vctrs::vec_size(x)
