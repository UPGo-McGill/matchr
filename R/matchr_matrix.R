#' Create a new matchr_matrix object
#'
#' @param matrix A list of correlation matrices.
#' @param x_ratios A list of numeric vectors: the highest and lowest aspect 
#' ratio in the x vector.
#' @param y_ratios A list of numeric vectors: the highest and lowest aspect 
#' ratio in the y vector.
#' @param x_files A list of character vectors: the paths or URLs of the files 
#' in the x vector.
#' @param y_files A list of character vectors: the paths or URLs of the files 
#' in the y vector.
#' @param x_total An integer scalar: the total number of x signatures analyzed.
#' @param y_total An integer scalar: the total number of y signatures analyzed.
#' @param x_na A character vector: the paths of x signatures which are NA.
#' @param y_na A character vector: the paths of y signatures which are NA.
#' @return An object of class `matchr_matrix`.

new_matrix <- function(matrix = list(), x_ratios = list(), y_ratios = list(),
                       x_files = list(), y_files = list(), 
                       x_total = integer(length = 1L), 
                       y_total = integer(length = 1L),
                       x_na = character(), y_na = character()) {
  vec_assert(matrix, list())
  vec_assert(x_ratios, list())
  vec_assert(y_ratios, list())
  vec_assert(x_files, list())
  vec_assert(y_files, list())
  vec_assert(x_total, integer())
  vec_assert(y_total, integer())
  vec_assert(x_na, character())
  vec_assert(y_na, character())
  new_rcrd(fields = list(matrix = matrix, x_ratios = x_ratios, 
                         y_ratios = y_ratios, x_files = x_files, 
                         y_files = y_files), 
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
#' @export

is_matrix <- function(x) {
  inherits(x, "matchr_matrix")
}

# ------------------------------------------------------------------------------

#' @export

format.matchr_matrix <- function(x, ...) {
  
  dims <- paste(lengths(field(x, "x_files")), "x", lengths(field(x, "y_files")))
  dims
  
}

# ------------------------------------------------------------------------------

#' @export

vec_ptype_abbr.matchr_matrix <- function(x, ...) {
  "matrix"
}

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
  ratios <- field(x, "x_ratios")
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
                                          length(attr(x, "y_na")), " NAs"))
    cat(footer)
  } 
}

# ------------------------------------------------------------------------------

#' @export
#' 
vec_restore.matchr_matrix <- function(x, to, ..., n = NULL) {
  
  new_matrix(
    matrix = field(x, "matrix"),
    x_ratios = field(x, "x_ratios"),
    y_ratios = field(x, "y_ratios"),
    x_files = field(x, "x_files"),
    y_files = field(x, "y_files"),
    x_total = sum(lengths(field(x, "x_files"))) + vec_size(attr(to, "x_na")),
    y_total = sum(lengths(field(x, "y_files"))) + vec_size(attr(to, "y_na")),
    x_na = attr(to, "x_na"),
    y_na = attr(to, "y_na")
  )
  
}



#' # ------------------------------------------------------------------------------
#' 
#' #' export
#' is.na.matchr_image <- function(x, ...) is.na(field(x, "array"))
#' 
