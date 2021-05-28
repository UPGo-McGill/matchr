#' Get image signatures from a matchr_matrix vector
#'
#' TKTK
#'
#' TKTK
#'
#' @param x TKTK
#' @return TKTK
#' @export

get_x_sig <- function(x) field(x, "x_sig")
get_y_sig <- function(x) field(x, "y_sig")


#' Get raw signatures from a matchr_signature vector
#'
#' TKTK
#'
#' TKTK
#'
#' @param x TKTK
#' @return TKTK
#' @export

get_raw_sig <- function(x) field(x, "signature")



#' Get file paths from a matchr_image or matchr_signature vector
#'
#' TKTK
#'
#' TKTK
#'
#' @param x TKTK
#' @return TKTK
#' @export

get_path <- function(x) field(x, "path")


#' Get raw arrays from a matchr_image or matchr_matrix vector
#'
#' TKTK
#'
#' TKTK
#'
#' @param x TKTK
#' @return TKTK
#' @export

get_array <- function(x) field(x, "array")


#' Get aspect ratios from a matchr_signature or matchr_matrix vector
#'
#' TKTK
#'
#' TKTK
#'
#' @param x TKTK
#' @return TKTK
#' @aliases
#' get_ar
#' get_x_ar
#' get_y_ar

#' @export

get_ar <- function(x) field(x, "aspect_ratio")
get_x_ar <- function(x) field(x, "x_ratios")
get_y_ar <- function(x) field(x, "y_ratios")



#' Get the signature length from a matchr_signature vector
#'
#' TKTK
#'
#' TKTK
#'
#' @param x TKTK
#' @return TKTK
#' @export

sig_length <- function(x) {
  
  vec_assert(x, new_signature())
  l <- unique(sapply(field(x, "signature"), length))
  if (length(l) == 2 && 1 %in% l) l <- setdiff(l, 1)
  stopifnot(length(l) == 1)
  l
}

