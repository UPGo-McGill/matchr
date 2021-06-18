#' Get internal fields from matchr vectors
#' 
#' matchr vectors are implemented as "record"-style classes 
#' (using \code{vctrs::new_rcrd}), which store their data and metadata as a set 
#' of lists of equal length, called "fields". Usually these fields do not need 
#' to be accessed independently of the vectors they comprise. But, for the cases
#' when they do, matchr has a series of \code{get_*} functions which allow
#' convenient access to fields, and which are all straightforward wrappers of
#' \code{vctrs::field}. In each case, \code{get_*(x)} is equivalent to 
#' \code{vctrs::field(x, "*")}. New values can be assigned to these fields in
#' the same manner, e.g. \code{get_path(x) <- "new_path"}.
#' 
#' @aliases
#' get_path
#' get_array
#' get_signature
#' get_x_sig
#' get_y_sig
#' get_ar
#' get_x_ar
#' get_y_ar
#' 
#' @param x For \code{get_path} a \code{matchr_image} or \code{matchr_signature} 
#' vector; for \code{get_array} a \code{matchr_image} or \code{matchr_matrix}
#' vector; for \code{get_signature} and \code{get_ar} a \code{matchr_signature} 
#' vector; for \code{get_x_sig}, \code{get_y_sig}, \code{get_x_ar} and 
#' \code{get_y_ar} a \code{matchr_matrix} vector.
#' @return A list, with the same length as the input vector `x`, containing the
#' field `*` of the input vector corresponding to the `get_*` function which was
#' called.
#' @examples
#' \dontrun{
#' # Setup
#' img <- load_image(test_urls)
#' sig <- create_signature(img)
#' matches <- match_signatures(sig)
#' 
#' get_path(img)
#' get_path(sig)
#' get_array(img)
#' get_array(matches)
#' get_signature(sig)
#' get_x_sig(matches)
#' get_y_sig(matches)
#' get_ar(sig)
#' get_x_ar(matches)
#' get_y_ar(matches)
#' }

#' @name get_*
#' @export
get_path <- function(x) {
  stopifnot("x must be a matchr_image or matchr_signature vector" = 
              "path" %in% vctrs::fields(x))
  field(x, "path")
  }

#' @name get_*
#' @export
`get_path<-` <- function(x, value) {
  stopifnot("x must be a matchr_image or matchr_signature vector" = 
              "path" %in% vctrs::fields(x))
  vctrs::field(x, "path") <- value
  x
}

#' @name get_*
#' @export
get_array <- function(x) {
  stopifnot("x must be a matchr_image or matchr_matrix vector" = 
              "array" %in% vctrs::fields(x))
  field(x, "array")
  }

#' @name get_*
#' @export
`get_array<-` <- function(x, value) {
  stopifnot("x must be a matchr_image or matchr_matrix vector" = 
              "array" %in% vctrs::fields(x))
  field(x, "array") <- value
  x
}

#' @name get_*
#' @export
get_signature <- function(x) {
  stopifnot("x must be a matchr_signature vector" = 
              "signature" %in% vctrs::fields(x))
  field(x, "signature")
  }

#' @name get_*
#' @export
`get_signature<-` <- function(x, value) {
  stopifnot("x must be a matchr_signature vector" = 
              "signature" %in% vctrs::fields(x))
  field(x, "signature") <- value
  x
}

#' @name get_*
#' @export
get_x_sig <- function(x) {
  stopifnot("x must be a matchr_matrix vector" = "x_sig" %in% vctrs::fields(x))
  field(x, "x_sig")
  }

#' @name get_*
#' @export
`get_x_sig<-` <- function(x, value) {
  stopifnot("x must be a matchr_matrix vector" = "x_sig" %in% vctrs::fields(x))
  field(x, "x_sig") <- value
  x
}

#' @name get_*
#' @export
get_y_sig <- function(x) {
  stopifnot("x must be a matchr_matrix vector" = "y_sig" %in% vctrs::fields(x))
  field(x, "y_sig")
}

#' @name get_*
#' @export
`get_y_sig<-` <- function(x, value) {
  stopifnot("x must be a matchr_matrix vector" = "y_sig" %in% vctrs::fields(x))
  field(x, "y_sig") <- value
  x
}

#' @name get_*
#' @export
get_ar <- function(x) {
  stopifnot("x must be a matchr_signature vector" = "ar" %in% vctrs::fields(x))
  field(x, "ar")
  }

#' @name get_*
#' @export
`get_ar<-` <- function(x, value) {
  stopifnot("x must be a matchr_signature vector" = "ar" %in% vctrs::fields(x))
  field(x, "ar") <- value
  x
}

#' @name get_*
#' @export
get_x_ar <- function(x) {
  stopifnot("x must be a matchr_matrix vector" = "x_ar" %in% vctrs::fields(x))
  field(x, "x_ar")
  }

#' @name get_*
#' @export
`get_x_ar<-` <- function(x, value) {
  stopifnot("x must be a matchr_matrix vector" = "x_ar" %in% vctrs::fields(x))
  field(x, "x_ar") <- value
  x
}

#' @name get_*
#' @export
get_y_ar <- function(x) {
  stopifnot("x must be a matchr_matrix vector" = "y_ar" %in% vctrs::fields(x))
  field(x, "y_ar")
  }

#' @name get_*
#' @export
`get_y_ar<-` <- function(x, value) {
  stopifnot("x must be a matchr_matrix vector" = "y_ar" %in% vctrs::fields(x))
  field(x, "y_ar") <- value
  x
}


#' Get the signature length from a matchr_signature vector
#'
#' By default the raw image signatures in a matchr_signature vector have a 
#' length of 160 (20 horizontal and vertical bands of greyscale, red, green, and 
#' blue). But since the length can take any arbitrary value, \code{sig_length} 
#' offers a convenient way to access this value on a per-vector basis.
#'
#' @param x A matchr_signature vector.
#' @return An integer scalar.
#' @export

sig_length <- function(x) {
  
  vec_assert(x, new_signature())
  l <- unique(sapply(get_signature(x), length))
  if (length(l) == 2 && 1 %in% l) l <- setdiff(l, 1)
  stopifnot(length(l) == 1)
  l
}
