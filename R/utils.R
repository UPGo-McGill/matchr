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
#' get_hash
#' get_ahash
#' get_signature
#' get_x_sig
#' get_y_sig
#' get_ar
#' get_x_ar
#' get_y_ar
#' 
#' @param x For \code{get_path} a \code{matchr_image} or \code{matchr_signature} 
#' vector; for \code{get_array} a \code{matchr_image} or \code{matchr_matrix}
#' vector; for \code{get_hash}, \code{get_ahash} and \code{get_ar} a 
#' \code{matchr_signature} vector; for \code{get_x_sig}, \code{get_y_sig}, 
#' \code{get_x_ar} and \code{get_y_ar} a \code{matchr_matrix} vector.
#' @param value A new vector of the same type as the field it is replacing.
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
#' get_hash(sig)
#' get_ahash(sig)
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
get_hash <- function(x) {
  stopifnot("x must be a matchr_signature vector" = 
              "hash" %in% vctrs::fields(x))
  field(x, "hash")
}

#' @name get_*
#' @export
`get_hash<-` <- function(x, value) {
  stopifnot("x must be a matchr_signature vector" = 
              "hash" %in% vctrs::fields(x))
  field(x, "hash") <- value
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
