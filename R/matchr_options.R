#' Options used in matchr
#' 
#' Below are the R options and environment variables that are used by the matchr 
#' package.
#' 
#' @section matchr options:
#'
#' \describe{
#'  \item{\option{matchr.blas}:}{A logical scalar. Does the system have a
#'  high-performance BLAS library installed? If not, 
#'  \code{\link{match_signatures}} will use a different method which does not
#'  rely on fast BLAS calculations. (Default: `TRUE`)}
#'  }
#'  
#' \describe{
#'  \item{\option{matchr.force_parallel}:}{A logical scalar. Should matchr
#'  functions always use multithreaded {future} plans, even if previous
#'  testing suggests that single-threaded (sequential) plans will be faster for
#'  the workload? (Default: `FALSE`)}
#'  }
#'
#' @aliases
#' matchr.options
#' matchr.blas
#' matchr.force_parallel
#' 
#' @name matchr_options
NULL
