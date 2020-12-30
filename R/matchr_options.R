#' Options used in matchr
#' 
#' Below are the R options and environment variables that are used by the matchr 
#' package.
#' 
#' @section matchr options:
#'
#' \describe{
#'  \item{\option{matchr.force_parallel}:}{A logical scalar. Default future 
#'  strategy plan used unless otherwise specified via . This will also 
#'  be the future plan set when calling `plan("default")`.  If not specified, 
#'  this option may be set when the \pkg{future} package is _loaded_ if 
#'  command-line option `--parallel=ncores` (short `-p ncores`) is specified; if
#'   `ncores > 1`, then option future.plan is set to `multisession` 
#'   otherwise `sequential` (in addition to option \option{mc.cores} being set 
#'   to `ncores`, if `ncores >= 1`).  If system environment variable 
#'   \env{R_FUTURE_PLAN} is set, then that overrides the future plan set by the 
#'   command-line option. (Default: `sequential`)}
#'  }
#'
#' @aliases
#' matchr.options
#' matchr.force_parallel
#' 
#' @name matchr_options
NULL
