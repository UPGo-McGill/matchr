#' Remove matchr function backups
#'
#' \code{remove_backups} manually removes the incremental backups which several
#' matchr functions keep in case of errors.
#'
#' The \code{\link{create_signature}} functions can create incremental backups 
#' of its progress in case it fails to complete. These backups are automatically 
#' removed once the function completes, but \code{remove_backups} can be used to 
#' remove them manually when necessary.
#'
#' @return A character vector of the backups removed, invisibly.
#' @export

remove_backups <- function() {

  removed <- character()

  if (exists("sig_backup", envir = .matchr_env)) {
    rm("sig_backup", envir = .matchr_env)
    removed <- c(removed, "sig_backup")
    }

  if (exists("sig_hash", envir = .matchr_env)) {
    rm("sig_hash", envir = .matchr_env)
    removed <- c(removed, "sig_hash")
  }

  if (length(removed) == 0) {
    message("No backups found.")
  } else {

    message("The following backups were removed: ",
            paste(removed, collapse = ", "),
            ".")

  }

  return(invisible(removed))

}
