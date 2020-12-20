#' Remove matchr function backups
#'
#' \code{remove_backups} manually removes the incremental backups which several
#' matchr functions keep in case of errors.
#'
#' The \code{\link{create_signature}} and \code{\link{match_signatures}}
#' functions can create incremental backups of their progress in case they fail
#' to complete. These backups are automatically removed once the function
#' completes, but \code{remove_backups} can be used to remove them manually
#' when necessary.
#'
#' @return A character vector of the backups removed, invisibly.
#' @export

remove_backups <- function() {

  removed <- character()

  if (exists("sig_backup", envir = .matchr_env)) {
    rm("sig_backup", envir = .matchr_env)
    removed <- c(removed, "sig_backup")
    }

  if (exists("sig_backup_size", envir = .matchr_env)) {
    rm("sig_backup_size", envir = .matchr_env)
    removed <- c(removed, "sig_backup_size")
  }

  if (exists("match_backup", envir = .matchr_env)) {
    rm("match_backup", envir = .matchr_env)
    removed <- c(removed, "match_backup")
  }

  if (exists("match_backup_size", envir = .matchr_env)) {
    rm("match_backup_size", envir = .matchr_env)
    removed <- c(removed, "match_backup_size")
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
