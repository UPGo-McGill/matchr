.matchr_env <- new.env()

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("sig_backup", "sig_backup_size", "match_backup",
                           "match_backup_size"))
}
