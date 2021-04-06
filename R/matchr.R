.matchr_env <- new.env()

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("sig_backup", "sig_hash", "match_backup",
                           "match_hash", "photos"))
}
