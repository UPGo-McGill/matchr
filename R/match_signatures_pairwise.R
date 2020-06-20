match_signatures_pairwise <- function(x, y, quiet = FALSE) {

  mapply(function(x, y) {

    stats::cor(x, y)

  }, x, y, SIMPLIFY = TRUE)


}
