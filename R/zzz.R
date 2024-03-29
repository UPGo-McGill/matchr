.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply",  quietly = TRUE)) {
    packageStartupMessage(
      "Install the {future} and {future.apply} packages to enable ",
      "multithreaded processing.")
  }

  if (!progressr::handlers(global = NA)) {
    packageStartupMessage("To enable progress bars, type ",
                          "'progressr::handlers(global = TRUE)'.")
  }
}

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("future", quietly = TRUE)) {
    .matchr_env$globals_max_size <- options(future.globals.maxSize = +Inf)
  }
}

.onUnload <- function(libpath) {
  
  library.dynam.unload("matchr", libpath)
  
  if (requireNamespace("future", quietly = TRUE)) {
    options(.matchr_env$globals_max_size)
  }
}
