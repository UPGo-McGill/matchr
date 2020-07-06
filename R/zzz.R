.onAttach <- function(libname, pkgname) {

  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply",  quietly = TRUE)) {

    packageStartupMessage(
      "Install the {future} and {future.apply} packages to enable ",
      "multithreaded processing.")

  }

  if (!requireNamespace("progressr", quietly = TRUE)) {

    packageStartupMessage("Install the {progressr} package to ",
                          "enable progress bars.")

  }

  if (!requireNamespace("crayon", quietly = TRUE)) {

    packageStartupMessage("Install the {crayon} package to ",
                          "enable styled output text.")

  }

}


.onLoad <- function(libname, pkgname) {

  if (requireNamespace("future", quietly = TRUE)) {

    .matchr_env$globals_max_size <- options(future.globals.maxSize = +Inf)

  }

}

.onUnload <- function(libname, pkgname) {

  if (requireNamespace("future", quietly = TRUE)) {

    options(future.globals.maxSize = .matchr_env$globals_max_size)

  }

}
