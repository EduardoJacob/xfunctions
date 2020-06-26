
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Loading ",pkgname," from ",libname)
  invisible()
}

# .onLoad <- function(libname, pkgname) { }

# .onUnload <- function(libname, pkgname) { }

