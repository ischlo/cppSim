.onAttach <- function(libname, pkgname) {
  found <- cpp_found_openmp()
  if (!found) {
    packageStartupMessage("Cound not find OpenMP.")
  } else {
    packageStartupMessage("OpenMP detected, parallel computations will be performed.")
  }
}
