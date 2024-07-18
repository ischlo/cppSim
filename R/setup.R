.onLoad <- function(libname, pkgname) {
  found <- cpp_found_openmp()
  if (!found) {
    cli::cli_alert_info("Cound not find OpenMP.")
  } else {
    cli::cli_alert_info("OpenMP detected, parallel computations will be performed.")
  }
}
