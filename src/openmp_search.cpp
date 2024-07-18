#include <RcppArmadillo.h>
//'
//'Function to check availability of OPENMP to run in parallel.
//'If openmp is found, this function returns TRUE
//'
// [[Rcpp::export]]
bool cpp_found_openmp() {
  bool found = false;
#ifdef _OPENMP
  found = true;
#endif
  return found;
}