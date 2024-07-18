// #include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]

arma::mat mat_exp(const arma::mat& mat, double coef);

double pearsoncoeff(const arma::mat& X, const arma::mat& Y);

double abs_val(double x);
