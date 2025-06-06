// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]


#include <RcppArmadillo.h>
#include <cstdlib>
#include <iterator>
#include <strings.h>
#include "support.h"

// #define ARMA_USE_OPENMP true

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

//'@name calibration_cpp
//'@title
//'Calibrating the balancing factors
//'
//'@description
//'Function to calibrate the A and B coefficients of the gravity model through
// an iterative process. 'To avoid infinite loop, the maximum number of
// iterations is fixed to 50, it usually takes around 5-7 iterations. 'to
// converge.
//'
//'@param cost_fun The matrix representing the cost function f(D), where D is
// the distance matrix.
//'@param O A numeric column vector of weights associated to the origin, the
// production constraint, usually the outbound flow.
//'@param D A numeric row vector of destination weights, the attraction
// constraint.
//'@param delta The error term we can tolerate on the convergence of the values.
//'
//'@returns A list object containing the A and B vectors of coefficients
//'and a vector e to verify convergence speed.
//'
//'@examples
//'cost_fun = matrix(data = c(1,2,3,4,5,6,7,8,9), nrow = 3)
//'O = c(1,2,3)
//'D = c(3,2,1)
//'
// a_b = calibration_cpp(cost_fun,O,D)
// [[Rcpp::export]]
Rcpp::List calibration_cpp(arma::mat cost_fun, arma::vec O, arma::vec D,
                           double delta = 0.05) {

  arma::vec B(cost_fun.n_cols, arma::fill::ones), B_new(cost_fun.n_cols),
      A(cost_fun.n_cols), A_new(cost_fun.n_cols);

  int i = 0;
  double eps = cost_fun.n_cols;

  Rcpp::NumericVector e;

  do {

    A_new =
        1.0 / arma::sum(arma::mat(cost_fun.each_row() % (B.t() % D.t())), 1);
    B_new = 1.0 /
            arma::sum(arma::mat(cost_fun.each_col() % (A_new % O)), 0).as_col();
    ++i;
    eps = arma::accu(arma::norm(B - B_new, 1));

    A = A_new;
    B = B_new;

    e.push_back(eps);

  } while ((eps > delta) & (i < 50));

  return Rcpp::List::create(Rcpp::Named("A") = A, Rcpp::Named("B") = B,
                            Rcpp::Named("e") = e);
}

//'@name run_model_cpp
//'@title Run model
//'
//'@description
//'This function is the C++ implementation of run_model, it will run a model
//'
//'@param flows A integer matrix of Origin-Destination flows.
//'@param distance a distance matrix between origins and destinations.
//'@param beta Exponent to use when calculating the cost function.
//@param type The only type of cost function currently implemented is
// exponential, parameter value "exp".
//'
//'@returns
//'A list containing an integer matrix with predicted values.
//@examples
// a = 2
// b = 3
// a + b
// [[Rcpp::export]]
Rcpp::List run_model_cpp(const arma::mat &flows, const arma::mat &distance,
                         double beta_, int ncores_ = 4) {

#ifdef _OPENMP
  omp_set_num_threads(ncores_);
  // std::cout << "using multiple cores " << std::endl;
#endif
  arma::mat f_c = mat_exp(distance, -beta_);

  arma::vec O = arma::sum(flows, 1);
  arma::vec D = arma::sum(flows.t(), 1);

  Rcpp::List A_B = calibration_cpp(f_c, O, D, 0.001);

  arma::vec A = A_B["A"];
  arma::vec B = A_B["B"];

  arma::mat flows_model =
      (A * B.as_row()) % (O * D.as_row()) % f_c; // arma::round( )

  return Rcpp::List::create(Rcpp::Named("values") = flows_model);
}

//
// [[Rcpp::export]]
Rcpp::List run_simulation_cpp(const arma::mat &distance, const arma::mat &flows,
                              double beta_orig = .25) {

  //,std::string type = "exp"
  // Newton method to find maxima here
  // trying to find the beta that maximisies the quality of fit function

  double beta_new = beta_orig + .05;
  Rcpp::List res1, res2, res3;
  double eps = 1.0;
  // int i = 0;
  //
  double step = 0.03;

  // #ifdef _OPENMP
  // omp_set_num_threads(ncores_);
  //  std::cout << "using multiple cores " << std::endl;
  // #endif

  do {

    res1 = run_model_cpp(flows, distance, beta_orig, 1);

    res2 = run_model_cpp(flows, distance, beta_orig + step, 1);

    res3 = run_model_cpp(flows, distance, beta_orig - step, 1);

    double r2 = -pearsoncoeff(flows, res1["values"]);

    double r2_2 = -pearsoncoeff(flows, res2["values"]);

    double r2_3 = -pearsoncoeff(flows, res3["values"]);
    //
    double d_r2 = (r2_2 - r2) / step;

    double dd_r2 = (r2_3 - 2 * r2 + r2_2) / (step * step);

    beta_new = beta_orig - d_r2 / dd_r2;

    eps = abs_val(beta_orig - beta_new);

    beta_orig = beta_new;

    // i++;

  } while (eps > step);

  // std::cout << "Iteration completed in " << i << " steps" << std::endl;

  return Rcpp::List::create(Rcpp::Named("best_fit_values") = res1["values"],
                            Rcpp::Named("best_fit_beta") = beta_orig);
}
