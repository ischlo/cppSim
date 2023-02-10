#include <RcppArmadillo.h>
#include <strings.h>
#include <cstdlib>
#include <iterator>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]


using namespace Rcpp;


arma::vec apply_iter(const arma::sp_mat& x, int dim);

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/


//'
//'Function to take the exponential of each element of a matrix individually.
//'If an element is x, this function returns exp(coef*x)
//' @param mat A numeric matrix
//' @param coef The exponent
//'
//'
arma::mat mat_exp(const arma::mat& mat, double coef = 1) {

  arma::mat res = mat*coef;

  arma::mat::iterator it = res.begin();

  for (;it != res.end(); ++it) {
    // std::cout << *it << std::endl;
    *it = std::exp(*it);
  }
  return res;
}


double r_2_cpp(arma::mat data, arma::mat fit){
  // cor ^ 2;
  return 0.0;
}


double e_sorencen_cpp(arma::mat data, arma::mat fit ){

  return 0.0;
}


// e_sorensen <- function(data, fit) {
//   2*sum(apply(cbind(data %>% c
//                       ,fit %>% c), MARGIN = 1, FUN = min))/(sum(data) + sum(fit))
// }

// function to turn a matrix into a sparse one using a threshold
// to use in order to avoid computing flows for unrealistic values, for active travel 15 km seems a good fit
arma::sp_mat mat_to_sparse(const arma::mat& x, double threshold = 15.0) {

  arma::sp_mat res(x);

  arma::sp_mat::iterator it = res.begin();

  for(;it != res.end();++it) {
    if (*it > threshold) {
      *it = 0;
    }
  }
  return res;
}



//'
//'@title
//'Calibrating the balancing factors
//'
//'@description
//'Function to calibrate the A and B coefficients of the gravity model through an iterative process.
//'To avoid infinite loop, the maximum number of iterations is fixed to 50, it usually takes around 5-7 iterations.
//'to converge.
//'
//'@param cost_fun The matrix representing the cost function f(D), where D is the distance matrix.
//'@param O A numeric column vector of weights associated to the origin, the production constraint, usually the outbound flow.
//'@param D A numeric row vector of destination weights, the attraction constraint.
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
//'a_b = calibration_cpp(cost_fun,O,D)
//'@export
// [[Rcpp::export]]
Rcpp::List calibration_cpp(arma::mat cost_fun
                            ,arma::vec O
                            ,arma::vec D
                            ,double delta = 0.05) {

  arma::vec B(cost_fun.n_cols, arma::fill::ones), B_new(cost_fun.n_cols) ,A(cost_fun.n_cols), A_new(cost_fun.n_cols);

  int i = 0;
  double eps = cost_fun.n_cols;

  NumericVector e;
  // std::cout << "Entering do loop" << std::endl;
  do {

    //  wsa the fastest, but arma::sum seems slightly faster
    // // std::cout << "calculating A_new" << std::endl;
    // for(int j = 0; j < cost_fun.n_rows; ++j){
    //   A_new(j) = 1.0/arma::accu(B%D%cost_fun.row(j).as_col());
    // }
    //
    // // std::cout << "calculating B_new" << std::endl;
    // for(int j = 0; j < cost_fun.n_cols; ++j){
    //   B_new(j) = 1.0/arma::accu(A_new%O%cost_fun.col(j));
    // }

    A_new = 1.0/arma::sum(arma::mat(cost_fun.each_row() % (B.t() % D.t())),1);

    B_new = 1.0/arma::sum(arma::mat(cost_fun.each_col() % (A_new % O)),0).as_col();

    ++i;

    eps = arma::accu(arma::norm(B-B_new, 1));

    A = A_new;
    B = B_new;

    e.push_back(eps);

  } while (eps > delta & i < 50);


  return Rcpp::List::create(Rcpp::Named("A") = A
                            ,Rcpp::Named("B") = B
                            ,Rcpp::Named("e") = e
                            );
}



//'
//'@description
//'Apply function from R rewritten for sparse matrices in cpp to gain speed.
//'
//'
//'@param mat1 The matrix to which the functino is applied
//'@param vec the vector in which the result will be stored.
//'@param dim 1 to perform the operation rowwise, 2 to perform on columnwise.
//'
//'@returns
//'A vector, to which the sum function has been applied either by row or by column
//'
//'@examples
//'library(Matrix)
//'mat = matrix(data = c(1,2,3,1,2,3,1,2,3), nrow = 3,sparse = TRUE)
//'
//'res1 = apply_iter(mat,1)
//'res2 = apply_iter(mat,2)
//@export
// [[Rcpp::export]]
arma::vec apply_iter(const arma::sp_mat& x, int dim = 1) {

  int n(0);
  switch(dim) {
  case 1:
    n = x.n_rows;
    break;
  case 2:
    n = x.n_cols;
    break;
  }
  arma::vec result(n);
  arma::sp_mat::const_iterator i = x.begin();
  if (dim ==1){
    for (; i != x.end(); ++i) {
      result(i.row())+=*i;
    }
  } else if (dim == 2){
    for (; i != x.end(); ++i) {
      result(i.col())+=*i;
    }
  }
  return result;
}
// void apply_cpp(arma::mat& mat1, arma::vec& res, int dim = 1) {
//
// // add checks whether the res dim matches.
//   if (dim == 1) {
//     // arma::vec res(mat1.n_rows);
//
//     for (int i = 0; i<mat1.n_rows; ++i) {
//       res(i) = accu(mat1.row(i));
//     }
//
//   } else if (dim == 2) {
//     // arma::vec res(mat1.n_cols);
//     for (int i = 0; i<mat1.n_cols; ++i) {
//       res(i) = accu(mat1.col(i));
//     }
//
//   } else {
//     std::cout << "Enter either 1 or 2 for dimension" << std::endl;
//   }
// }


//'
//'@title
//'Run model
//'
//'@description
//'This function is the C++ implementation of run_model, it will run a model
//'
//'@param flows A integer matrix of Origin-Destination flows.
//'@param distance a distance matrix between origins and destinations.
//'@param beta Exponent to use when calculating the cost function.
//'@param type The only type of cost function currently implemented is exponential, parameter value "exp".
//'
//'@returns
//'A list containing an integer matrix with predicted values.
//'@examples
//'a = 2
//'b = 3
//'a + b
//'
// @export
// [[Rcpp::export]]
List run_model_cpp(const arma::mat& flows
                 ,const arma::mat& distance
                 ,double beta_ = .25
                 ,double threshold = 15
                 ,std::string type = "exp"){

  arma::mat f_c = mat_exp(distance, -beta_);
  std::cout << "Cost function computed ! " << std::endl;

  // arma::vec O = apply_iter(flows,1);
  // // std::cout << "Computed O"<< std::endl;
  // arma::vec D = apply_iter(flows,2);
  // // std::cout << "Computed D"<< std::endl;

  arma::vec O = arma::sum(flows,1);
  arma::vec D = arma::sum(flows.t(),1);

  Rcpp::List A_B = calibration_cpp(f_c,O,D, 0.001);

  std::cout<< " Calibration over. " << std::endl;

  arma::vec A = A_B["A"];
  arma::vec B = A_B["B"];

  arma::mat flows_model = arma::round( (A * B.as_row()) % (O * D.as_row()) % f_c);

  std::cout<< " Values modelled " << std::endl;

  return Rcpp::List::create(Rcpp::Named("values") = flows_model);
}



// [[Rcpp::export]]
List run_model_prod_cpp(const arma::sp_mat& flows
                     ,const arma::mat& distance
                     ,double beta = .25
                     ,std::string type = "exp"){

  //  simulating the production constrained model, when Oi is known.

  // arma::mat f_c = mat_exp(distance, -beta);
  // std::cout << "Cost function computed ! " << std::endl;
  //
  // arma::vec O = apply_iter(flows,1);
  // // std::cout << "Computed O"<< std::endl;
  // arma::vec D = apply_iter(flows,2);
  // // std::cout << "Computed D"<< std::endl;
  //
  // Rcpp::List A_B = calibration_cpp(f_c,O,D, 0.001);
  //
  // std::cout<< " Calibration over. " << std::endl;
  //
  // arma::vec A = A_B["A"];
  // arma::vec B = A_B["B"];
  //
  // arma::mat flows_model = arma::round( (A * B.as_row()) % (O * D.as_row()) % f_c);
  //
  // std::cout<< " Values modelled " << std::endl;

  return Rcpp::List::create(Rcpp::Named("values"));
}



// [[Rcpp::export]]
List run_model_attr_cpp(const arma::mat& flows
                          ,const arma::mat& distance
                          ,double beta = .25
                          ,std::string type = "exp"){

  // simulating the attraction constrained model, when Dj is known

  // arma::mat f_c = mat_exp(distance, -beta);
  // std::cout << "Cost function computed ! " << std::endl;
  //
  // arma::vec O = apply_iter(flows,1);
  // // std::cout << "Computed O"<< std::endl;
  // arma::vec D = apply_iter(flows,2);
  // // std::cout << "Computed D"<< std::endl;
  //
  // Rcpp::List A_B = calibration_cpp(f_c,O,D, 0.001);
  //
  // std::cout<< " Calibration over. " << std::endl;
  //
  // arma::vec A = A_B["A"];
  // arma::vec B = A_B["B"];
  //
  // arma::mat flows_model = arma::round( (A * B.as_row()) % (O * D.as_row()) % f_c);
  //
  // std::cout<< " Values modelled " << std::endl;

  return Rcpp::List::create(Rcpp::Named("values"));
}



// [[Rcpp::export]]
double pearsoncoeff(const arma::mat& X, const arma::mat& Y)
{
  // double sq_mean = arma::pow(Y.as_col() - arma::mean(Y.as_col()),2);
  //
  // double resid = arma::pow(X.as_col() - Y.as_col(),2);

  return arma::as_scalar(arma::cor(X.as_col(),Y.as_col()));

  // return 1.0 - arma::accu()/arma::accu();

}

double abs_val(double x) {

  if (x >= 0){
    return x;
  }
  return -x;
}


// [[Rcpp::export]]
List run_simulation(const arma::mat& distance
                      ,const arma::mat& flows
                      ,double beta_orig = .25
                      ,std::string type = "exp"){

  // Newton method to find maxima here
  // trying to find the beta that maximisies the quality of fit function

  double beta_new = beta_orig + .05;
  List res1, res2,res3;
  double eps = 1.0;
  int i = 0;
  double step = 0.03;

do {


  res1 = run_model_cpp(flows
                         ,distance
                         ,beta_orig);

  res2 = run_model_cpp(flows
                         ,distance
                         ,beta_orig + step);

  res3 = run_model_cpp(flows
                         ,distance
                         ,beta_orig - step);

  double r2 = -pearsoncoeff(flows, res1["values"]);

  double r2_2 = -pearsoncoeff(flows, res2["values"]);

  double r2_3 = -pearsoncoeff(flows, res3["values"]);
  //
  double d_r2 = (r2_2 - r2)/step;

  double dd_r2 = (r2_3 - 2*r2 + r2_2)/(step*step);

  beta_new = beta_orig - d_r2/dd_r2;

  eps = abs_val(beta_orig-beta_new);

  beta_orig = beta_new;

  i++;

  } while ( eps > step);

  std::cout << "Iteration completed in " << i << " steps" << std::endl;

  return Rcpp::List::create(Rcpp::Named("best_fit_values") = res1["values"]
                              ,Rcpp::Named("best_fit_beta") = beta_orig);
}


