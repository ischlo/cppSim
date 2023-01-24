#include <RcppArmadillo.h>
#include <strings.h>

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::plugins("cpp11")]]

#include <iterator>

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

    // std::cout << "calculating A_new" << std::endl;
    for(int j = 0; j < cost_fun.n_rows; ++j){
      A_new(j) = 1.0/arma::accu(B%D%cost_fun.row(j).as_col());
    }

    // std::cout << "calculating B_new" << std::endl;
    for(int j = 0; j < cost_fun.n_cols; ++j){
      B_new(j) = 1.0/arma::accu(A_new%O%cost_fun.col(j));
    }
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
//'
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
List run_model_cpp(const arma::sp_mat& flows
                 ,const arma::mat& distance
                 ,double beta = .25
                 ,std::string type = "exp"){

  arma::mat f_c = mat_exp(distance, -beta);
  std::cout << "Cost function computed ! " << std::endl;

  arma::vec O = apply_iter(flows,1);
  // std::cout << "Computed O"<< std::endl;
  arma::vec D = apply_iter(flows,2);
  // std::cout << "Computed D"<< std::endl;

  Rcpp::List A_B = calibration_cpp(f_c,O,D, 0.001);

  std::cout<< " Calibration over. " << std::endl;

  arma::vec A = A_B["A"];
  arma::vec B = A_B["B"];

  arma::mat flows_model = arma::round( (A * B.as_row()) % (O * D.as_row()) % f_c);

  std::cout<< " Values modelled " << std::endl;

  return Rcpp::List::create(Rcpp::Named("values") = flows_model);
}

