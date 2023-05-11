#include <RcppArmadillo.h>
#include <strings.h>
#include <cstdlib>
#include <iterator>
#include "support.h"

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]


using namespace Rcpp;

//'
 //'@title
 //'Run singly constrained model
 //'
 //'@description
 //'This function is the C++ implementation of run_model_single, it will run a singly constrained model
 //'
 //'@param flows a vector of Origin or Destination flows.
 //'@param weight a vectoer of weights, set to 1 by default.
 //'@param distance a distance matrix between origins and destinations.
 //'@param beta Exponent to use when calculating the cost function.
 //'@param type The only type of cost function currently implemented is exponential, parameter value "exp".
 //'
 //'@returns
 //'A list containing a numeric matrix with predicted flows.
 //'@examples
 //'a = 2
 //'b = 3
 //'a + b
 //'
 // @export
 // [[Rcpp::export]]
List run_model_single_cpp(const arma::vec& flow
                          ,const arma::vec& weight
                          ,const arma::mat& distance
                          ,double beta = .25
                          ,std::string type = "exp"){

  //  simulating the production constrained model, when Oi is known.

  arma::mat f_c = mat_exp(distance, -beta);
  // std::cout << "Cost function computed ! " << std::endl;
  //
  arma::vec A = 1.0/arma::sum(arma::mat(f_c.each_row() % (weight.t())),1);

  arma::mat T_model = arma::round(((A % flow) * weight.t()) % f_c);

  return Rcpp::List::create(Rcpp::Named("values") = T_model);
}




// // [[Rcpp::export]]
// List run_model_attr_cpp(const arma::vec& in_flow
//                           ,const arma::vec& weight
//                           ,const arma::mat& distance
//                           ,double beta = .25
//                           ,std::string type = "exp"){
//
//   arma::mat f_c = mat_exp(distance, -beta);
//   // std::cout << "Cost function computed ! " << std::endl;
//   //
//   arma::vec B = 1.0/arma::sum(arma::mat(f_c.each_row() % (weight.t())),1);
//
//   arma::mat T_model = arma::round(( weight * (B % in_flow) ) % f_c);
//
//   return Rcpp::List::create(Rcpp::Named("values") = T_model);
//
// }


