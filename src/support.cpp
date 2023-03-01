#include <RcppArmadillo.h>
#include <strings.h>
#include <cstdlib>
#include <iterator>
#include "support.h"

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]


using namespace Rcpp;


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


 // [[Rcpp::export]]
 double pearsoncoeff(const arma::mat& X, const arma::mat& Y)
 {
   return arma::as_scalar(arma::cor(X.as_col(),Y.as_col()));
 }

 // [[Rcpp::export]]
 double abs_val(double x) {
   if(x >= 0) return x;
   else return -x;
 }
