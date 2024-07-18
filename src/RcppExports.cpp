// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// calibration_cpp
Rcpp::List calibration_cpp(arma::mat cost_fun, arma::vec O, arma::vec D, double delta);
RcppExport SEXP _cppSim_calibration_cpp(SEXP cost_funSEXP, SEXP OSEXP, SEXP DSEXP, SEXP deltaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type cost_fun(cost_funSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type O(OSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type D(DSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    rcpp_result_gen = Rcpp::wrap(calibration_cpp(cost_fun, O, D, delta));
    return rcpp_result_gen;
END_RCPP
}
// run_model_cpp
Rcpp::List run_model_cpp(const arma::mat& flows, const arma::mat& distance, double beta_, int ncores_);
RcppExport SEXP _cppSim_run_model_cpp(SEXP flowsSEXP, SEXP distanceSEXP, SEXP beta_SEXP, SEXP ncores_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type flows(flowsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type distance(distanceSEXP);
    Rcpp::traits::input_parameter< double >::type beta_(beta_SEXP);
    Rcpp::traits::input_parameter< int >::type ncores_(ncores_SEXP);
    rcpp_result_gen = Rcpp::wrap(run_model_cpp(flows, distance, beta_, ncores_));
    return rcpp_result_gen;
END_RCPP
}
// run_simulation_cpp
Rcpp::List run_simulation_cpp(const arma::mat& distance, const arma::mat& flows, double beta_orig);
RcppExport SEXP _cppSim_run_simulation_cpp(SEXP distanceSEXP, SEXP flowsSEXP, SEXP beta_origSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type distance(distanceSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type flows(flowsSEXP);
    Rcpp::traits::input_parameter< double >::type beta_orig(beta_origSEXP);
    rcpp_result_gen = Rcpp::wrap(run_simulation_cpp(distance, flows, beta_orig));
    return rcpp_result_gen;
END_RCPP
}
// cpp_found_openmp
bool cpp_found_openmp();
RcppExport SEXP _cppSim_cpp_found_openmp() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(cpp_found_openmp());
    return rcpp_result_gen;
END_RCPP
}
// run_model_single_cpp
List run_model_single_cpp(const arma::vec& flow, const arma::vec& weight, const arma::mat& distance, double beta, std::string type);
RcppExport SEXP _cppSim_run_model_single_cpp(SEXP flowSEXP, SEXP weightSEXP, SEXP distanceSEXP, SEXP betaSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type flow(flowSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type distance(distanceSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< std::string >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(run_model_single_cpp(flow, weight, distance, beta, type));
    return rcpp_result_gen;
END_RCPP
}
// apply_iter
arma::vec apply_iter(const arma::sp_mat& x, int dim);
RcppExport SEXP _cppSim_apply_iter(SEXP xSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::sp_mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(apply_iter(x, dim));
    return rcpp_result_gen;
END_RCPP
}
// pearsoncoeff
double pearsoncoeff(const arma::mat& X, const arma::mat& Y);
RcppExport SEXP _cppSim_pearsoncoeff(SEXP XSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(pearsoncoeff(X, Y));
    return rcpp_result_gen;
END_RCPP
}
// abs_val
double abs_val(double x);
RcppExport SEXP _cppSim_abs_val(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(abs_val(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_cppSim_calibration_cpp", (DL_FUNC) &_cppSim_calibration_cpp, 4},
    {"_cppSim_run_model_cpp", (DL_FUNC) &_cppSim_run_model_cpp, 4},
    {"_cppSim_run_simulation_cpp", (DL_FUNC) &_cppSim_run_simulation_cpp, 3},
    {"_cppSim_cpp_found_openmp", (DL_FUNC) &_cppSim_cpp_found_openmp, 0},
    {"_cppSim_run_model_single_cpp", (DL_FUNC) &_cppSim_run_model_single_cpp, 5},
    {"_cppSim_apply_iter", (DL_FUNC) &_cppSim_apply_iter, 2},
    {"_cppSim_pearsoncoeff", (DL_FUNC) &_cppSim_pearsoncoeff, 2},
    {"_cppSim_abs_val", (DL_FUNC) &_cppSim_abs_val, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_cppSim(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
