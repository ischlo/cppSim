# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#'
#'@title
#'Calibrating the balancing factors
#'
#'@description
#'Function to calibrate the A and B coefficients of the gravity model through an iterative process.
#'To avoid infinite loop, the maximum number of iterations is fixed to 50, it usually takes around 5-7 iterations.
#'to converge.
#'
#'@param cost_fun The matrix representing the cost function f(D), where D is the distance matrix.
#'@param O A numeric column vector of weights associated to the origin, the production constraint, usually the outbound flow.
#'@param D A numeric row vector of destination weights, the attraction constraint.
#'@param delta The error term we can tolerate on the convergence of the values.
#'
#'@returns A list object containing the A and B vectors of coefficients
#'and a vector e to verify convergence speed.
#'
#'@examples
#'cost_fun = matrix(data = c(1,2,3,4,5,6,7,8,9), nrow = 3)
#'O = c(1,2,3)
#'D = c(3,2,1)
#'
#'a_b = calibration_cpp(cost_fun,O,D)
NULL

#'
#'@title
#'Run model
#'
#'@description
#'This function is the C++ implementation of run_model, it will run a model
#'
#'@param flows A integer matrix of Origin-Destination flows.
#'@param distance a distance matrix between origins and destinations.
#'@param beta Exponent to use when calculating the cost function.
#'@param type The only type of cost function currently implemented is exponential, parameter value "exp".
#'
#'@returns
#'A list containing an integer matrix with predicted values.
#'@examples
#'a = 2
#'b = 3
#'a + b
#'
NULL

calibration_cpp <- function(cost_fun, O, D, delta = 0.05) {
    .Call(`_cppSim_calibration_cpp`, cost_fun, O, D, delta)
}

run_model_cpp <- function(flows, distance, beta_ = .25, threshold = 15, type = "exp") {
    .Call(`_cppSim_run_model_cpp`, flows, distance, beta_, threshold, type)
}

run_simulation_cpp <- function(distance, flows, beta_orig = .25, type = "exp") {
    .Call(`_cppSim_run_simulation_cpp`, distance, flows, beta_orig, type)
}

#'
NULL

run_model_single_cpp <- function(flow, weight, distance, beta = .25, type = "exp") {
    .Call(`_cppSim_run_model_single_cpp`, flow, weight, distance, beta, type)
}

#'
NULL

apply_iter <- function(x, dim = 1L) {
    .Call(`_cppSim_apply_iter`, x, dim)
}

pearsoncoeff <- function(X, Y) {
    .Call(`_cppSim_pearsoncoeff`, X, Y)
}

abs_val <- function(x) {
    .Call(`_cppSim_abs_val`, x)
}

