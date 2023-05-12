#  loading data
# spatial file of partition (administrative units)
# this file contains polygons of administrative units, but also various from that will be used for routing.

# Constructing network

# parameters:
# edges,nodes, simple
# cppr stuff
# graph


# Routing (origins and destinations based on the from)
# which from to route as origin and destination
# params : centroid
# available options: geom_centr, pw_centr, graph_centr, ew_centr (employment weighted centr)
# finding the nearest node in the network to the centroid.
# geom_centr_node
# pw_centr_node
# graph_centr_node

# grav model
# cost function: exp, pow, distance matrice
# flows:
# function sketch :
# https://cloud.r-project.org/doc/manuals/r-release/R-lang.html read for inspiration
#

#'@name simulation
#'@title
#'Running a whole simulation of a doubly constrained gravity model
#'@description
#'this script takes flows data, distance matrix, and a reference beta parameter
#'and finds the optimal beta value for the model, runs it, and returns the result and the beta of
#'best fit.
#'
#'currently only the exp value is accepted for the cost_fun parameter.
#'
#'@param flows_matrix a integer matrix of flows
#'@param dist_matrix a distance matrix containing numeric values in kilometers
#'@param beta_offset an offset from 0 from which to start looking for the best fit value.
#'@param cost_fun type of cost function
#'
#'@returns creates a folder based on the run_name parameter to which images and files are written.
#' The file run_name_best_fit.rds contain the matrices with values from the model
#', and the quality of fit values for the beta values.
#'
#'@examples
#'
#'data(flows_test)
#'data(distance_test)
#'
#'model <- simulation(flows_test,distance_test)
#'@export
simulation <- function(flows_matrix
                       ,dist_matrix
                       ,beta_offset = .25
                       ,cost_fun = "exp"
                       ) {
  # run name is a prefix to include in the name of saved files

  # CHECK VARIABLES PROVIDED TO SUBMIT TO CPP

  stopifnot(any(class(flows_matrix) == "matrix")
            ,typeof(flows_matrix) == "integer"
            ,any(class(dist_matrix) == "matrix")
            ,mode(dist_matrix) == "numeric"
            ,is.numeric(beta_offset)) # other checks to add if networks are provided

  # HERE CAN BE ADDED THE ROUTING PART FOR MODELS THAT ARE NOT TO BIG (100 OD NODES FOR EXAMPLE)
  # ,WHERE THE ROUTING CAN BE DONE LOCALLY AS WELL IF AN APPROPRIATE DATA SET TO CONSTRUCT THE GRAPH IS PROVIDED
  # OVERALL SHOULD BE MUCH SIMPLER THAN THE R VERSION IN THE OTHER PROJECT.
  #### GRAVITY MODEL STUFF
  # key values : for an exponential cost function,

  return(run_simulation_cpp(dist_matrix
                     ,flows_matrix
                     ,beta_orig = beta_offset
                     ))
}


#'@name run_model
#'@title
#'Running doubly constrained model
#'
#'@title
#'Run model
#'
#'@description
#'This function is the C++ implementation of run_model, it will run a doubly constrained model
#'
#'@param flows A integer matrix of Origin-Destination flows.
#'@param distance a distance matrix between origins and destinations, provide distance in km.
#'@param beta Exponent to use when calculating the cost function.
#'@param ncores on how manz cores should the computation run in parallel, if OPENMP is found on the machine.
#'@param type The only type of cost function currently implemented is exponential, parameter value "exp".
#'@returns
#'A list containing an integer matrix with predicted values.
#'
#'@examples
#'
#'data(flows_test)
#'data(distance_test)
#'
#'model_test <- run_model(flows_test,distance_test)
#'
#'@export
run_model <- function(flows
                      ,distance
                      ,beta = 0.25
                      ,ncores = 1
                      ,type = "exp"
                      ) {

  if(!is.matrix(flows) &
     !(typeof(flows) %in% c("integer"))) {
    stop("provide a matrix with integers for flows, you can force the data type with 'as.integer(flows)'")
  }

  if(!is.matrix(distance) &
     !(is.numeric(distance))) {
    stop("provide a matrix with numeric values for the distance.")
  }

  if(!(type %in% c("exp","pow"))) {
    stop("the type of cost function is either 'exp' for exponential or 'pow' for power law.")
  }

  if(!is.numeric(beta)) {
    stop("provide a numeric values for the beta parameter.")
  }

  if(is.na(as.integer(ncores))) {
    print('Non integer value provided to ncores, using the default values of 1')
    ncores <<- 1
  } else if (ncores > RcppParallel::defaultNumThreads()) {
    print('Value provided to ncores to big, using default of one')
    ncores <<- 1
  }

  print(paste0("Running a model on "
               ,ncores
               ," cores."))

  return(
    run_model_cpp(flows
                ,distance
                ,beta
                ,ncores
                ,type
                )
    )

}

#'@name run_model_single
#'@title
#'Running a singly constrained model
#'
#'@description
#'This function is the C++ implementation of run_model, it will run a singly constrained model
#'there must be a match in the dimensions, when running a production constrained model,
#'any(dim(distance) == length(flows)) must be TRUE
#'if no values for weight are provided, a vector with ones is used
#'
#'@param flows A vector of either origin (production constrained) or destination (attraction constrained) flows.
#'@param distance a distance matrix between origins and destinations, provide distance in km.
#'@param weight a vector of weights for the unconstrained part of the model.
#'@param beta Exponent to use when calculating the cost function, default .25.
#'@param type The only type of cost function currently implemented is exponential, parameter value "exp".
#'
#'@returns
#'A list containing a matrix with predicted values.
#'
#'@examples
#'
#'data(flows_test)
#'data(distance_test)
#'
#'flows_test <- apply(flows_test,MARGIN = 1,FUN = sum)
#'
#'model_test <- run_model_single(flows_test,distance_test)
#'
#'@export
run_model_single <- function(flows
                             ,distance
                             ,weight = NULL
                             ,beta = 0.25
                             ,type = "exp"){

  stopifnot(is.numeric(flows)
            ,is.numeric(distance)
            ,any(is.numeric(weight),is.null(weight))
            ,any(length(flows) == dim(distance))
            )

  if(is.null(weight) & length(flows) == dim(distance)[1]) {
    weight = rep_len(1, dim(distance)[2])
  } else if(is.null(weight) & length(flows) == dim(distance)[2]) {
    weight = rep_len(1, dim(distance)[1])
  } else {
    stop("provide flows that match one of the distance matrix dimensions.")
  }


  run_model_single_cpp(flow = flows
                       ,weight = weight
                       ,distance = distance
                       ,beta = beta)
}







