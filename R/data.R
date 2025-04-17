#' flows_test
#'
#' @format ## `flows_test`
#' A matrix of size 983x983 containing flows of users using walking or cycling as the main method of commute.
#'
#'
#' @source UK Census, 2011
"flows_test"


#' distance_test
#'
#' @format ## `distance_test`
#' A 983x983 matrix of distances between MSOAs in London.
#' Computed using the London road network from OpenStreetMap and the cppRouting package.
#'
#' @source Ivann Schlosser, 2022
"distance_test"


#' london_msoa
#'
#' @format ## `london_msoa`
#' A data.table with London MSOA, their centroids and geometries
#'
#'
#' @source ONS, Office for National Statistics, 2011
"london_msoa"


#' flows_london
#'
#' @format ## `flows_london`
#' A data.table with flows information
#'
#'
#' @source ONS, Office for National Statistics, 2011
"flows_london"
