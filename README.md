
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cppSim

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

![](https://github.com/ischlo/cppSim/actions/workflows/check-standard.yaml/badge.svg)

![](https://github.com/ischlo/cppSim/actions/workflows/rhub.yaml/badge.svg)

![](https://github.com/ischlo/cppSim/actions/workflows/test-coverage.yaml/badge.svg)

<!-- badges: end -->

This package is in its early versions of development, it aims at
providing a set of fast, efficient functions to perform Gravity models
in the context of spatial interaction modelling. Currently, the doubly
constrained model is implemented and future versions will aim to
implement origin and destination constraints as well. It was developed
in the context of studying commuter flows by active travel (cycling &
walking ) in Great Britain as part of a project at CASA, UCL.

## Installation

Not yet on CRAN, so please install the development version of `cppSim`
with:

``` r
# install.packages("devtools")
devtools::install_github("ischlo/cppSim")

# pak::pak("ischlo/cppSim")
```

## Built in data sets

The package comes with sample data sets that allow to test the functions
right away as well as see the type of input that is recommended.

- flows_test : using the official census data in England from 2011, it’s
  a 983x983 matrix representing the flows of cyclists and pedestrians
  from each to each MSOA in London.
- distance_test : the distances between centroids of MSOAs. Computed
  with the London road network from OpenStreetMap and using the
  `cppRouting` package.

## Example

Using the built-in data sets `flows_test` and `distance_test`, we can
run a test by following the example This is a basic example which shows
you how to solve a common problem:

``` r
library(cppSim)
## basic example code

data("flows_test")
data("distance_test")


model_test <- run_model(
  flows = flows_test,
  distance = distance_test
)
```

## Performance

Compared to the equivalent functions implemented in pure R, it runs
about x10 faster.

    #>      test replications elapsed relative user.self sys.self user.child sys.child
    #> 2     cpp           10   3.257    1.000     3.046    0.184          0         0
    #> 1 regular           10  34.201   10.501    31.887    1.970          0         0
