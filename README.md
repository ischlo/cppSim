------------------------------------------------------------------------

<!-- README.md is generated from README.Rmd. Please edit that file -->

# cppSim

<!-- badges: start -->

<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14367756.svg)](https://doi.org/10.5281/zenodo.14367756) -->

[![CRAN](https://www.r-pkg.org/badges/version/cppSim)](https://CRAN.R-project.org/package=cppSim)
[![](https://img.shields.io/badge/doi-10.1088/2632--072X/add257-blue.svg)](https://doi.org/10.1088/2632-072X/add257)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
![](https://github.com/ischlo/cppSim/actions/workflows/check-standard.yaml/badge.svg)
<!-- ![](https://github.com/ischlo/cppSim/actions/workflows/rhub.yaml/badge.svg) -->
<!-- [![arXiv](https://img.shields.io/badge/arXiv-1234.56789-b31b1b.svg?style=flat-square)](https://arxiv.org/abs/2309.02112) -->
<!-- ![](https://github.com/ischlo/cppSim/actions/workflows/test-coverage.yaml/badge.svg) -->

<!-- badges: end -->

This aims at providing a set of fast, memory efficient functions to
perform spatial interaction modelling, also called gravity modelling.
Currently, the doubly and singly constrained models are implemented for
canonical set of constraints. Future versions will aim to implement more
origin and destination constraints as well. It was developed in the
context of studying commuter flows by active travel (cycling & walking )
in Great Britain as part of a project at CASA, UCL.

## Installation

You can install the package from github or CRAN with:

``` r
install.packages("cppSim")

# # Or: 
# install.packages("pak")
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

## Spatial interaction models

Refer to the vignette to find some theory on SIMs and a *naive*
implementation in `R`.

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

## Source

For an example of what can be done with this package, please refer to
the publication on active travel spatial interaction models in London
for which it was originally developed.

> [Schlosser, I., Maureira, V.M., Milton, R., Arcaute, E., Batty, M.,
> 2023. Active-travel modelling: a methodological approach to networks
> for walking and cycling commuting
> analysis.](https://doi.org/10.1088/2632-072X/add257)

**Pre-print**: <https://arxiv.org/abs/2309.02112>

The accompanying code for the analysis is provided in the
[`ischlo/quant_cycle_walk`](https://github.com/ischlo/quant_cycle_walk)
repository.

## Dependencies

This package has some dependencies that might need manual installation,
although the most important external ones have been provided with the
source code.

### External

The package uses the [`armadillo`](https://arma.sourceforge.net)
library, which is imported and linked automatically when the package is
installed.

### Other

On the R side, it uses `Rcpp`\[@eddelbuettel2011\] and
`RcppArmadillo`\[@eddelbuettel2014\].

## Performance

Compared to the equivalent functions implemented in pure R, it runs
about x10 faster for a $`\sim 1000 \times 1000`$ OD matrix, the speed up
is increasingly more significant as matrices get bigger.

         test replications elapsed relative user.self sys.self user.child sys.child
    2     cpp           10   3.257    1.000     3.046    0.184          0         0
    1 regular           10  34.201   10.501    31.887    1.970          0         0

## Citation

    To cite package 'cppSim' in publications use:

      Schlosser I (2023). _cppSim: Fast and Memory Efficient Spatial
      Interaction Models_. doi:10.5281/zenodo.14367756
      <https://doi.org/10.5281/zenodo.14367756>,
      <https://ischlo.github.io/cppSim/>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {cppSim: Fast and Memory Efficient Spatial Interaction Models},
        author = {Ivann Schlosser},
        year = {2023},
        doi = {10.5281/zenodo.14367756},
        url = {https://ischlo.github.io/cppSim/},
      }

<!-- ## References -->
