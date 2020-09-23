
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparseLRMatrix

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`sparseLRMatrix` provides a single matrix S4 class called
`sparseLRMatrix` which represents matrices that can be expressed as the
sum of sparse matrix and a low rank matrix. We also provide an efficient
SVD method for these matrices by wrapping the `RSpectra` SVD
implementation.

Eventually, we will fully subclass `Matrix::Matrix` objects, but the
current implementation is extremely minimal.

## Installation

You can install the released version of sparseLRMatrix from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("remotes")
remotes::install_github("RoheLab/sparseLRMatrix")
```

## Usage

``` r
library(sparseLRMatrix)
#> Loading required package: Matrix
library(RSpectra)

set.seed(528491)

n <- 50
m <- 40
k <- 3

A <- rsparsematrix(n, m, 0.1)

U <- Matrix(rnorm(n * k), nrow = n, ncol = k)
V <- Matrix(rnorm(m * k), nrow = m, ncol = k)

# construct the matrix, which represents A + U %*% t(V)
X <- sparseLRMatrix(sparse = A, U = U, V = V)

s <- svds(X, 5)  # efficient
```

And a quick sanity check

``` r
Y <- A + tcrossprod(U, V)
s2 <- svds(Y, 5)  # inefficient, but same calculation

# singular values match up, you can check for yourself
# that the singular vectors do as well!
all.equal(s$d, s2$d)
#> [1] TRUE
```
