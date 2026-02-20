# Evaluate a multivariate normal (log-)density function at a specified point using Cholesky or eigen decomposition

Evaluate a multivariate normal (log-)density function at a specified
point using Cholesky or eigen decomposition

## Usage

``` r
dmvnorm(x, mean, cov, method = c("chol", "eigen"), log = TRUE)
```

## Arguments

- x:

  A vector, the point at which to evaluate the density.

- mean:

  The mean vector for the multivariate distribution.

- cov:

  The covariance matrix for the multivariate distribution.

- method:

  The method to use for matrix decomposition, either "chol" for Cholesky
  decomposition or "eigen" for eigen decomposition. Defaults to "chol".

- log:

  A boolean, TRUE to return log-density and FALSE to return the density.
  Defaults to TRUE.

## Value

A single numeric value, the (log-)density at the specified point.

## Examples

``` r
density <- dmvnorm(x = rep(0, 4), mean = rep(0, 4), cov = diag(1, nrow = 4))
```
