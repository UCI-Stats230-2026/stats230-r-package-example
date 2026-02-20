# Simulate realizations from a multivariate normal distribution using Cholesky decomposition

Simulate realizations from a multivariate normal distribution using
Cholesky decomposition

## Usage

``` r
rmvnorm(mean, cov, N)
```

## Arguments

- mean:

  The mean vector for the multivariate distribution.

- cov:

  The covariance matrix for the multivariate distribution.

- N:

  The number of realizations to be returned.

## Value

A matrix with columns of realizations of the multivariate normal.

## Examples

``` r
sample <- rmvnorm(mean = rep(0, 4), cov = diag(1, nrow = 4), N = 100)
```
