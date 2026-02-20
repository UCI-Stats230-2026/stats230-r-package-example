# Multiply the inverse of a square matrix by a vector

Multiply the inverse of a square matrix by a vector

## Usage

``` r
mult_Ainv_c(A, c, direct = TRUE)
```

## Arguments

- A:

  A square matrix.

- c:

  A vector.

- direct:

  A boolean indicating whether to solve the system directly using \\Ax =
  c\\ (default), or compute \\A^{-1}c\\ by first inverting A.

## Value

A numeric vector or column/row vector (\\1\times n\\ or \\n \times 1\\
matrix).

## Examples

``` r
mult_Ainv_c(matrix(rnorm(25), nrow = 5), rpois(5, 5))
#> [1]  16.037264  -5.711019  10.547692 -33.796937   6.922963
```
