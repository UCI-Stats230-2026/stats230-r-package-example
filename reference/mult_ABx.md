# Multiply two square matrices and a vector

Multiply two square matrices and a vector

## Usage

``` r
mult_ABx(A, B, x, slow = FALSE)
```

## Arguments

- A:

  A square matrix.

- B:

  Another square matrix.

- x:

  A vector.

- slow:

  A boolean indicating whether to multiply using the slower method,
  \\(AB)x\\, or the faster method, \\A(Bx)\\ (default).

## Value

A numeric vector or column/row vector (\\1\times n\\ or \\n \times 1\\
matrix).

## Examples

``` r
mult_ABx(matrix(rnorm(25), nrow = 5), matrix(runif(25, 0, 10), nrow = 5), rpois(5, 5))
#>            [,1]
#> [1,] -178.42850
#> [2,]  -51.39040
#> [3,]  159.44892
#> [4,]  -85.33383
#> [5,] -205.34149
```
