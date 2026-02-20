# Use Cholesky decomposition, QR decomposition, or Singular Value Decomposition to get OLS estimates

Use Cholesky decomposition, QR decomposition, or Singular Value
Decomposition to get OLS estimates

## Usage

``` r
my_lm(formula, data, method = c("chol", "qr", "svd"), se = FALSE)
```

## Arguments

- formula:

  A formula object of the form y ~ x1 + x2 + ... that specifies the
  model to be fit.

- data:

  The dataframe containing the data for the OLS fit, including a column
  of 1s if an intercept is desired.

- method:

  Methods available are "chol" for Cholesky decomposition, "qr" for QR
  decomposition, and "svd" for Singular Value Decomposition.

- se:

  A boolean, TRUE to return standard errors for coefficients. Defaults
  to FALSE.

## Value

A named vector containing coefficient names and their corresponding
estimates. If se = TRUE, returns a list with elements "coefficients" and
"standard_errors".
