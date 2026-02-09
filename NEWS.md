#  STATS230 0.2.0 (Homework 2)

## New Features
- Added function [`rmvnorm`](R/homework2.R) to generate random samples from the multivariate normal distribution.
- Added function [`dmvnorm`](R/homework2.R) to compute the density of the multivariate normal distribution using either Cholesky or Eigen decomposition methods.
- Added function [`my_lm`](R/homework2.R) to fit ordinary least squares regression models using Cholesky, QR, or SVD matrix decomposition methods.

## Documentation
- Added [vignette](vignettes/homework2.Rmd) demonstrating the usage of `rmvnorm`, `dmvnorm`, and `my_lm`, along with benchmarking results comparing the computational efficiency of different methods for each function.

# STATS230 0.1.0 (Homework 1)

## New Features
- Added function [`mult_ABx`](R/homework1.R), which multiplies two square matrices and a vector. It has the option of using a more efficient method (A(Bx)) or a less efficient method ((AB)x).
- Added function [`mult_Ainv_c`](R/homework1.R), which computes the product of the inverse of a square matrix and a vector. It has the option of using a more efficient method (solve(A, c)) or a less efficient method (solve(A) %*% c).

## Documentation
- Added [vignette](vignettes/homework1.Rmd) walking through the usage of `mult_ABx` and `mult_Ainv_c`, and comparing computational efficiency of two methods for each.

## Contributors
- @vminin
- @jessalynnsebastian