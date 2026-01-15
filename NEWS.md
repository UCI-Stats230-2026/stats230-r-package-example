# STATS230 0.1.0 (Homework 1)

## New Features
- Added function [`mult_ABx`](R/homework1.R), which multiplies two square matrices and a vector. It has the option of using a more efficient method (A(Bx)) or a less efficient method ((AB)x).
- Added function [`mult_Ainv_c`](R/homework1.R), which computes the product of the inverse of a square matrix and a vector. It has the option of using a more efficient method (solve(A, c)) or a less efficient method (solve(A) %*% c).

## Documentation
- Added [vignette](vignettes/homework1.Rmd) walking through the usage of `mult_ABx` and `mult_Ainv_c`, and comparing computational efficiency of two methods for each.

## Contributors
- @vminin
- @jessalynnsebastian