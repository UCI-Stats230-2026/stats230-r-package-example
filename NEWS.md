# STATS230 0.5.0 (Homework 5)

## New Features
- Added function [`montecarlo_moments`](R/homework5.R) for estimating moments with either naive Monte Carlo or importance sampling, including Monte Carlo standard errors and confidence intervals.
- Added function [`wright_fisher_path`](R/homework5.R) to simulate Wright-Fisher Markov chain paths with mutation.

## Documentation
- Added [vignette](vignettes/homework5.Rmd) covering Monte Carlo moment estimation and Wright-Fisher Markov chain analysis, including a derivation of the stationary mean and numerical verification.

# STATS230 0.4.0 (Homework 4)

## New Features
- Added function [`em_mixture_normals`](R/homework4.R) which uses an expectation-maximization (EM) algorithm to estimate MLEs for parameters of a mixture of two normal distributions given some observations.
- Added function [`occasionally_dishonest_casino`](R/homework4.R) which simulates a hidden Markov model (HMM) in which we observe the rolls of one of two dice: a loaded die, and a fair die, but we do not know which one is being used when. The function returns the hidden state of the die, the observed rolls, and the result of running the forward and backward algorithm on the observed rolls to estimate probabilities of the hidden state at each time.

## Documentation
- Added [vignette](vignettes/homework4.Rmd) in which there are two parts: in the first, the EM update formulas are derived and the implementation in `em_mixture_normals` is tested, and in the second, an HMM is simulated and the forward and backward algorithms are used to estimate marginal probabilities of the hidden state.

#  STATS230 0.3.0 (Homework 3)

## New Features
- Added function [`fit_logistic`](R/homework3.R) which uses three different optimization methods (Newton-Raphson (`nr`), Gradient Descent (`gd`), and Stochastic Gradient descent (`sgd`)) to compute MLEs for logistic regression coefficients.

## Documentation
- Added [vignette](vignettes/homework3.Rmd) demonstrating the usage of `fit_logistic` and plotting/benchmarking convergence.

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