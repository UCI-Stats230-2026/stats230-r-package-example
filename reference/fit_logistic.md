# Fit Logistic Regression with Multiple Optimization Methods

Fits a logistic regression model using gradient descent, stochastic
gradient descent, or Newton-Raphson optimization.

## Usage

``` r
fit_logistic(
  formula,
  data,
  method = "nr",
  conf_level = 0.95,
  learning_rate = 0.01,
  max_iterations = 1000,
  batch_size = 1,
  tolerance = 1e-08,
  verbose = FALSE,
  standardize = TRUE,
  decay = 0,
  convergence_metric = "ll"
)
```

## Arguments

- formula:

  A formula object specifying the model (e.g., y ~ x1 + x2)

- data:

  A data frame containing the variables in the formula

- method:

  A character string specifying the optimization method: "gd" for
  gradient descent, "sgd" for stochastic gradient descent, "nr" for
  Newton-Raphson (default: "nr" because it's my favorite \<3)

- conf_level:

  Numeric. Confidence level for intervals (default: 0.95)

- learning_rate:

  Numeric. Learning rate for gradient descent and SGD (default: 0.01).
  This is a reasonable starting point but the best choice depends on the
  data scale, amount of regularization (if any), and optimizer; you may
  need to tune it for your problem.

- max_iterations:

  Numeric. Maximum number of iterations (default: 1000)

- batch_size:

  Numeric. Batch size for SGD (default: 1, plain SGD)

- tolerance:

  Numeric. Convergence tolerance for log-likelihood change (default:
  1e-8)

- verbose:

  Logical. If TRUE, print iteration information (default: FALSE)

- standardize:

  Logical. If TRUE, standardize covariates (default: TRUE)

- decay:

  Numeric. Learning rate decay factor for GD and SGD. Learning rate at
  iteration t is: lr_t = lr / (1 + decay \* t). With small decay (e.g.,
  0.001), this has little effect early on but reduces step size in later
  iterations. The amount of decay is problem-dependent and often needs
  tuning; start with 0 (no decay) and increase only if you see unstable
  updates. (default: 0, no decay)

- convergence_metric:

  Character. Convergence criterion to use: "ll" for log-likelihood
  change (default), "beta" for change in coefficient estimates

## Value

A list containing:

- coefficients:

  MLE of coefficient estimates

- conf_int:

  Asymptotic confidence intervals for coefficients

- fitted_values:

  Predicted probabilities on original data

- log_likelihood_history:

  Vector of log-likelihoods at each iteration

- method:

  Optimization method used

- iterations:

  Number of iterations performed

- converged:

  Logical. Whether convergence criterion was met

- formula:

  The formula used
