# Monte Carlo Estimation of Moments

Estimates \$\\E(X^k)\\\$ for a random variable \$X\$ using either naive
Monte Carlo sampling from the target distribution or importance
sampling.

## Usage

``` r
montecarlo_moments(
  rv_distribution = list(sampler = function(n) rgamma(n, shape = 4, rate = 2), density =
    function(x) dgamma(x, shape = 4, rate = 2)),
  moment = 1,
  type = c("importance", "naive"),
  n_samples = 1000,
  ci_level = 0.95,
  instrument_distribution = list(sampler = function(n) rlnorm(n, meanlog = log(2) -
    log(1.25)^2/2, sdlog = log(1.25)), density = function(x) dlnorm(x, meanlog = log(2) -
    log(1.25)^2/2, sdlog = log(1.25)))
)
```

## Arguments

- rv_distribution:

  A list describing the target distribution. For `type = "naive"`, it
  must include `sampler`, a function with argument `n` that returns
  random draws from the target. For `type = "importance"`, it must
  include `density`, a function with argument `x` that evaluates the
  target density at `x`.

- moment:

  Numeric scalar giving the moment order \$k\$ in \$\\E(X^k)\\\$.
  Default is `1`.

- type:

  Character string indicating estimator type: `"importance"` (default)
  or `"naive"`.

- n_samples:

  Positive numeric scalar. Number of Monte Carlo samples. Default is
  `1000`.

- ci_level:

  Numeric scalar in `(0, 1)`. Confidence level used for the
  normal-approximation confidence interval. Default is `0.95`.

- instrument_distribution:

  A list describing the proposal distribution used when
  `type = "importance"`. Must include `sampler`, a function with
  argument `n` that returns proposal draws, and `density`, a function
  with argument `x` that evaluates the proposal density.

## Value

A list with:

- estimate:

  Numeric scalar estimate of the requested moment.

- mc_error:

  Estimated Monte Carlo standard error.

- conf_int:

  Length-2 numeric vector with lower and upper confidence bounds.
