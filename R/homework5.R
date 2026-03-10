#' Monte Carlo Estimation of Moments
#'
#' Estimates $\eqn{E(X^k)}$ for a random variable $X$ using either
#' naive Monte Carlo sampling from the target distribution or
#' importance sampling.
#'
#' @param rv_distribution A list describing the target distribution.
#'   For `type = "naive"`, it must include `sampler`, a function with argument
#'   `n` that returns random draws from the target.
#'   For `type = "importance"`, it must include `density`, a function with
#'   argument `x` that evaluates the target density at `x`.
#' @param moment Numeric scalar giving the moment order $k$ in
#'   $\eqn{E(X^k)}$. Default is `1`.
#' @param type Character string indicating estimator type:
#'   `"importance"` (default) or `"naive"`.
#' @param n_samples Positive numeric scalar. Number of Monte Carlo samples.
#'   Default is `1000`.
#' @param ci_level Numeric scalar in `(0, 1)`. Confidence level used for the
#'   normal-approximation confidence interval. Default is `0.95`.
#' @param instrument_distribution A list describing the proposal distribution
#'   used when `type = "importance"`. Must include `sampler`, a function with
#'   argument `n` that returns proposal draws, and `density`, a function with
#'   argument `x` that evaluates the proposal density.
#'
#' @return A list with:
#' \describe{
#'   \item{estimate}{Numeric scalar estimate of the requested moment.}
#'   \item{mc_error}{Estimated Monte Carlo standard error.}
#'   \item{conf_int}{Length-2 numeric vector with lower and upper confidence
#'   bounds.}
#' }
#'
#' @import stats
#' @export
montecarlo_moments <- function(
    rv_distribution = list(
      sampler = function(n) rgamma(n, shape = 4, rate = 2),
      density = function(x) dgamma(x, shape = 4, rate = 2)
    ),
    moment = 1,
    type = c("importance", "naive"),
    n_samples = 1000,
    ci_level = 0.95,
    instrument_distribution = list(
      sampler = function(n) rlnorm(n, meanlog = log(2) - log(1.25)^2 / 2, sdlog = log(1.25)),
      density = function(x) dlnorm(x, meanlog = log(2) - log(1.25)^2 / 2, sdlog = log(1.25))
    )) {
  type <- match.arg(type, choices = c("importance", "naive"))

  if (!is.numeric(n_samples) || length(n_samples) != 1 || n_samples <= 0) {
    stop("`n_samples` invalid.")
  }
  if (!is.numeric(moment) || length(moment) != 1) {
    stop("`moment` invalid.")
  }
  if (!is.numeric(ci_level) || length(ci_level) != 1 ||
        ci_level <= 0 || ci_level >= 1) {
    stop("`ci_level` invalid.")
  }

  zcrit <- stats::qnorm((1 + ci_level) / 2)

  if (type == "naive") {
    sampler <- rv_distribution$sampler
    if (is.null(sampler)) {
      stop("`rv_distribution` needs sampler function.")
    }
    samples <- do.call(sampler, c(list(n = n_samples)))
    h <- samples^moment
    estimate <- mean(h)
    mc_error <- stats::sd(h) / sqrt(n_samples)
    conf_int <- c(estimate - zcrit * mc_error, estimate + zcrit * mc_error)
    names(conf_int) <- c("lower", "upper")

    return(list(
      estimate = estimate,
      mc_error = mc_error,
      conf_int = conf_int
    ))
  } else if (type == "importance") {
    sampler <- instrument_distribution$sampler
    if (is.null(sampler)) {
      stop("`instrument_distribution` needs sampler function.")
    }
    instrument_density <- instrument_distribution$density
    if (is.null(instrument_density)) {
      stop("`instrument_distribution` needs density function.")
    }
    samples <- do.call(sampler, c(list(n = n_samples)))
    density_func <- rv_distribution$density
    if (is.null(density_func)) {
      stop("`rv_distribution` needs density function.")
    }
    weights <- do.call(density_func, c(list(x = samples))) /
      do.call(instrument_density, c(list(x = samples)))

    h <- samples^moment
    weighted_terms <- weights * h
    estimate <- mean(weighted_terms)
    mc_error <- stats::sd(weighted_terms) / sqrt(n_samples)
    conf_int <- c(estimate - zcrit * mc_error, estimate + zcrit * mc_error)
    names(conf_int) <- c("lower", "upper")

    return(list(
      estimate = estimate,
      mc_error = mc_error,
      conf_int = conf_int
    ))
  } else {
    stop("Invalid `type`.")
  }
}


#' Wright-Fisher Model Path Simulation
#'
#' Simulates a path of allele frequencies under the Wright-Fisher model
#' with mutation.
#'
#' @param n_steps Integer. Number of time steps to simulate.
#' @param x0 Integer. Initial number of copies of the allele (must be between 0 and N).
#' @param N Integer. Population size (number of alleles). Default is 100.
#' @param u Numeric. Mutation rate from a to A.
#'   Default is 0.35.
#' @param v Numeric. Mutation rate from allele A to a.
#'   Default is 0.5.
#'
#' @return Numeric vector of length n_steps + 1 containing the number of copies of
#'   the allele at each time step, starting with x0.
#'
#' @export
wright_fisher_path <- function(n_steps, x0, N = 100, u = 0.35, v = 0.5) {
  x <- numeric(n_steps + 1)
  x[1] <- x0
  for (t in 2:(n_steps + 1)) {
    i <- x[t - 1]
    q_i <- (i / N) * (1 - v) + (1 - i / N) * u
    x[t] <- rbinom(1, size = N, prob = q_i)
  }
  return(x)
}