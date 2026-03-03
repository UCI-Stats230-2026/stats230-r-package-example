#' Parameter Estimates for Mixture of Two Normal Distributions via EM Algorithm
#'
#' This function estimates the parameters of a two-component Gaussian mixture
#' model using the Expectation-Maximization (EM) algorithm. The model assumes
#' that observations come from a mixture of two normal distributions with known,
#' fixed standard deviations but unknown means and mixing proportion.
#'
#' @param y A numeric vector of observations from which the mixture model will
#'   be estimated. Must contain at least 2 observations.
#' @param alpha_init Numeric scalar between 0 and 1. Initial value for the
#'   mixing proportion of the first component. Default is 0.5 (equal mixture).
#' @param mu1_init Numeric scalar. Initial value for the mean of the first
#'   normal component. Default is the 25th percentile of \code{y}.
#' @param mu2_init Numeric scalar. Initial value for the mean of the second
#'   normal component. Default is the 75th percentile of \code{y}.
#' @param max_iter Positive integer. Maximum number of EM iterations to perform.
#'   Default is 200.
#' @param tol Positive numeric. Convergence tolerance based on parameter
#'   changes. The algorithm stops when the maximum absolute change in parameters
#'   (alpha, mu1, mu2) is less than \code{tol}. Default is 1e-8.
#' @param sigma1 Positive numeric. Fixed standard deviation of the first normal
#'   component. Default is 1.0.
#' @param sigma2 Positive numeric. Fixed standard deviation of the second normal
#'   component. Default is 0.8.
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{alpha}{Estimated mixing proportion for the first component (between
#'     0 and 1).}
#'   \item{mu1}{Estimated mean of the first normal component.}
#'   \item{mu2}{Estimated mean of the second normal component.}
#'   \item{loglik}{Numeric vector of observed-data log-likelihood values at each
#'     iteration.}
#'   \item{converged}{Logical indicating whether the algorithm converged within
#'     \code{max_iter} iterations.}
#'   \item{iterations}{Integer number of iterations performed.}
#' }
#'
#' @details
#' The EM algorithm iteratively performs two steps:
#' \itemize{
#'   \item \strong{E-step}: Computes the posterior probability (responsibility) 
#'     that each observation belongs to the first component, given current 
#'     parameter estimates.
#'   \item \strong{M-step}: Updates the mixing proportion and component means 
#'     by maximizing the expected complete-data log-likelihood.
#' }
#' 
#' The standard deviations (\code{sigma1} and \code{sigma2}) are held fixed 
#' throughout the algorithm. Convergence is assessed based on the maximum 
#' absolute change in parameters between successive iterations.
#' 
#' @import stats
#' 
#' @export
em_mixture_normals <- function(y,
                               alpha_init = 0.5,
                               mu1_init = quantile(y, probs = 0.25),
                               mu2_init = quantile(y, probs = 0.75),
                               max_iter = 200,
                               tol = 1e-8,
                               sigma1 = 1.0,
                               sigma2 = 0.8) {
  # some basic checks on the data and the parameters
  stopifnot(is.numeric(y), length(y) >= 2)
  stopifnot(is.numeric(alpha_init), length(alpha_init) == 1)
  stopifnot(is.numeric(mu1_init), length(mu1_init) == 1)
  stopifnot(is.numeric(mu2_init), length(mu2_init) == 1)
  stopifnot(is.numeric(max_iter), max_iter >= 1)
  stopifnot(is.numeric(tol), tol > 0)

  # helper: observed-data log-likelihood
  obs_loglik <- function(y, alpha, mu1, mu2, s1, s2) {
    comp1 <- alpha * stats::dnorm(y, mean = mu1, sd = s1)
    comp2 <- (1 - alpha) * stats::dnorm(y, mean = mu2, sd = s2)
    sum(log(comp1 + comp2))
  }

  # initialize, taking care of the log(0) issue if alpha_init is exactly 0 or 1
  if (alpha_init == 0) {
    warning("Cannot initialize with alpha_init = 0; resetting to 0.1.")
    alpha_init <- 0.1
  } else if (alpha_init == 1) {
    warning("Cannot initialize with alpha_init = 1; resetting to 0.9.")
    alpha_init <- 0.9
  }
  alpha <- alpha_init
  mu1 <- mu1_init
  mu2 <- mu2_init

  # set up trackers
  loglik <- numeric(max_iter)
  converged <- FALSE

  for (k in seq_len(max_iter)) {
    ## THIS IS THE E STEP ##
    # all we need is that p_i_k from the derivation
    # so that we can compute the expected complete log-likelihood
    # and then maximize it in the M step
    # first get the numerator for p_i = Pr(x_i = 1 | y_i, current params)
    num <- alpha * stats::dnorm(y, mean = mu1, sd = sigma1)
    # then the denominator
    den <- num + (1 - alpha) * stats::dnorm(y, mean = mu2, sd = sigma2)
    p_i_k <- num / den

    ## THIS IS THE M STEP ##
    # we update using the maxima we derived in the Rmd
    alpha_kp1 <- mean(p_i_k)
    mu1_kp1 <- sum(p_i_k * y) / sum(p_i_k)
    mu2_kp1 <- sum((1 - p_i_k) * y) / sum(1 - p_i_k)

    # log-likelihood at current update (after M-step)
    loglik[k] <- obs_loglik(y, alpha_kp1, mu1_kp1, mu2_kp1, sigma1, sigma2)
    # check to see how much params have changed; if very little, we can stop
    delta <- max(abs(alpha_kp1 - alpha), abs(mu1_kp1 - mu1), abs(mu2_kp1 - mu2))
    alpha <- alpha_kp1
    mu1 <- mu1_kp1
    mu2 <- mu2_kp1

    if (!is.finite(loglik[k])) {
      warning("Non-finite log-likelihood encountered; stopping early.")
      loglik <- loglik[seq_len(k)]
      break
    }

    if (delta < tol) {
      converged <- TRUE
      loglik <- loglik[seq_len(k)]
      break
    }
  }

  list(
    alpha = alpha,
    mu1 = mu1,
    mu2 = mu2,
    loglik = loglik,
    converged = converged,
    iterations = length(loglik)
  )
}

#' Forward-Backward Algorithm for the Occasionally Dishonest Casino HMM
#'
#' Runs the forward and backward algorithms for a 2-state HMM (1=fair, 2=loaded)
#' with discrete emissions (die rolls 1-6).
#'
#' @param y Integer vector of observed die rolls, each in \{1,2,3,4,5,6\}.
#' @param initial_dist Numeric length-2 vector giving P(x_1=1) and P(x_1=2). Must sum to 1.
#' @param p_fair_to_loaded Scalar, transition prob P(x_t=2 | x_\{t-1\}=1).
#' @param p_loaded_to_fair Scalar, transition prob P(x_t=1 | x_\{t-1\}=2).
#' @param loaded_probs Numeric length-6 vector of emission probs for loaded state (sum to 1).
#'
#' @return A list with:
#' \describe{
#'   \item{gamma}{T x 2 matrix of smoothed state probabilities P(x_t=i | y_\{1:T\}).}
#'   \item{P}{2 x 2 transition matrix used.}
#'   \item{E}{2 x 6 emission matrix used (rows=states, cols=roll outcomes 1..6).}
#' }
#'
forward_backward_casino <- function(y,
                                    initial_dist = c(0.5, 0.5),
                                    p_fair_to_loaded = 0.02,
                                    p_loaded_to_fair = 0.05,
                                    loaded_probs = c(0.1, 0.1, 0.5,
                                                     0.1, 0.1, 0.1)) {

  T <- length(y)
  if (T < 2) {
    stop("Need at least 2 observations for forward-backward.")
  }

  # get the P and E (transition and emission matrices) for the model
  P <- matrix(c(1 - p_fair_to_loaded, p_fair_to_loaded,
                p_loaded_to_fair,     1 - p_loaded_to_fair),
              nrow = 2, byrow = TRUE)

  E <- rbind(rep(1/6, 6), loaded_probs) 
  # ^^rows: state 1 (fair), state 2 (loaded)

  # forward pass
  alpha <- matrix(0, nrow = T, ncol = 2)
  alpha[1, ] <- initial_dist * E[, y[1]]
  for (t in 2:T) {
    pred <- as.numeric(alpha[t - 1, ] %*% P)
    alpha[t, ] <- E[, y[t]] * pred
  }

  # backward pass
  beta <- matrix(0, nrow = T, ncol = 2)
  beta[T, ] <- c(1, 1)
  for (t in (T - 1):1) {
    beta[t, ] <- as.numeric(P %*% (E[, y[t + 1]] * beta[t + 1, ]))
  }

  alphabeta <- alpha * beta
  gamma <- alphabeta / rowSums(alphabeta)
  list(
    gamma = gamma,
    P = P,
    E = E
  )
}

#' Simulate the Occasionally Dishonest Casino Hidden Markov Model
#'
#' This function simulates a sequence of observations from the
#' "Occasionally Dishonest Casino" hidden Markov model (HMM). The model
#' consists of two states: "Fair" and "Loaded". In the "Fair" state, a
#' six-sided die is rolled with equal probability for each face (1-6). In
#' the "Loaded" state, the die has unequal probabilities for each face
#' (1-6). The state transitions are governed by a Markov process with
#' specified transition probabilities.
#'
#' @param n_obs Positive integer. The number of observations (die rolls)
#'   to simulate. Default is 100.
#' @param initial_dist Numeric vector of length 2. The initial distribution
#' over the states. Must sum to 1. Default is c(0.5, 0.5) (starting in either
#' state with equal probability).
#' @param p_fair_to_loaded Numeric scalar between 0 and 1. Transition
#'   probability from the "Fair" state to the "Loaded" state.
#'   Default is 0.02.
#' @param p_loaded_to_fair Numeric scalar between 0 and 1. Transition
#'   probability from the "Loaded" state to the "Fair" state.
#'   Default is 0.05.
#' @param loaded_probs Numeric vector of length 6. The probabilities of
#'   rolling each face (1-6) when in the "Loaded" state. Must sum to 1.
#'   Default is c(0.1, 0.1, 0.5, 0.1, 0.1, 0.1) (biased towards rolling
#'   a 3).
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{states}{Integer vector of length \code{n_obs} indicating the
#'     hidden state at each time point (1 for "Fair", 2 for "Loaded").}
#'   \item{observations}{Integer vector of length \code{n_obs} containing
#'     the simulated die rolls (values between 1 and 6).}
#' }
#' @import stats
#' @export

occasionally_dishonest_casino <- function(n_obs = 100,
                                          initial_dist = c(0.5, 0.5),
                                          p_fair_to_loaded = 0.02,
                                          p_loaded_to_fair = 0.05,
                                          loaded_probs = c(0.1, 0.1, 0.5,
                                                           0.1, 0.1, 0.1)) {
  # basic checks
  stopifnot(is.numeric(n_obs), n_obs > 0)
  stopifnot(is.numeric(initial_dist), length(initial_dist) == 2,
            all(initial_dist >= 0), sum(initial_dist) == 1)
  stopifnot(is.numeric(p_fair_to_loaded), p_fair_to_loaded >= 0,
            p_fair_to_loaded <= 1)
  stopifnot(is.numeric(p_loaded_to_fair), p_loaded_to_fair >= 0,
            p_loaded_to_fair <= 1)

  # define states,
  states <- integer(n_obs)
  observations <- integer(n_obs)

  # initial state using dist provided
  states[1] <- sample(1:2, size = 1, prob = initial_dist)

  # generate a sequence of hidden states
  for (t in seq_len(n_obs)) {
    if (t > 1) {
      # transition to next state based on current state
      if (states[t - 1] == 1) { # currently fair
        states[t] <- ifelse(runif(1) < p_fair_to_loaded, 2, 1)
      } else { # currently loaded
        states[t] <- ifelse(runif(1) < p_loaded_to_fair, 1, 2)
      }
    }

    # generate observations for each hidden state
    if (states[t] == 1) {
      observations[t] <- sample(1:6, size = 1)
    } else {
      observations[t] <- sample(1:6, size = 1, prob = loaded_probs)
    }
  }

  # internally run forward-backward
  fb <- forward_backward_casino(observations, initial_dist, p_fair_to_loaded,
                                p_loaded_to_fair, loaded_probs)

  list(states = states, observations = observations,
       gamma = fb$gamma)
}
