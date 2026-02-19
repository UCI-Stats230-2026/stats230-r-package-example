#' Fit Logistic Regression with Multiple Optimization Methods
#'
#' Fits a logistic regression model using gradient descent, stochastic
#' gradient descent, or Newton-Raphson optimization.
#'
#' @param formula A formula object specifying the model (e.g., y ~ x1 + x2)
#' @param data A data frame containing the variables in the formula
#' @param method A character string specifying the optimization method:
#'   "gd" for gradient descent,
#'   "sgd" for stochastic gradient descent,
#'   "nr" for Newton-Raphson
#'  (default: "nr" because it's my favorite <3)
#' @param conf_level Numeric. Confidence level for intervals (default: 0.95)
#' @param learning_rate Numeric. Learning rate for gradient descent and SGD
#'   (default: 0.01). This is a reasonable starting point but the best
#'   choice depends on the data scale, amount of regularization (if any),
#'   and optimizer; you may need to tune it for your problem.
#' @param max_iterations Numeric. Maximum number of iterations
#'   (default: 1000)
#' @param batch_size Numeric. Batch size for SGD (default: 1, plain SGD)
#' @param tolerance Numeric. Convergence tolerance for log-likelihood change
#'   (default: 1e-8)
#' @param verbose Logical. If TRUE, print iteration information
#'   (default: FALSE)
#' @param standardize Logical. If TRUE, standardize covariates (default: TRUE)
#' @param decay Numeric. Learning rate decay factor for GD and SGD.
#'   Learning rate at iteration t is: lr_t = lr / (1 + decay * t).
#'   With small decay (e.g., 0.001), this has little effect early on but
#'   reduces step size in later iterations. The amount of decay is
#'   problem-dependent and often needs tuning; start with 0 (no decay)
#'   and increase only if you see unstable updates. (default: 0, no decay)
#' @param convergence_metric Character. Convergence criterion to use:
#'   "ll" for log-likelihood change (default),
#'   "beta" for change in coefficient estimates
#'
#' @return A list containing:
#'   \item{coefficients}{MLE of coefficient estimates}
#'   \item{conf_int}{Asymptotic confidence intervals for coefficients}
#'   \item{fitted_values}{Predicted probabilities on original data}
#'   \item{log_likelihood_history}{Vector of log-likelihoods at each
#'     iteration}
#'   \item{method}{Optimization method used}
#'   \item{iterations}{Number of iterations performed}
#'   \item{converged}{Logical. Whether convergence criterion was met}
#'   \item{formula}{The formula used}
#'
#' @export

fit_logistic <- function(
    formula, data, method = "nr",
    conf_level = 0.95,
    learning_rate = 0.01,
    max_iterations = 1000, batch_size = 1,
    tolerance = 1e-8, verbose = FALSE, standardize = TRUE,
    decay = 0, convergence_metric = "ll") {
  mf <- model.frame(formula, data = data)
  y <- model.response(mf)

  # do some checking of y
  if (is.factor(y)) {
    if (nlevels(y) != 2) {
      stop("response must have 2 levels for logistic regression")
    }
    y <- as.integer(y == levels(y)[2])
  } else if (is.logical(y)) {
    y <- as.integer(y)
  } else {
    y <- as.numeric(y)
  }
  if (any(is.na(y)) || any(!(y %in% c(0, 1)))) {
    stop("response must be coded as 0/1 (or a 2-level factor/logical).")
  }

  # keep original x but then scale the oe for optimization
  x_matrix_orig <- model.matrix(formula, data = data)
  x_matrix <- x_matrix_orig
  cols_to_scale <- integer(0)
  scale_center <- NULL
  scale_scale <- NULL

  if (standardize && ncol(x_matrix) > 1) {
    candidate_cols <- 2:ncol(x_matrix) # never scale intercept
    is_indicator_01 <- vapply(candidate_cols, function(j) {
      v <- x_matrix[, j]
      u <- unique(v[!is.na(v)])
      length(u) > 0 && all(u %in% c(0, 1))
    }, logical(1))

    cols_to_scale <- candidate_cols[!is_indicator_01]

    if (length(cols_to_scale) > 0) {
      scaled_x <- scale(x_matrix[, cols_to_scale, drop = FALSE])
      scale_center <- attr(scaled_x, "scaled:center")
      scale_scale <- attr(scaled_x, "scaled:scale")
      x_matrix[, cols_to_scale] <- scaled_x
    }
  }

  ##### Helper functions #####

  # 1. sigmoid (or i call it expit)
  sigmoid <- function(z) 1 / (1 + exp(-z))

  # 2. compute log-likelihood
  compute_log_likelihood <- function(beta, X, y) {
    probs <- sigmoid(X %*% beta)
    sum(y * log(probs) + (1 - y) * log(1 - probs))
  }

  # 3. convergence check
  converged_now <- function(metric, ll_curr, ll_prev,
                            beta_curr, beta_prev, tol) {
    if (metric == "ll") return(abs(ll_curr - ll_prev) < tol)
    if (metric == "beta") return(sum(abs(beta_curr - beta_prev)) < tol)
    stop("convergence_metric must be 'll' or 'beta'")
  }

  # 4. do backtracking for gd/nr because we have the full data ll
  backtrack_full_ll <- function(beta, step_dir, ll_curr,
                                x_matrix, y,
                                alpha0 = 1, min_alpha = 1e-8,
                                shrink = 0.5) {
    alpha <- alpha0
    while (alpha >= min_alpha) {
      beta_try <- beta - alpha * step_dir
      ll_try <- compute_log_likelihood(beta_try, x_matrix, y)
      if (is.finite(ll_try) && ll_try >= ll_curr) {
        return(list(beta_new = beta_try, ll_new = ll_try, alpha = alpha))
      }
      alpha <- alpha * shrink
    }
    list(beta_new = beta, ll_new = ll_curr, alpha = 0)
  }

  # 5. gradient descent update
  update_gd <- function(beta, i, x_matrix, y, n, learning_rate, decay) {
    ll_curr <- compute_log_likelihood(beta, x_matrix, y)
    probs <- sigmoid(x_matrix %*% beta)
    g <- (t(x_matrix) %*% (probs - y)) / n
    lr_t <- learning_rate / (1 + decay * i)
    step_dir <- lr_t * g

    out <- backtrack_full_ll(beta, step_dir, ll_curr, x_matrix, y, alpha0 = 1)
    list(beta_new = out$beta_new, ll_new = out$ll_new)
  }

  # 6. Newton-Raphson update
  update_nr <- function(beta, i) {
    ll_curr <- compute_log_likelihood(beta, x_matrix, y)

    probs <- sigmoid(x_matrix %*% beta)
    g <- t(x_matrix) %*% (probs - y)
    w <- as.vector(probs * (1 - probs))
    H <- crossprod(x_matrix, x_matrix * w)

    # plain Newton step via solve
    step <- solve(H, g)

    out <- backtrack_full_ll(beta, step, ll_curr, x_matrix, y, alpha0 = 1)
    list(beta_new = out$beta_new, ll_new = out$ll_new)
  }

  # 7. SGD update function
  update_sgd <- function(beta, i) {
    idx <- sample(n, batch_size, replace = TRUE)
    Xb <- x_matrix[idx, , drop = FALSE]
    yb <- y[idx]

    probs <- sigmoid(Xb %*% beta)
    g <- (t(Xb) %*% (probs - yb)) / batch_size

    lr_t <- learning_rate / (1 + decay * i)

    step_dir <- lr_t * as.vector(g)

    beta_new <- beta - step_dir
    ll_new <- compute_log_likelihood(beta_new, x_matrix, y) # keep history
    list(beta_new = beta_new, ll_new = ll_new)
  }

  # overall optimization loop that runs the chosen updater
  run_optimizer_loop <- function(beta0, update_fn, max_iterations,
                                 verbose_every,
                                 tol, metric,
                                 pass_extra_args = TRUE) {
    beta <- beta0
    ll_values <- numeric(max_iterations)
    converged <- FALSE

    for (i in seq_len(max_iterations)) {
      if (pass_extra_args) {
        out <- update_fn(beta, i, x_matrix, y, n, learning_rate, decay)
      } else {
        out <- update_fn(beta, i)
      }
      beta_new <- as.vector(out$beta_new)
      ll_new <- out$ll_new
      ll_values[i] <- ll_new

      if (i > 1) {
        if (converged_now(metric, ll_new, ll_values[i - 1],
                          beta_new, beta, tol)) {
          converged <- TRUE
          return(list(beta = beta_new,
                      ll_history = ll_values[seq_len(i)],
                      iterations = i,
                      converged = converged))
        }
      }

      if (verbose && (i %% verbose_every == 0)) {
        delta_beta <- sum(abs(beta_new - beta))
        ll_change <- if (i > 1) ll_new - ll_values[i - 1] else NA_real_
        lr_display <- if (decay > 0) learning_rate / (1 + decay * i) else NA_real_
        cat("Iteration", i, "| LL change:", ll_change, "| Beta change:", delta_beta)
        if (decay > 0) cat(" | Learning rate:", lr_display)
        cat("\n")
      }

      beta <- beta_new
    }

    warning("maximum iterations reached without convergence")
    list(beta = beta,
          ll_history = ll_values,
         iterations = max_iterations,
         converged = converged)
  }

  # run the loop w the optimizer function
  p <- ncol(x_matrix)
  n <- nrow(x_matrix)
  beta0 <- rep(0, p)

  if (method == "gd") {
    result <- run_optimizer_loop(beta0, update_gd, max_iterations,
                                  verbose_every = 100 * verbose,
                                  tol = tolerance, metric = convergence_metric,
                                  pass_extra_args = TRUE)
  } else if (method == "sgd") {
    if (convergence_metric != "beta") {
      warning("SGD implementation only supports 'beta' convergence metric. ",
              "Using 'beta' instead.")
    }
    result <- run_optimizer_loop(beta0, update_sgd, max_iterations,
                                  verbose_every = 100 * verbose,
                                  tol = tolerance, metric = "beta",
                                  pass_extra_args = FALSE)
  } else if (method == "nr") {
    if (convergence_metric != "ll") {
      warning("Newton-Raphson implementation only supports 'll' convergence metric. ",
              "Using 'll' instead.")
    }
    result <- run_optimizer_loop(beta0, update_nr, max_iterations,
                                  verbose_every = 10 * verbose,
                                  tol = tolerance, metric = "ll",
                                  pass_extra_args = FALSE)
  } else {
    stop("method must be 'gd', 'sgd', or 'nr'")
  }

  ## get results to store/return
  beta <- as.vector(result$beta)
  ll_history <- result$ll_history
  iterations <- result$iterations
  converged <- result$converged

  # rescale betas back
  if (standardize && !is.null(scale_scale) && length(cols_to_scale) > 0) {
    beta_rescaled <- beta
    beta_rescaled[cols_to_scale] <- beta[cols_to_scale] / scale_scale
    beta_rescaled[1] <- beta[1] - sum(beta[cols_to_scale]
                                      * scale_center / scale_scale)
    beta <- beta_rescaled
  }

  # wald asymptotic confidence intervals
  z_alpha <- qnorm(1 - (1 - conf_level) / 2)
  probs_final <- sigmoid(x_matrix_orig %*% beta)
  w_final <- as.vector(probs_final * (1 - probs_final))
  H_final <- crossprod(x_matrix_orig, x_matrix_orig * w_final)

  vcov_beta <- solve(H_final)
  se_beta <- sqrt(diag(vcov_beta))

  conf_int <- cbind(
    lower = beta - z_alpha * se_beta,
    upper = beta + z_alpha * se_beta
  )
  rownames(conf_int) <- colnames(x_matrix_orig)

  # return the list of everything
  list(
    coefficients = setNames(as.vector(beta), colnames(x_matrix_orig)),
    conf_int = conf_int,
    fitted_values = as.vector(sigmoid(x_matrix_orig %*% beta)),
    log_likelihood_history = ll_history,
    method = method,
    iterations = iterations,
    converged = converged,
    formula = formula
  )
}
