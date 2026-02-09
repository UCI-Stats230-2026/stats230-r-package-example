#' Simulate realizations from a multivariate normal distribution using
#' Cholesky decomposition
#'
#' @param mean The mean vector for the multivariate distribution.
#' @param cov The covariance matrix for the multivariate distribution.
#' @param N The number of realizations to be returned.
#'
#' @returns A matrix with columns of realizations of the multivariate normal.
#' @examples
#' sample <- rmvnorm(mean = rep(0, 4), cov = diag(1, nrow = 4), N = 100)
#' @import stats
#' @export

rmvnorm <- function(mean, cov, N) {
  if (length(mean) != nrow(cov) || nrow(cov) != ncol(cov)) {
    stop("The dimensions of 'mean' and 'cov' do not match.")
  }
  eig_vals <- eigen(cov)$values
  if (min(eig_vals) <= 0 || !isSymmetric(cov)) {
    stop("'cov' is not a valid covariance matrix.")
  }
  # chol() gives the upper triangular matrix in the decomp,
  # the equivalent of our L transpose
  L_T <- chol(cov)
  Z_vec_list <- lapply(rep(0, N), function(x) stats::rnorm(length(mean), x))
  x_mat <- sapply(Z_vec_list, function(x) mean + t(L_T) %*% x)
  return(x_mat)
}

#' Evaluate a multivariate normal (log-)density function at a specified point
#' using Cholesky or eigen decomposition
#'
#' @param x A vector, the point at which to evaluate the density.
#' @param mean The mean vector for the multivariate distribution.
#' @param cov The covariance matrix for the multivariate distribution.
#' @param method The method to use for matrix decomposition, either "chol" for
#' Cholesky decomposition or "eigen" for eigen decomposition. Defaults to "chol".
#' @param log A boolean, TRUE to return log-density and FALSE to
#' return the density. Defaults to TRUE.
#'
#' @returns A single numeric value, the (log-)density at the specified point.
#' @examples
#' density <- dmvnorm(x = rep(0, 4), mean = rep(0, 4), cov = diag(1, nrow = 4))
#' @import stats
#' @export

dmvnorm <- function(x, mean, cov, method = c("chol", "eigen"), log = TRUE) {
  method <- match.arg(method)
  if (length(x) != length(mean) || length(mean) != nrow(cov) || nrow(cov) != ncol(cov)) {
    stop("The dimensions of 'x', 'mean', and 'cov' do not match.")
  }
  eig <- eigen(cov)
  if (min(eig$values) <= 0 || !isSymmetric(cov)) {
    stop("'cov' is not a valid covariance matrix.")
  }
  d <- length(mean)
  if (method == "chol") {
    L <- t(chol(cov))
    u <- forwardsolve(L, x - mean)
    log_dens <- -0.5 * d * log(2 * pi) - sum(log(diag(L))) - 0.5 * sum(u^2)
  } else if (method == "eigen") {
    # already computed eigen to check cov
    values <- eig$values
    vectors <- eig$vectors
    u <- crossprod(vectors, x - mean)
    log_dens <- -0.5 * d * log(2 * pi) - 0.5 * sum(log(values)) - 0.5 * sum((u^2) / values)
  }
  if (log) {
    return(log_dens)
  } else {
    return(exp(log_dens))
  }
}

#' Use Cholesky decomposition, QR decomposition, or Singular Value Decomposition
#' to get OLS estimates
#'
#' @param formula A formula object of the form y ~ x1 + x2 + ... that specifies
#'  the model to be fit.
#' @param data The dataframe containing the data for the OLS fit, including a
#'   column of 1s if an intercept is desired.
#' @param method Methods available are "chol" for Cholesky decomposition, "qr"
#'   for QR decomposition, and "svd" for Singular Value Decomposition.
#' @param se A boolean, TRUE to return standard errors for coefficients.
#'   Defaults to FALSE.
#'
#' @returns A named vector containing coefficient names and their corresponding
#'   estimates. If se = TRUE, returns a list with elements "coefficients" and
#'   "standard_errors".
#' @export

my_lm <- function(formula, data, method = c("chol", "qr", "svd"), se = FALSE) {
  method <- match.arg(method)
  X <- model.matrix(formula, data)
  y <- model.response(model.frame(formula, data))
  n <- nrow(X)
  p <- ncol(X)

  # Store decomposition objects to avoid recomputation
  decomp <- switch(method,
    "chol" = {
      XTX <- crossprod(X)
      list(XTX = XTX, L = chol(XTX))
    },
    "qr" = {
      list(qr_obj = qr(X))
    },
    "svd" = {
      list(SVD = svd(X))
    }
  )

  beta <- switch(method,
    "chol" = {
      XTy <- crossprod(X, y)
      a <- forwardsolve(t(decomp$L), XTy)
      as.vector(backsolve(decomp$L, a))
    },
    "qr" = {
      Q <- qr.Q(decomp$qr_obj)
      R <- qr.R(decomp$qr_obj)
      as.vector(backsolve(R, crossprod(Q, y)))
    },
    "svd" = {
      as.vector(decomp$SVD$v %*% (crossprod(decomp$SVD$u, y) / decomp$SVD$d))
    }
  )

  names(beta) <- colnames(X)

  if (se) {
    residuals <- y - X %*% beta
    sigma_sq <- sum(residuals^2) / (n - p)

    var_beta <- switch(method,
      "chol" = {
        L_inv <- backsolve(decomp$L, diag(p))
        sigma_sq * rowSums(L_inv^2)
      },
      "qr" = {
        R <- qr.R(decomp$qr_obj)
        R_inv <- backsolve(R, diag(p))
        sigma_sq * rowSums(R_inv^2)
      },
      "svd" = {
        V_scaled <- sweep(decomp$SVD$v, 2, decomp$SVD$d, "/")
        sigma_sq * rowSums(V_scaled^2)
      }
    )

    std_errors <- sqrt(var_beta)
    names(std_errors) <- colnames(X)

    return(list(coefficients = beta, standard_errors = std_errors))
  }

  return(beta)
}
