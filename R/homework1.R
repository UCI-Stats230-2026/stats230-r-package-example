#' Multiply two square matrices and a vector
#'
#' @param A A square matrix.
#' @param B Another square matrix.
#' @param x A vector.
#' @param slow A boolean indicating whether to multiply using the slower method,
#'   \eqn{(AB)x}, or the faster method, \eqn{A(Bx)} (default).
#'
#' @returns A numeric vector or column/row vector (\eqn{1\times n} or \eqn{n \times 1}
#'   matrix).
#' @examples
#' mult_ABx(matrix(rnorm(25), nrow = 5), matrix(runif(25, 0, 10), nrow = 5), rpois(5, 5))
#' @export

mult_ABx <- function(A, B, x, slow = FALSE) {
  if (slow == FALSE) result <- try(A %*% (B %*% x), silent = TRUE)
  else if (slow == TRUE) result <- try((A %*% B) %*% x, silent = TRUE)
  else stop("Specify multiplication method as slow = TRUE or = FALSE (default).")

  if (!("try-error" %in% class(result))) {
    if (is.vector(c) && is.numeric(c) && !is.matrix(c)) return(drop(result))
    return(result)
  }

  # only do the below if an error is thrown
  if (!is.matrix(A) || !is.matrix(B)) stop("A and B must both be matrices.")
  if ((dim(A)[1] != dim(A)[2]) || (dim(A)[1] != dim(B)[2]) || (dim(B)[1] != dim(A)[2])) {
    stop("A and B must be square matrices of the same dimensions.")
  }
  if (is.matrix(x)) {
    if (dim(B)[2] != dim(x)[1]) {
      if (dim(B)[2] == dim(x)[2]) {
        x <- t(x)
        message("Note: x was transposed to be conformable with A and B.")
        return(mult_ABx(A, B, x, slow))
      } else stop("Incorrect dimensions of vector x.")
    }
  } else if (is.vector(x) && is.numeric(x)) {
    if (dim(B)[2] != length(x)) stop("Incorrect dimensions of vector x.")
  } else {
    stop("x must be a vector or 1 x n matrix.")
  }
}

#' Multiply the inverse of a square matrix by a vector
#'
#' @param A A square matrix.
#' @param c A vector.
#' @param direct A boolean indicating whether to solve the system directly using
#'   \eqn{Ax = c} (default), or compute \eqn{A^{-1}c} by first inverting A.
#'
#' @returns A numeric vector or column/row vector (\eqn{1\times n} or \eqn{n \times 1}
#'   matrix).
#' @examples
#' mult_Ainv_c(matrix(rnorm(25), nrow = 5), rpois(5, 5))
#' @export

mult_Ainv_c <- function(A, c, direct = TRUE) {
  if (direct == TRUE) result <- try(solve(A, c), silent = TRUE)
  else if (direct == FALSE) result <- try(solve(A) %*% c, silent = TRUE)
  else stop("Specify computation method as direct = TRUE (default) or = FALSE.")

  if (!("try-error" %in% class(result))) {
    if (is.vector(c) && is.numeric(c) && !is.matrix(c)) return(drop(result))
    return(result)
  }

  # only do the below if an error is thrown
  if (!is.matrix(A)) stop("A must be a matrix.")
  if ((dim(A)[1] != dim(A)[2])) {
    stop("A must be a square matrix.")
  }
  if (is.matrix(c)) {
    if (dim(A)[1] != dim(c)[1]) {
      if (dim(A)[1] == dim(c)[2]) {
        c <- t(c)
        message("Note: c was transposed to be conformable with A.")
        return(mult_Ainv_c(A, c, direct))
      } else stop("Incorrect dimensions of vector c.")
    }
  } else if (is.vector(c) && is.numeric(c)) {
    if (dim(A)[1] != length(c)) stop("Incorrect dimensions of vector c.")
  } else {
    stop("c must be a vector or n x 1 matrix.")
  }
}