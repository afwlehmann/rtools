#
# nca.R
# copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
#


#' The gradient of A corresponding to KL divergence.
#'
#' Maximize the probability of obtaining an error-free classification of the
#' entire training set.
#' @references Goldberger et al., "Neighbourhood components analysis", 2004
#' @param X a matrix whose rows represent the original data
#' @param A the current transformation matrix
#' @param P a matrix where \eqn{P_ij} denotes the probability of point
#'  \eqn{X_i} belonging to the same class a \eqn{X_j}
#' @param klass a vector or factor whose i-th element corresponds to the class
#'  of point \eqn{X_i}
#' @return the gradient of A corresponding to KL divergence
nca.grad.kl <- function(X, A, P, klass) {
  N <- nrow(X)
  M <- ncol(X)

  stopifnot(M == nrow(A))
  stopifnot(N == nrow(P) && nrow(P) == ncol(P))

  outerSum <- matrix(0, M, M)
  for (i in 1:N) {
    # Precompute.
    X_minus_xi <- sqrt(P[i,]) * sweep(X, 2, X[i,], "-")
    indices <- klass == klass[i]

    p_i <- sum(P[i, indices])
    leftTerm <- crossprod(X_minus_xi)
    rightTerm <- crossprod(X_minus_xi[indices,])

    outerSum <- outerSum + leftTerm - rightTerm / p_i
  }

  2 * outerSum %*% A
}


#' The gradient of A corresponding to expected number of points.
#'
#' Maximime expected number of points correctly classified.
#' @references Goldberger et al., "Neighbourhood components analysis", 2004
#' @param X a matrix whose rows represent the original data
#' @param A the current transformation matrix
#' @param P a matrix where \eqn{P_ij} denotes the probability of point
#'  \eqn{X_i} belonging to the same class a \eqn{X_j}
#' @param klass a vector or factor whose i-th element corresponds to the class
#'  of point \eqn{X_i}
#' @return the gradient of A corresponding to expected number of points
nca.grad.points <- function(X, A, P, klass) {
  N <- nrow(X)
  M <- ncol(X)

  stopifnot(M == nrow(A))
  stopifnot(N == nrow(P) && nrow(P) == ncol(P))

  outerSum <- matrix(0, M, M)
  for (i in 1:N) {
    # Precompute.
    X_minus_xi <- sqrt(P[i,]) * sweep(X, 2, X[i,], "-")
    indices <- klass == klass[i]

    p_i <- sum(P[i, indices])
    leftTerm <- crossprod(X_minus_xi)
    rightTerm <- crossprod(X_minus_xi[indices,])

    outerSum <- outerSum + p_i * leftTerm - rightTerm
  }

  2 * outerSum %*% A
}


#' Neighbourhood Component Analysis
#' @param X a matrix whose rows represent the original data
#' @param klass a vector or factor whose i-th element corresponds to the class
#'  of point \eqn{X_i}
#' @param d the dimensionality for the result
#' @param maxIter the maximum number of iterations
#' @param lambda the learning rate (usually <<1)
#' @param epsilon the desired accuracy, i.e. once the Frobenius norm of the
#'  gradient becomes less than or equal to \code{epsilon}, convergence is assumed
#' @param algorithm one of \code{c("points", "kl")}
#' @param verb \verb{TRUE} for verbose output, else \verb{FALSE}
#' @return a linear transformation matrix that maps the original data in \code{X}
#'  to a \code{d}-dimensional space
#' @export
nca <- function(X, klass, d=2, maxIter=100, lambda=1e-3, epsilon=1e-2, algorithm="points", verb=T) {
  N <- nrow(X)
  M <- ncol(X)

  stopifnot(is.vector(klass) || is.factor(klass))
  stopifnot(N > 1)
  stopifnot(N %% length(klass) == 0)

  stopifnot(algorithm %in% c("points", "kl"))
  fun <- if (algorithm == "points") nca.grad.points else nca.grad.kl

  # Initial "smart" guess for A, i.e. the square root of the covariance matrix
  # of X, reshaped to M x d. This way, we're starting out with the Mahalanobis
  # distance.
  aux <- svd(X)
  AtA <- aux$v %*% diag(aux$d^2) %*% t(aux$v) / (N-1)
  A <- chol(AtA)[1:M,1:d]
  rm(aux, AtA)

  lastGradA <- matrix(0, M, d)
  for (iter in 1:maxIter) {
    # Compute the probabilities p_ij that x_i has the same class label as x_j.
    P <- exp(-pwSquaredDist(X %*% A, asMatrix=T))
    diag(P) <- 0 # per definition
    P <- P / rowSums(P)

    # Update A.
    gradA <- fun(X, A, P, klass)
    A <- A + lambda * (gradA + lastGradA)
    lastGradA <- gradA

    # Another round?
    cont <- norm(gradA, "F")
    if (verb)
      cat(sprintf("%d (%e)\n", iter, cont))
    if (cont < epsilon)
      break
  }

  # Return the transformation matrix.
  A
}
