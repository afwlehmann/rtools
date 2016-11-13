#
# matrix.R
# copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
#


#' A lower triangular matrix (incl. the diagonal) from a given vector.
#'
#' @param x the vector
#' @param n the number of rows/columns
#' @return a lower triangular matrix with values corresponding to \code{x}
#' @examples
#'  genLowerTriMatrix(1:10, 4)
#' @export
genLowerTriMatrix <- function(x, n) {
  stopifnot(is.vector(x))

  lx <- n*n - n*(n-1)/2
  if (lx != length(x))
    stop(sprintf("x must be a vector of length %d!\n", lx))

  m <- matrix(0, ncol=n, nrow=n)
  m[lower.tri(m, diag=TRUE)] <- x
  m
}


#' An upper triangular matrix (incl. the diagonal) from a given vector.
#'
#' @param x the vector
#' @param n the number of rows/columns
#' @return an upper triangular matrix with values corresponding to \code{x}
#' @examples
#'  genUpperTriMatrix(1:10, 4)
#' @export
genUpperTriMatrix <- function(x, n) {
  stopifnot(is.vector(x))
  lx <- n*n - n*(n-1)/2
  if (lx != length(x))
    stop(sprintf("x must be a vector of length %d!\n", lx))
  m <- matrix(0, ncol=n, nrow=n)
  m[upper.tri(m, diag=TRUE)] <- x
  m
}


#' A shuffled copy of the given dataset \code{X}.
#'
#' A copy of the given dataset whose rows have been shuffled in random order.
#' 
#' @param X a matrix or data.frame whose rows are supposed to be shuffled
#' @return a shuffled copy of X
#' @export
shuffleRows <- function(X)
  X[sample(1:nrow(X), nrow(X)),]


#' A shuffled copy of the given dataset \code{X}.
#'
#' A copy of the given dataset whose columns have been shuffled in random order.
#'
#' @param X matrix or data.frame whose columns are supposed to be shuffled
#' @return a shuffled copy of X
#' @export
shuffleColumns <- function(X)
  X[, sample(1:ncol(X), ncol(X))]


#' A copy of \code{X} where each row is a unit vector.
#'
#' @param X a matrix
#' @return a matrix where each row has been scaled to unit length
#' @export
unitLengthRows <- function(X)
  t(apply(X, 1, function(x) { x / sqrt(sum(x^2)) }))


#' A copy of \code{X} where each column is a unit vector.
#'
#' @param X a matrix
#' @return a matrix where each column has been scaled to unit length
#' @export
unitLengthColumns <- function(X)
  t(apply(X, 2, function(x) { x / sqrt(sum(x^2)) }))


#' A centered copy of \code{X}.
#'
#' @param X a matrix whose rows hold the samples and whose columns hold the
#'  variables.
#' @return a matrix with zero-mean samples (one sample per row) and attribute
#'  `mu`
#' @export
centeredRows <- function(X) {
  mu <- colMeans(X)
  structure(sweep(X, 2, mu, `-`), mu=mu)
}


#' A centered copy of \code{X}.
#'
#' @param X a matrix whose columns hold the samples and whose rows hold the
#'   variables
#' @return a matrix with zero-mean samples (one sample per column)
#' @export
centeredColumns <- function(X) {
  mu <- rowMeans(X)
  structure(sweep(X, 1, mu, `-`), mu=mu)
}
