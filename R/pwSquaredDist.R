#
# pwSquaredDist.R
# copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
#

#' Squared distance of each pair of row vectors of the matrix \code{X}.
#'
#' @examples
#' Y <- matrix(c(1, -2, 3, 55, -8, 4, 3, 7, 11), ncol=3)
#' distances <- pwSquaredDist(Y)        # Result as vector
#'
#' D <- pwSquaredDist(Y, asMatrix=TRUE) # Result as lower triangular matrix
#' @param X a matrix
#' @param asMatrix a boolean indicating whether the result should be a
#'   vector or a symmetric matrix, defaults to \code{FALSE}
#' @return a vector of length N*(N-1)/2 if \code{asMatrix=T}, else a NxN
#'   symmetric matrix
#' @export
pwSquaredDist <- function(X, asMatrix=FALSE) {
  N <- nrow(X)
  M <- ncol(X)
  d <- .C("pwSquaredDist",
          as.double(X), N, M,
          d = double(N * (N-1) / 2),
          PACKAGE="rtools")$d
  if (asMatrix) {
    mat <- matrix(0, N, N)
    mat[lower.tri(mat)] <- d
    mat + t(mat)
  } else
    d
}
