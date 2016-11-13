#
# pca.R
# copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
#


#' Principal Component Analysis of the given dataset \code{X}.
#'
#' PCA is performed through Singular Value Decomposition as that is numerically
#' more stable than doing it by means of eigenvalue decomposition of the
#' covariance matrix.
#'
#' Depending on the given tolerance \code{tol}, either all principal components
#' are computed, i.e. the trace of the transformed covariance matrix is the same
#' as before the transformation (no information is lost), or only those
#' components whose variances sum up to at least a fraction of \code{tol}
#' of the sum of the total variance.
#' @param X a matrix where each row denotes one multivariate sample
#' @param center a boolean indicating whether the data should be centered
#' @param retX whether to compute and return a projection of the (possibly
#'   centered) original dataset onto its principal components
#' @param tol a value in [0,1] indicating which components to keep
#' @return a list consisting of
#'   \item{\code{X}}{if \code{retX} is \code{TRUE}, a projection of the (possibly
#'     centered original dataset onto its principal components, else \code{NULL}}
#'   \item{\code{V}}{an orthonormal matrix whose columns resemble the
#'     principal components, i.e. the eigenvectors of the covariance matrix}
#'   \item{\code{sd}}{the standard deviations of the principal components, i.e.
#'     the square roots of the eigenvalues of the covariance matrix}
#'   \item{\code{mean}}{if \code{center} is \code{TRUE}, the mean of the original
#'     dataset, else \code{NULL}}
#' @export
pca.full <- function(X, center=TRUE, retX=TRUE, tol=1) {
  stopifnot(tol >= 0, tol <= 1)
  # Determine the mean.
  if (center) {
    mu <- colMeans(X)
    X <- sweep(X, 2, mu, `-`)
  }
  # Perform SVD on the data instead of decomposing the covariance matrix.
  aux <- svd(X)
  # Determine the indices of those components whose standard deviations
  # sum up to at least a fraction of `tol` of the total sum.
  # Note that aux$d contains the square roots of the eigenvalues of X^T X, hence:
  #lambda <- aux$d^2 / (nrow(X)-1)
  #sigma <- sqrt(lambda)
  sigma <- aux$d / sqrt(nrow(X)-1)
  sigmaSq <- sigma^2
  sigmaAccRatio <- Reduce(`+`, sigmaSq, accumulate = TRUE) / sum(sigmaSq)
  indices <- seq( min(which(sigmaAccRatio >= tol)) )
  sigma <- sigma[indices]
  V <- aux$v[,indices]
  # Return the result.
  list(X = if (retX) X %*% V else NULL,
       V = V,
       sd = sigma,
       mean = if (center) mu else NULL)
}


#' Principal Component Analysis of the given high-dimensional dataset X.
#'
#' This function is intended for datasets whose dimensionality \eqn{M} exceeds
#' the number of samples \eqn{N} by several magnitudes, i.e. \eqn{M \gg N}{M >> N}.
#' 
#' The basic idea is to compute the eigenvectors of the covariance matrix of
#' the \emph{transposed} dataset \eqn{X^T}. Note that in this scenario, where
#' \eqn{M \gg N}{M >> N}, there are at most \eqn{N} linearly independent
#' eigenvectors.
#'
#' By working on the transposed dataset, the size of the corresponding
#' covariance matrix is reduced from \eqn{M \times M}{M x M} to \eqn{N \times
#' N}{N x N}, which, under the given circumstances, is a huge computational
#' advantage.
#'
#' Let \eqn{\Sigma := X^T X}{Sigma := X^T X} be the covariance matrix of \eqn{X}
#' (note that both the mean \eqn{\mu}{mu} as well as the factor
#' \eqn{\frac{1}{N-1}}{1/(N-1)} have been dropped for increased readability),
#' then
#' \deqn{
#'   \begin{array}{lrcl}
#'                     &    X^T X x & = & \lambda x   \\
#'     \Leftrightarrow &  X X^T X x & = & \lambda X x
#'   \end{array}
#' }{
#'       X^T X x = lambda x
#' <=> X X^T X x = lambda X x
#' }
#' 
#' Substitution of \eqn{y} for \eqn{X x} yields
#' \deqn{X X^T y = \lambda y}{X X^T y = lambda y}
#'
#' Then
#' \deqn{
#'   \begin{array}{lrcl}
#'                     &   X^T X X^T y & = & \lambda X^T y \\
#'     \Leftrightarrow & (X^T X) X^T y & = & \lambda X^T y \\
#'     \Leftrightarrow &  \Sigma X^T y & = & \lambda X^T y
#'   \end{array}
#' }{
#'       X^T X X^T y = lambda X^T y
#' <=> (X^T X) X^T y = lambda X^T y
#' <=>   Sigma X^T y = lambda X^T y
#' }
#'
#' from which it follows that \eqn{x = X^T y} is an eigenvector of the original
#' covariance matrix \eqn{\Sigma}{Sigma}.
#' 
#' Hence, first the eigenvectors \eqn{y} of \eqn{X X^T} are calculated and then
#' transformed by \eqn{X^T} in order to compute the eigenvectors \eqn{x} of
#' \eqn{X^T X = \Sigma}{X^T X = Sigma}.
#'
#' @param X a matrix whose rows contain the multivariate samples
#' @param center a boolean indicating whether the data should be centered
#' @param retX whether to compute and return a projection of the (possibly
#'   centered) original dataset onto its principal components
#' @return a list consisting of
#'   \item{\code{X}}{if \code{retX} is \code{TRUE}, a projection of the (possibly
#'     centered original dataset onto its principal components, else \code{NULL}}
#'   \item{\code{V}}{an orthonormal matrix whose columns resemble the
#'     principal components, i.e. the eigenvectors of the covariance matrix}
#'   \item{\code{sd}}{the standard deviations of the principal components, i.e.
#'     the square roots of the eigenvalues of the covariance matrix}
#'   \item{\code{mean}}{mean of the original dataset}
#' @export
pca.reduced <- function(X, center=TRUE, retX=TRUE) {
  # Note that the data must first be centered on the *original* dimensions
  # because the centering of the 'transposed covariance' is meaningless for
  # the dataset. This is also why Sigma must be computed dependent on N
  # instead of simply using cov().
  if (center) {
    mu <- colMeans(X)
    X <- sweep(X, 2, mu, `-`)
  }
  # From now on we're looking at the transpose of X:
  Xt <- t(X)
  aux <- svd(Xt)
  V <- Xt %*% aux$v
  # Normalize the columns of V.
  V <- apply(V, 2, function(x) x / sqrt(sum(x^2)))
  # Done.
  list(X = if (retX) X %*% V else NULL,
       V = V,
       sd = aux$d / sqrt(nrow(X)-1),
       mean = if (center) mu else NULL)
}


#' Whitening of the given dataset \code{X}.
#'
#' Performs whitening of the given dataset \code{X} by means of projection of
#' the data onto their principal components (thus decorrelating the variables)
#' and subsequent normalization to unit variance.
#'
#' Depending on the given tolerance \code{tol}, either all principal components
#' are computed, i.e. the trace of the transformed covariance matrix is the same
#' as before the transformation (no information is lost), or only those
#' components whose standard deviation is at least \code{tol} times as big as
#' that of the maximum component.
#'
#' The whitening transformation is of the form \eqn{W = UD}, where \eqn{U} is a
#' matrix whose columns contain the eigenvectors of the covariance matrix of
#' \eqn{X}, and \eqn{D} is a diagonal matrix whose elements are the reciprocals
#' of the standard deviations of the principal components, i.e. the square roots
#' of the corresponding eigenvalues.
#' @param X a matrix whose rows contain the multivariate samples
#' @param center a boolean indicating whether the data should be centered
#' @param retX whether to compute and return a whitened copy of \code{X}
#' @param tol a value in [0,1] indicating which components to keep (those
#'  components whose standard deviation is at least \code{tol} times as big as
#'  that of the maximum component)
#' @return a list consisting of
#'  \item{\code{X}}{if \code{retX} is \code{TRUE}, a whitened copy of the
#'    original dataset, else \code{NULL}}
#'  \item{\code{W}}{the transformation matrix}
#'  \item{\code{invW}}{the inverse of \code{W}. Note that \code{invW} can of
#'    course only invert so far as to the components that were kept after the
#'    analysis (see parameter \code{tol})}
#'   \item{\code{sd}}{the standard deviations of the principal components, i.e.
#'     the square roots of the eigenvalues of the covariance matrix}
#'  \item{\code{mean}}{the mean of the original dataset}
#' @export
pca.whiten <- function(X, center=TRUE, retX=TRUE, tol=0) {
  stopifnot(0 <= tol && tol <= 1)
  # Determine the samples' mean.
  if (center) {
    mu <- colMeans(X)
    X <- sweep(X, 2, mu, `-`)
  }
  # Perform SVD on the data instead of decomposing the covariance matrix.
  aux <- svd(X)
  # Determine the indices of those components whose standard deviations are at
  # least `tol` times as big as the maximum.
  # Note that aux$d contains the square roots of the eigenvalues of X^T X, hence:
  #lambda <- aux$d^2 / (nrow(X)-1)
  #sd <- sqrt(lambda)
  sd <- aux$d / sqrt(nrow(X)-1)
  indices <- which( (sd / sd[1]) >= tol ) # sd[1] == max(sd)
  # Compute the whitening transformation `W` (and its inverse `inv`) that
  # projects onto the principal components of X and scales to unit variance.
  vi   <- aux$v[,indices,drop=F]
  sdi  <- sd[indices]
  W    <- vi %*% diag(1 / sdi, nrow=length(sdi))
  invW <- diag(sdi, nrow=length(sdi)) %*% t(vi)
  # Done.
  list(X = if (retX) X %*% W else NA,
       W = W,
       invW = invW,
       sd = sd[indices],
       mean = if (center) mu else NULL)
}
