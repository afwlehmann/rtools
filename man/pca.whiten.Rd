% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca.R
\name{pca.whiten}
\alias{pca.whiten}
\title{Whitening of the given dataset \code{X}.}
\usage{
pca.whiten(X, center = TRUE, retX = TRUE, tol = 0)
}
\arguments{
\item{X}{a matrix whose rows contain the multivariate samples}

\item{center}{a boolean indicating whether the data should be centered}

\item{retX}{whether to compute and return a whitened copy of \code{X}}

\item{tol}{a value in [0,1] indicating which components to keep (those
components whose standard deviation is at least \code{tol} times as big as
that of the maximum component)}
}
\value{
a list consisting of
 \item{\code{X}}{if \code{retX} is \code{TRUE}, a whitened copy of the
   original dataset, else \code{NULL}}
 \item{\code{W}}{the transformation matrix}
 \item{\code{invW}}{the inverse of \code{W}. Note that \code{invW} can of
   course only invert so far as to the components that were kept after the
   analysis (see parameter \code{tol})}
  \item{\code{sd}}{the standard deviations of the principal components, i.e.
    the square roots of the eigenvalues of the covariance matrix}
 \item{\code{mean}}{the mean of the original dataset}
}
\description{
Performs whitening of the given dataset \code{X} by means of projection of
the data onto their principal components (thus decorrelating the variables)
and subsequent normalization to unit variance.
}
\details{
Depending on the given tolerance \code{tol}, either all principal components
are computed, i.e. the trace of the transformed covariance matrix is the same
as before the transformation (no information is lost), or only those
components whose standard deviation is at least \code{tol} times as big as
that of the maximum component.

The whitening transformation is of the form \eqn{W = UD}, where \eqn{U} is a
matrix whose columns contain the eigenvectors of the covariance matrix of
\eqn{X}, and \eqn{D} is a diagonal matrix whose elements are the reciprocals
of the standard deviations of the principal components, i.e. the square roots
of the corresponding eigenvalues.
}

