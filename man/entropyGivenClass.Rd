% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/information.R
\name{entropyGivenClass}
\alias{entropyGivenClass}
\title{Conditional entropy of a random variable and corresponding class attribute.}
\usage{
entropyGivenClass(x, y, breaks, ...)
}
\arguments{
\item{x}{a vector or a factor}

\item{y}{a factor of the same length as x that encodes the class of each
corresponding sample of x}

\item{breaks}{see \code{\link{cut}}}

\item{...}{arguments passed on to \code{\link{entropy}}}
}
\value{
the conditional entropy of X given Y
}
\description{
\deqn{H(X|Y) = \sum_y p(y) H(x|Y = y)}
}
\seealso{
\code{\link{entropy}}
}

