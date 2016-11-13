#' Entropy of a random variable.
#'
#' \deqn{H(X) = -\sum_x p(x) \cdot \log p(x)}
#'
#' For numeric vectors, a histogram of the values in \code{x} will be computed
#' after quantization of \code{x} according to the given \code{breaks}.
#'
#' For factors, a histogram will be computed according to the relative
#' frequencies of the factor levels.
#' @param x a vector or a factor
#' @param breaks see \code{\link{cut}}
#' @param base the base of the logarithm (2 = bits, exp(1) = nats, etc.)
#' @return the discrete entropy of X
#' @export
entropy <- function(x, breaks, base = exp(1)) {
  if (is.factor(x)) {
    x <- sapply(split(as.double(df$arity), df$arity), length)
  } else {
    x <- as.double(tabulate(cut(x, breaks=breaks)))
  }
  px <- x / sum(x)
  lpx <- log(px, base)
  -sum(px * replace(lpx, is.infinite(lpx), 0))
}


#' Conditional entropy of a random variable and corresponding class attribute.
#'
#' \deqn{H(X|Y) = \sum_y p(y) H(x|Y = y)}
#' @seealso \code{\link{entropy}}
#' @param x a vector or a factor
#' @param y a factor of the same length as x that encodes the class of each
#'   corresponding sample of x
#' @param breaks see \code{\link{cut}}
#' @param ... arguments passed on to \code{\link{entropy}}
#' @return the conditional entropy of X given Y
#' @export
entropyGivenClass <- function(x, y, breaks, ...) {
  stopifnot(is.factor(y))
  stopifnot(length(x) == length(y))
  py <- sapply(split(as.double(y), y), length) / length(y)
  Hxs <- sapply(split(x, y), entropy, breaks = breaks, ...)
  sum(py * Hxs)
}


#' Mutual information of a random variable and corresponding class attribute.
#'
#' \deqn{I(X;Y) = H(X) - H(X|Y)}
#' @seealso \code{\link{entropy}} and \code{\link{entropyGivenClass}}
#' @param x a vector or a factor
#' @param y a factor of the same length as x that encodes the class of each
#'   corresponding sample of x
#' @param breaks see \code{\link{cut}}
#' @param ... arguments passed on to \code{\link{entropy}}
#' @return the mutual information of X and Y
#' @export
mutualInformation <- function(x, y, breaks, ...) {
  stopifnot(is.factor(y))
  HX <- entropy(x, breaks = breaks, ...)
  HXgivenY <- entropyGivenClass(x, y, breaks, ...)
  HX - HXgivenY
}


#' Uncertainty coefficient of a random variable and corresponding class attribute.
#'
#' \deqn{U(X|Y) = \frac{I(X;Y)}{H(X)}}
#' @seealso \code{\link{mutualInformation}} and \code{\link{entropy}}
#' @param x a vector or a factor
#' @param y a factor of the same length as x that encodes the class of each
#'   corresponding sample of x
#' @param breaks see \code{\link{cut}}
#' @param base the base of the logarithm (2 = bits, exp(1) = nats, etc.)
#' @param ... arguments passed on to \code{\link{entropy}}
#' @return the mutual information of X and Y
#' @export
uncertaintyCoefficient <- function(x, y, breaks, base = exp(1), ...) {
  stopifnot(is.factor(y))
  HY <- local({
    py <- sapply(split(as.double(y), y), length) / length(y)
    lpy <- log(py, base)
    -sum(py * replace(lpy, is.infinite(lpy), 0))
  })
  HX <- entropy(x, breaks = breaks, ...)
  HXgivenY <- entropyGivenClass(x, y, breaks, ...)
  (HX - HXgivenY) / HY
}
