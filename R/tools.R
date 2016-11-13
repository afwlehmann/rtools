#
# tools.R
# copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
#


#' Clamp the values of \code{x} to a given range.
#' @param x input vector
#' @param lower the lower bound
#' @param upper the upper bound
#' @return a vector of the values of \code{x} clamped to [lower,upper]
#' @export
clamp <- function(x, lower=-1, upper=+1)
    pmax(lower, pmin(upper, x))
