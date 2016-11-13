#
# fun.R
# copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
#


#' A partially applied function object.
#'
#' @param FUN a function object
#' @param ... the parameters to be bound to the function object
#' @return a function object with a \code{...} parameter list where all of the
#'  already given parameters \code{...} have already been bound
#' @examples
#'  sumOfTwoAndWhatever <- partial(sum, 2)
#'  sumOfTwoAndWhatever(4) == 6
#'  sumOfTwoAndWhatever(4, 7) == 13
#' @export
partial <- function (FUN, ...) {
  # Taken from package roxygen (it's misnamed there as "curry")
  .orig = list(...)
  function(...) do.call(FUN, c(.orig, list(...)))
}


#' The composition of the given functions \code{...}.
#'
#' @param ... some function objects
#' @return a function object that represents the composition of the given
#'  functions, e.g. \code{f(g(...))} for given \code{f} and \code{g}
#' @examples
#'  g <- function(x) 2*x
#'  h <- function(y) y+3
#'  f <- compose(g, h)
#'  f(5) == 16
#' @export
compose <- function(...) {
  # Taken from package roxygen.
  fs <- rev(list(...))
  function(...)
    Reduce(function(x, f) f(x), fs, ...)
}


#' Tuples consisting of the first, second, third, and so forth elements of the
#' given arguments in \code{...}.
#'
#' @param zipper the function used for concatenation, e.g. \code{c} or \code{list}
#' @param ... the lists/vectors/etc. that hold the elements to be zipped
#' @return a list of tuples
zip <- function(zipper, ...) {
  # Taken from package roxygen.
  m <- mapply(zipper, ...)
  split(m, col(m))
}


#' Tuples zipped by using the \code{c()} function.
#'
#' @seealso \code{\link{zip}}.
#' @param ... the lists/vectors/etc. that hold the elements to be zipped
#' @return a list of tuples
#' @export
zip.c <- function(...)
  # Taken from roxygen package.
  zip(c, ...)


#' Tuples zipped by using the \code{list()} function.
#'
#' @seealso \code{\link{zip}}.
#' @param ... the lists/vectors/etc. that hold the elements to be zipped
#' @return a list of tuples
#' @export
zip.list <- function(...)
  # Taken from roxygen package.
  zip(list, ...)


#' The power set of the given \code{x}, minus the empty set.
#' 
#' @param x a set of values
#' @return the power set of the given \code{x}, minus the empty set.
#' @export
powerSet <- function(x) {
  if (length(x) == 1) x
  else {
    h <- head(x, 1)
    t <- tail(x, length(x)-1)
    pt <- powerSet(t)
    c(h, pt, lapply(powerSet(t), function(l) c(h, l)))
  }
}

