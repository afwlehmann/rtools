#
# statistics.R
# copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
#


#' Kullback-Leibler divergence.
#'
#' The Kullback-Leibler divergence between two distributions given their
#' density functions \code{p} and \code{q} along with samples \code{X}.
#'
#' \deqn{
#'   KL(p || q) = \sum_{x \in \Omega} \log \frac{p(x)}{q(x)} p(x)
#' }
#'
#' @param p a density function of the form \code{p(X)} whose single argument is
#'  a vector or matrix
#' @param q a density function of the same form as \code{p}
#' @param X a vector or matrix representing the observations
#' @return the KL divergence between \code{p} and \code{q}
#' @export
klDiv <- function(p, q, X) {
  pX <- p(X)
  qX <- q(X)
  lpq <- log(pX) - log(qX)
  # is.finite deals both with +/- Inf as well as NaNs, the latter of which
  # come along with -Inf - (-Inf).
  sum(replace(lpq, !is.finite(lpq), 0) * pX)
}


#' Print the given confusion matrix.
#'
#' Print the given confusion matrix along with information on the overall
#' accuracy as well as precision and recall for each class.
#' @param x an instance of \code{confusionMatrix}
#' @param ... ignored
#' @method print confusionMatrix
#' @export print.confusionMatrix
print.confusionMatrix <- function(x, ...) {
  cat("Rows = actual, columns = predicted\n")
  numDigits <- as.integer(log(max(x), 10) + 1)
  formStr <- sprintf(" %%%ds", numDigits)
  formNum <- sprintf(" %%%dd", numDigits)
  D <- ncol(x)
  scores <- confusionMatrixScores(x)
  cat("    ", sprintf(formStr, paste0("C", seq(D))), "\n", sep="")
  for (idx in seq(D)) {
    cat(sprintf(" %3s", paste0("C", idx)), sprintf(formNum, x[idx,]), sep="")
    cat(sprintf("\tPrec: %6.2f%%, Rec: %6.2f%%\n",
                scores$precision[idx] * 100, scores$recall[idx] * 100))
  }
  cat(sprintf("Accuracy: %6.2f%%\n", scores$accuracy * 100))
}


#' Accuracy, precisions and recalls for a given covariance matrix.
#' @param X a NxN matrix where rows represent the actual classes and columns
#'  represent the predicted classes
#' @return a list consisting of
#'  \item{accuracy}{the overall classification accuracy}
#'  \item{precision}{a vector of the classification precision for each class}
#'  \item{recall}{a vector of the classification recall for each class}
#' @export
confusionMatrixScores <- function(X) {
  stopifnot(nrow(X) == ncol(X))
  
  aux <- function(x, y) { r <- x/y; replace(r, !is.finite(r), 0) }
  D <- ncol(X)
  accuracy  <- aux(sum(diag(X)), sum(X))
  precision <- sapply(1:D, function(i) aux(X[i,i], sum(X[,i])))
  recall    <- sapply(1:D, function(i) aux(X[i,i], sum(X[i,])))

  list(accuracy=accuracy, precision=precision, recall=recall)
}
