#
# partition.R
# copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
#


#' S3 method for partitioning \code{X} into \code{numPartitions} partitions.
#' 
#' @param X an instance of a supported class
#' @param numPartitions the number of partitions
#' @param ... passed on to S3 method delegates
#' @return a list containing the partitions of \code{X}
#' @export
partition <- function(X, numPartitions, ...) {
  UseMethod("partition")
}


#' Partition the given vector \code{X} into \code{numPartitions} partitions.
#'
#' @param X an instance of a supported class
#' @param numPartitions the number of partitions
#' @param ... reserved
#' @return a list containing the partitions of \code{X}
#' @method partition numeric
#' @export partition.numeric
partition.numeric <- function(X, numPartitions, ...) {
  N <- length(X)
  partSize <- N %/% numPartitions
  if (partSize < 1)
    stop("Too many partitions!")
  spl <- rep(1:numPartitions, each=partSize)
  if (N %% numPartitions != 0) {
    remainder <- N - partSize * numPartitions
    spl <- c(spl, rep(numPartitions, remainder))
  }
  lapply(1:numPartitions, function(i) X[spl == i])
}


#' Partition the given matrix \code{X} into \code{numPartitions} partitions.
#'
#' @param X an instance of a supported class
#' @param numPartitions the number of partitions
#' @param axis the axis along which to partition
#' @param ... reserved
#' @return a list containing the partitions of \code{X}
#' @method partition matrix
#' @export partition.matrix
partition.matrix <- function(X, numPartitions, axis=1, ...) {
  stopifnot(axis == 1 || axis == 2)
  spl <- NULL
  if (axis == 1) {
    N <- nrow(X)
    fun <- function(i) X[spl == i, ]
  } else {
    N <- ncol(X)
    fun <- function(i) X[, spl == i]
  }
  partSize <- N %/% numPartitions
  if (partSize < 1)
    stop("Too many partitions!")
  spl <- rep(1:numPartitions, each=partSize)
  if (N %% numPartitions != 0) {
    remainder <- N - partSize * numPartitions
    spl <- c(spl, rep(numPartitions, remainder))
  }
  lapply(1:numPartitions, fun)
}


#' Partition the given array \code{X} into \code{numPartitions} partitions.
#'
#' @param X an instance of a supported class
#' @param numPartitions the number of partitions
#' @param axis the axis along which to partition
#' @param ... reserved
#' @return a list containing the partitions of \code{X}
#' @method partition array
#' @export partition.array
partition.array <- function(X, numPartitions, axis=1, ...) {
  N <- dim(X)[axis]
  partSize <- N %/% numPartitions
  if (partSize < 1) 
    stop("Too many partitions!")
  spl <- rep(1:numPartitions, each=partSize)
  if (N %% numPartitions != 0) {
    remainder <- N - partSize * numPartitions
    spl <- c(spl, rep(numPartitions, remainder))
  }
  before <- axis - 1
  after <- length(dim(X)) - before - 1
  fun <- function(i)
    c(list(X), rep(T, before), list(spl==i), rep(T, after), drop=F)
  lapply(1:numPartitions,
         function(i) do.call(`[`, c(list(X),
                                    rep(T, before),
                                    list(spl==i),
                                    rep(T, after),
                                    drop=F)))
}
