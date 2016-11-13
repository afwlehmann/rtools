#!/usr/bin/env Rscript
#
# pca_demo.R
# copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
#

library(rtools)

# Generate multivariate samples.
X <- matrix(rnorm(5 * 250000), ncol=5)
X <- sweep(X %*% diag(abs(rnorm(5, sd=10))), 2, rnorm(5, sd=50))

# Perform PCA. Keep all information.
pcaResult <- pca.full(X)
cat("Center of the original dataset:", pcaResult$mean, "\n")
cat("Variances of the principal components:", pcaResult$sd^2, "\n")
cat("Covariance matrix of the decorrelated dataset:\n")
print(cov(pcaResult$X))

# Perform PCA. Keep only those components whose standard deviation
# is at least half of the maximum component's standard deviation.
pcaTolResult <- pca.full(X, tol=0.5)
cat("Variances of the principal components:", pcaTolResult$sd^2, "\n")
cat("Covariance matrix of the decorrelated, reduced dataset:\n")
print(cov(pcaTolResult$X))
if (require(scatterplot3d)) {
  lim <- range(pcaResult$X)
  scatterplot3d(pcaTolResult$X, highlight.3d=TRUE, col.axis="blue",
                xlim=lim, ylim=lim, zlim=lim)
}

# Perform PCA-based whitening.
whit <- pca.whiten(X, tol=0.5)
cat("Variances of components that were kept:", whit$sd^2, "\n")
cat("Covariance matrix of X:\n"); print(cov(X))
cat("Covariance matrix of whitened X:\n"); print(cov(whit$X))

# `Restore` as much of the original data as possible.
Xhat <- whit$X %*% whit$invW
cat("Covariance matrix of Xhat:\n"); print(cov(Xhat))

# Performed `reduced` PCA on e.g. a dataset of 150 observations of 16384
# variables each.
Y <- matrix(rnorm(150 * 16384), ncol=16384) %*% diag(abs(rnorm(16384, sd=50)))
system.time(pcaRedResult <- pca.reduced(Y))
system.time(pcaCmpResult <- pca.full(Y))
