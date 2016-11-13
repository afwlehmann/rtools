#!/usr/bin/env Rscript
#
# nca_demo.R
# copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
#

suppressMessages({
  library(rtools)
})

# Example data.
X <- local({ data(wine); wine })
klass <- as.factor(X[,1])
Xprime <- as.matrix(subset(X, select=-Class))

# Whitening.
Xprime <- pca.whiten(Xprime)$X

# Auxiliary function for plotting.
plotStuff <- function(Y, xlim = range(Y[,1]), ylim = range(Y[,2]), title) {
  dev.new()
  plot(Y[klass==1,], col='red', xlim=xlim, ylim=ylim, xlab=NA, ylab=NA)
  par(new=T)
  plot(Y[klass==2,], col='green', xlim=xlim, ylim=ylim, xlab=NA, ylab=NA)
  par(new=T)
  plot(Y[klass==3,], col='blue', xlim=xlim, ylim=ylim, xlab=NA, ylab=NA)
  title(title)
}

# Perform NCA with the "points" algorithm.
A <- nca(Xprime, klass, d=2, lambda=1e-2, maxIter=25, algorithm="points")
Y <- Xprime %*% A
plotStuff(Y, title="points")

# Perform NCA with the "kl" algorithm.
A <- nca(Xprime, klass, d=2, lambda=1e-2, maxIter=25, algorithm="kl")
Y <- Xprime %*% A
plotStuff(Y, title="kl")
