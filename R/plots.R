#' 2d histogram plots of all variables vs. each other.
#'
#' @param dataset a dataframe or matrix whose rows contain the samples and
#'                whose columns denote the variables
#' @param nbins an integer or a two-vector describing the number of bins per axis,
#'              passed on to \code{hist2d}
#' @param oma passed on to \code{par}
#' @param col colors of the various levels, passed on to \code{hist2d}
#' @importFrom gplots hist2d
#' @export
plotAllVsAll <- function(dataset, nbins=200, oma=c(2,1,3.5,1),
                         col=c("white", heat.colors(16)))
{
    stopifnot(is.matrix(dataset) || is.data.frame(dataset))

    parBackup <- par(no.readonly=TRUE)
    par(oma=oma, mar=rep(0.1, 4), mgp=c(3,0.4,0), las=1, cex.axis=0.8, tck=-0.05)

    # 3x3 example of the layout of all plots:
    #
    # 1 4 7
    # 2 5 8
    # 3 6 9
    nc <- ncol(dataset)
    layout(matrix(1:(nc^2), ncol=nc), respect=TRUE)
    
    for (i in 1:nc) {
      for (j in 1:nc) {
        plot.new()
        box()

        if (i == j) {
          xmm <- range(dataset[,i])
          ymm <- range(dataset[,j])
          par(usr=c(xmm[1], xmm[2], ymm[1], ymm[2]))
          text(x=sum(xmm)/2, y=sum(ymm)/2, labels=colnames(dataset)[i])
        } else {
          par(new=TRUE)
          hist2d(dataset[,c(i,j)], axes=FALSE, xlab="", ylab="",
                 nbins=nbins, col=col)
        }

        # Top/bottom axis labels
        if (i%%2 == 1 && j == nc)
          axis(1, at=pretty(dataset[,i], n=5))
        else if (i%%2 == 0 && j == 1)
          axis(3, at=pretty(dataset[,i], n=5))

        # Left/right axis labels
        if (j%%2 == 1 && i == nc)
          axis(4, at=pretty(dataset[,j], n=5))
        else if (j%%2 == 0 && i == 1)
          axis(2, at=pretty(dataset[,j], n=5))
      }
    }
    par(parBackup)
}


#' Plot standard deviation markers.
#'
#' @param x x-coordinates of points
#' @param y y-coordinates of points
#' @param stddev standard deviations corresponding to \code{x} and \code{y}
#' @param length the length of the perpendicular bar, i.e. top and bottom of thex
#' marker
#' @param ... passed on to \code{arrows}
#' @export
plotStdDev <- function(x, y, stddev, length=0.1, ...) {
  arrows(x, y+stddev, x, y-stddev, angle=90, code=3, length=length, ...)
}


#' Legend for color gradient coded plots.
#' @param tlx x-coordinate of the top-left corner
#' @param tly y-coordinate of the top-left corner
#' @param brx x-coordinate of the bottom-right corner
#' @param bry y-coordinate of the bottom-right corner
#' @param cols a vector or list of colors
#' @param labels a vector or list of labels
#' @export
legend.gradient <- function(tlx, tly, brx, bry,
                            cols=heat.colors(5), labels=c(0, 1))
{
  stopifnot(length(cols) > 1 && length(labels) <= length(cols))

  y <- seq(bry, tly, length.out=length(cols)+1)
  mapply(function(y1, y2, c)
           polygon(x=c(tlx, tlx, brx, brx), y=c(y1, y2, y2, y1), col=c, border=F),
         head(y, length(y)-1),
         tail(y, length(y)-1),
         cols)
  rect(tlx, tly, brx, bry, col='darkgray', density=0)

  cw <- par('cxy')[1]
  ch <- par('cxy')[2]
  mapply(function(y, l)
           text(brx + cw, y, labels=l, adj=c(0, 0.5)),
         seq(bry + ch/2, tly - ch/2, length.out=length(labels)),
         labels)
}


