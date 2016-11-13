#
# transformations.R
# copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
#


#' Homogeneous rotation matrix (x-axis).
#'
#' A homogeneous 4x4 matrix for rotation around the x-axis.
#' @param alpha the rotation angle in radians
#' @return a 4x4 homogeneous matrix
#' @export
Rx <- function(alpha) {
  matrix(c(1, 0, 0, 0,
           0, cos(alpha), sin(alpha), 0,
           0, -sin(alpha), cos(alpha), 0,
           0, 0, 0, 1),
         nrow=4, byrow=TRUE)
}


#' Homogeneous rotation matrix (y-axis).
#'
#' A homogeneous 4x4 matrix for rotation around the y-axis.
#' @param alpha the rotation angle in radians
#' @return a 4x4 homogeneous matrix
#' @export
Ry <- function(alpha) {
  matrix(c(cos(alpha), 0, sin(alpha), 0,
           0, 1, 0, 0,
           -sin(alpha), 0, cos(alpha), 0,
           0, 0, 0, 1),
         nrow=4, byrow=TRUE)
}
         

#' Homogeneous rotation matrix (z-axis).
#'
#' A homogeneous 4x4 matrix for rotation around the z-axis.
#' @param alpha the rotation angle in radians
#' @return a 4x4 homogeneous matrix
#' @export
Rz <- function(alpha) {
  matrix(c(cos(alpha), sin(alpha), 0, 0,
           -sin(alpha), cos(alpha), 0, 0,
           0, 0, 1, 0,
           0, 0, 0, 1),
         nrow=4, byrow=TRUE)
}
