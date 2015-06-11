\name{nextneighbourdistribution}
\alias{nndist}
\title{Ploting next neighbour distribution of 3D point patterns}
\usage{
nndist(X,Y,Z,X2=X,Y2=Y,Z2=Z,same=TRUE,psz=25,main="Next neighbour distribution",file=NULL, return=FALSE)
}
\arguments{
  \item{X}{X coordinate of (first) point pattern in microns.}
  \item{Y}{Y coordinate of (first) point pattern in microns.}
  \item{Z}{Z coordinate of (first) point pattern in microns.}
  \item{X2}{X2 coordinate of second point pattern in microns.}
  \item{Y2}{Y2 coordinate of second point pattern in microns.}
  \item{Z2}{Z2 coordinate of second point pattern in microns.}
  \item{same}{is a boolean parameter; TRUE if all points are of same color.}
  \item{psz}{is the approximate number of pixels per micron (for internal purposes only).}
  \item{main}{is the title of the plot.}
  \item{file}{is the file name of the png file. Default is NULL, which plots to the Null Device.}
  \item{return}{if TRUE, return next neighbour histogram (default: FALSE).}
}
\description{
Computes the minimal distances between next neighbours and plots their distribution. For marked points (e.g., red and green), same=FALSE computes the minimal distances to the next neighbour of the other mark (color).
X, Y and Z coordinates of the point pattern have to be given in microns.
}
\value{
Histogram of Next Neighbour Distribution (if return=TRUE).
}
\author{Volker J. Schmid \email{volkerschmid@users.sourceforge.net}}

