% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segmentoutside.R
\name{segment.outside}
\alias{segment.outside}
\title{Segmentation of the background of 3D images based on automatic threshold}
\usage{
segment.outside(img, blobsize = 1)
}
\arguments{
\item{img}{is a 3d array representing an image.}

\item{blobsize}{is an integer, representing the minimal diameter for bridges from the outside. E.g., a blobsize=3 allows for holes of size 2*(blobsize-1)=4 in the edge of the object.}
}
\value{
A binary 3D array: 1 outside the object, 0 inside the object.
}
\description{
Segmentation of the background of 3D images. Starting from the borders of the image, the algorithm tries to find the edges of an object in the middle of the image. From this, a threshold for the edge is defined automatically. The function then return the a logical array representing voxel inside the object.
}
\examples{
\donttest{
kringel <- readTIF(system.file("extdata","kringel.tif",package="bioimagetools"))
out <- segment.outside(kringel)
img(out, z=1)
}
}
