\name{segment.outside}
\alias{segment.outside}
\title{Segmentation of the background of 3D images based on automatic threshold}
\description{
Segmentation of the background of 3D images. Starting from the borders of the image, the algorithm trys to find the edges of an object in the middle of the image. From this, a threshold for the edge is defined automatically. The function then return the a logical array representing voxel inside the object. 
}
\usage{
segment.outside(img,blobsize=1)
}
\arguments{
  \item{img}{is a 3d arrary representing an image.}
  \item{blobsize}{is an integer, representing the minimal diameter for bridges from the outside. E.g., a blobsize=3 allows for holes of size 2*(blobsize-1)=4 in the edge of the object.}
}
\value{
 A binary 3d array: 1 outside the object, 0 inside the object.  
}
\author{Volker J. Schmid \email{volkerschmid@users.sourceforge.net}}

