\name{outside}
\alias{outside}
\title{Segmentation of the background of 3D images based on classes}
\description{
Segmentation of the background of 3D images based on classes. 
}
\usage{
outside(img,what,blobsize=1)
}
\arguments{
  \item{img}{is a 3d arrary representing an image.}
  \item{what}{is an integer of the class of the background.}
  \item{blobsize}{is an integer, representing the minimal diameter for bridges from the outside. E.g., a blobsize=3 allows for holes of size 2*(blobsize-1)=4 in the edge of the object.}
}
\value{
 A binary 3d array: 1 outside the object, 0 inside the object.  
}
\author{Volker J. Schmid \email{volkerschmid@users.sourceforge.net}}

