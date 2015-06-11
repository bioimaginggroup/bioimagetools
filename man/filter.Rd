\name{filter}
\alias{filter}
\title{Apply filter to 3D images}
\description{
A filter is applied to a 3D array representing an image. So far only variance filters are supported.
}
\usage{
filter(img, filter="var", window, z.scale=1) 
}
\arguments{
  \item{img}{is a 3d arrary representing an image.}
  \item{filter}{is the filter to be applied. Options: var: Variance filter.}
  \item{window}{half size of window; i.e. window=1 uses a window of 3 voxels in each direction.}
  \item{z.scale}{ratio of voxel dimension in x/y direction and z direction.}
}
\value{
  Multi-dimensional array of filtered image data.
}
\author{Volker J. Schmid \email{volkerschmid@users.sourceforge.net}}

