% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nearestclassdistances.R
\name{nearestClassDistance}
\alias{nearestClassDistance}
\title{Title Find distance to next neighbour of a specific class}
\usage{
nearestClassDistance(coord, img, class, voxelsize, step = 0)
}
\arguments{
\item{coord}{coordinate of relevant voxel}

\item{img}{image array of classes}

\item{class}{class to find}

\item{voxelsize}{vector of length three. size of voxel in X-/Y-/Z-direction}

\item{step}{size of window to start with}
}
\value{
distance to nearest voxel of class "class"
}
\description{
Title Find distance to next neighbour of a specific class
}
