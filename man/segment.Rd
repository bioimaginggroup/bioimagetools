% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segment.R
\name{segment}
\alias{segment}
\title{Segmentation of 3D images using EM algorithms}
\usage{
segment(
  img,
  nclust,
  beta,
  z.scale = 0,
  method = "cem",
  varfixed = TRUE,
  maxit = 30,
  mask = array(TRUE, dim(img)),
  priormu = rep(NA, nclust),
  priormusd = rep(NULL, nclust),
  min.eps = 10^{
     -7
 },
  inforce.nclust = FALSE,
  start = NULL,
  silent = FALSE
)
}
\arguments{
\item{img}{is a 3d array representing an image.}

\item{nclust}{is the number of clusters/classes to be segmented.}

\item{beta}{is a matrix of size nclust x nclust, representing the prior weight of classes neighboring each other.}

\item{z.scale}{ratio of voxel dimension in x/y direction and z direction. Will be multiplied on beta for neighboring voxel in z direction.}

\item{method}{only "cem" classification EM algorithm implemented.}

\item{varfixed}{is a logical variable. If TRUE, the variance is equal in each class.}

\item{maxit}{is the maximum number of iterations.}

\item{mask}{is a logical array, representing the voxels to be used in the segmentation.}

\item{priormu}{is a vector with mean of the normal prior of the expected values of all classes. Default is NA, which represents no prior assumption.}

\item{priormusd}{is a vector with standard deviations of the normal prior of the expected values of all classes.}

\item{min.eps}{stop criterion. Minimal change in sum of squared estimate of mean in order to stop.}

\item{inforce.nclust}{if TRUE enforces number of clusters to be nclust. Otherwise classes might be removed during algorithm.}

\item{start}{not used}

\item{silent}{if TRUE, function remains silent during running time}
}
\value{
A list with "class": 3d array of class per voxel; "mu" estimated means; "sigma": estimated standard deviations.
}
\description{
Segmentation of 3D images using EM algorithms
}
\examples{
\dontrun{
original<-array(1,c(300,300,50))
for (i in 1:5)original[(i*60)-(0:20),,]<-original[(i*60)-(0:20),,]+1
for (i in 1:10)original[,(i*30)-(0:15),]<-original[,(i*30)-(0:15),]+1
original[,,26:50]<-4-aperm(original[,,26:50],c(2,1,3))

img<-array(rnorm(300*300*50,original,.2),c(300,300,50))
img<-img-min(img)
img<-img/max(img)

try1<-segment(img,3,beta=0.5,z.scale=.3)
print(sum(try1$class!=original)/prod(dim(original)))

beta<-matrix(rep(-.5,9),nrow=3)
beta<-beta+1.5*diag(3)
try2<-segment(img,3,beta,z.scale=.3)
print(sum(try2$class!=original)/prod(dim(original)))

par(mfrow=c(2,2))
img(original)
img(img)
img(try1$class)
img(try2$class)
}
}
