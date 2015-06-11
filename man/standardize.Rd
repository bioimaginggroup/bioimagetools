\name{standardize}
\title{Standardize images}
\description{
Standardizes images in order to compare different images. Mean of standardized image is 0.5, standard deviation is sd.}
\usage{
  standardize(img,mask=array(TRUE,dim(img)),N=32,sd=1/6) 
}
\arguments{
  \item{img}{is a 2d/3d arrary representing an image.}
  \item{mask}{a mask.}
  \item{N}{number of classes.}
  \item{sd}{standard deviation.}
}
\value{
  Multi-dimensional array of standardized image.
}
\author{Volker J. Schmid \email{volkerschmid@users.sourceforge.net}}
\examples{
#simuliere Daten zum Testen
test2<-runif(128*128,0,1)
test2<-sort(test2)
test2<-array(test2,c(128,128))
image(test2,col=grey(seq(0,1,by=1/1000)))

# Standardisiere test2 in 32 Klassen
std<-standardize(test2,N=32,sd=4)
}