\name{readTIF}
\alias{readTIF}
\title{Read TIFF stacks}
\description{
Function to read TIFF stacks
}
\usage{
readTIF(file) }
\arguments{
  \item{file}{A character vector of file names.}
}
\value{
 Returns an array with TIF data as integer. Further informations are given as attributes. 
}
\examples{
#bi<-readTIF("ratbert.tif")
#image(bi[,,1])
}
\author{Volker J. Schmid \email{volkerschmid@users.sourceforge.net}}

