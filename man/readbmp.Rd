\name{readBMP}
\alias{readBMP}
\title{Read Bitmap files}
\description{
Function to read 2D grey-value BMP files
}
\usage{
readBMP(file) }
\arguments{
  \item{file}{A character vector of file names or URLs.}
}
\value{
 Returns a matrix with BMP data as integer.
}
\examples{
bi<-readBMP("http://www.statistik.lmu.de/institut/ag/bioimg/bit/ratbert.bmp")
image(bi,col=grey(seq(1,0,length=100)))
}
\author{Volker J. Schmid \email{volkerschmid@users.sourceforge.net}}


