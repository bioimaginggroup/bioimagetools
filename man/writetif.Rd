\name{writeTIF}
\alias{writeTIF}
\title{Write TIFF stacks}
\description{
Function to write grey scale TIFF stacks
}
\usage{
writeTIF(img, file, bps=NULL) }
\arguments{
  \item{img}{Image as integer array.}
  \item{file}{A character vector of file names.}
  \item{bps}{Bits per sample. Default is either attr(img,"bits.per.sample") or 8l}
}
\value{
None}
\examples{
}
\author{Volker J. Schmid \email{volkerschmid@users.sourceforge.net}}
