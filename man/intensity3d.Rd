\name{intensity3d}
\alias{intensity3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Intensity of a 3d Dataset or a Model
}
\description{
Generic function for computing the intensity of a spatial dataset or spatial point process model.}
\usage{
intensity3d(X,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A spatial dataset or a spatial point process model.}
  \item{...}{Further arguments depending on the class of \code{X.}}
}
\details{
This is a generic function for computing the intensity of a spatial dataset or spatial point process model. There are methods for point patterns (objects of class \code{"ppp"}) and fitted point process models (objects of class \code{"ppm"}).
The empirical intensity of a dataset is the average density (the average amount of ‘stuff’ per unit area or volume). The empirical intensity of a point pattern is computed by the method \code{intensity.ppp.}
The theoretical intensity of a stochastic model is the expected density (expected amount of ‘stuff’ per unit area or volume). The theoretical intensity of a fitted point process model is computed by the method \code{intensity.ppm.}}
\value{
Usually a numeric value or vector.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Adrian Baddeley \code{<Adrian.Baddeley@uwa.edu.au> http://www.maths.uwa.edu.au/~adrian/} and Rolf Turner \code{<r.turner@auckland.ac.nz>}}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{intensity.ppp, intensity.ppm.}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
