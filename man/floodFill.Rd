\name{floodFill}
\alias{floodFill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Region filling
}
\description{
Fill regions in images.
}
\usage{
floodFill(x, pt, col, tolerance=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An \code{image} object or an array.}
  \item{pt}{Coordinates of the start filling point.}
  \item{col}{Fill color. This argument should be a numeric for Grayscale images and an R
color for Color images.}
  \item{tolerance}{Color tolerance used during the fill.}
}
\details{
Flood fill is performed using the fast scan line algorithm. Filling starts at
pt
and grows in connected
areas where the absolute difference of the pixels intensities (or colors) remains below
\code{tolerance.}}
\value{
An \code{Image} object or an array, containing the transformed version of \code{x.}
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Gregoire Pau, Oleg Sklyar; 2007}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x = readImage(system.file("images", "shapes.png", package="EBImage"))
y = floodFill(x, c(67, 146), 0.5)
if (interactive()) display(y)
y = channel(y, rgb)
y = floodFill(y, c(48, 78), red)
y = floodFill(y, c(156, 52), orange)
if (interactive()) display(y)
x = readImage(system.file("images", "lena.png", package="EBImage"))
y = floodFill(x, c(226, 121), 1, tolerance=0.1)
if (interactive()) display(y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
