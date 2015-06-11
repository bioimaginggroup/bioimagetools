\name{L3cross3d}
\alias{L3cross3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multitype L-function (cross-type)}
\description{Calculates an estimate of the cross-type L-function for a multitype point pattern.}
\usage{L3cross3d(X, i, j, ...)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{The observed point pattern, from which an estimate of the cross-type $L$
function $L_{ij}(r)$ will be computed. It must be a multitype point pattern (a marked point pattern whose marks are a factor). See under Details.}
  \item{i}{The type (mark value) of the points in \code{X} from which distances are measured. A character string (or something that will be converted to a character string). Defaults to the first level of \code{marks(X)}.}
  \item{j}{The type (mark value) of the points in \code{X} to which distances are measured. A character string (or something that will be converted to a character string). Defaults to the second level of \code{marks(X)}.}
  \item{...}{Arguments passed to \code{Kcross}.}
}
\details{
The cross-type L-function is a transformation of the cross-type K-function,
$L_{ij}(r) = \\sqrt{\\frac{K_{ij}(r)}{\\pi}}$
where K_{ij}(r) is the cross-type K-function from type \code{i} to type \code{j}. See
\code{Kcross} for information about the cross-type K-function.

The command \code{Lcross} first calls \code{Kcross} to compute the estimate of the cross-type K-function, and then applies the square root transformation.

For a marked point pattern in which the points of type
\code{i} are independent of the points of type \code{j}, the theoretical value of the L-function is $L_{ij}(r) = r$. The square root also has the effect of stabilising
the variance of the estimator, so that $L_{ij}$ is more appropriate for use in simulation envelopes and hypothesis tests.
}
\value{
An object of class
\code{"fv"}, see
\code{fv.object}, which can be plotted directly using \code{plot.fv}.
Essentially a data frame containing columns

  \item{r}{the vector of values of the argument $r$ at which the function $L_{ij}$
has been estimated}
  \item{theo}{he theoretical value $L_{ij}(r) = r$ for a stationary Poisson process}
  
together with columns named
\code{"border", "bord.modif", "iso"} and/or \code{"trans"}, according to the
selected edge corrections. These columns contain estimates of the function
$L_{ij}$ obtained by the edge corrections named.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Adrian Baddeley
\code{<Adrian.Baddeley@uwa.edu.au> http://www.maths.uwa.edu.au/~adrian/} and Rolf Turner \code{<r.turner@auckland.ac.nz>}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{Kcross, Ldot, Lest}
}
\examples{
data(amacrine)
L <- Lcross(amacrine, "off", "on")
plot(L)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
