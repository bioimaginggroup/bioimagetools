\name{Kcross3D}
\alias{Kcross3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multitype K Function (Cross-type)}
\description{
For a multitype point pattern, estimate the multitype
$K$ function which counts the expected number of points of type $j$ within a given distance of a point of type $i$.}
\usage{
Kcross3D(X, i, j, r=NULL, breaks=NULL, correction, ..., ratio=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{The observed point pattern, from which an estimate of the cross type $K$ function $K_{ij}(r)$ will be computed. It must be a multitype point pattern (a marked point pattern whose marks are a factor). See under Details.}
  \item{i}{The type (mark value) of the points in \code{X} from which distances are measured. A character string (or something that will be converted to a character string). Defaults to the first level of \code{marks(X)}.}
  \item{j}{The type (mark value) of the points in \code{X} to which distances are measured. A character string (or something that will be converted to a character string). Defaults to the second level of \code{marks(X)}.}
  \item{r}{numeric vector. The values of the argument $r$ at which the distribution function $K_{ij}(r)$ should be evaluated. There is a sensible default. First-time users are strongly advised not to specify this argument. See below for important conditions on $r$.}
  \item{breaks}{This argument is for internal use only.}
  \item{correction}{A character vector containing any selection of the options \code{
"border", "bord.modif", "isotropic", "Ripley", "translate", "translation", "none"}
or \code{"best"}. It specifies the edge correction(s) to be applied.}
  \item{\dots}{Ignored}
  \item{ratio}{Logical. If \code{TRUE}, the numerator and denominator of each edge-corrected estimate will also be saved, for use in analysing replicated point patterns.}
}
\details{
This function \code{Kcross} and its companions \code{Kdot} and \code{Kmulti} are generalisations of the function \code{Kest} to multitype point patterns.

A multitype point pattern is a spatial pattern of points classified into a finite number of possible
“colours” or “types”. In the \\textbf{spatstat} package, a multitype pattern is represented as a single point pattern object in which the points carry marks, and the mark value attached to each point determines the type of that point.

The argument \code{X} must be a point pattern (object of class \code{"ppp"}) or any data that are acceptable to \code{as.ppp}. It must be a marked point pattern, and the mark vector \code{X$marks} must be a factor.

The arguments \code{i} and \code{j} will be interpreted as levels of the factor
\code{X$marks}. If \code{i} and \code{j} are missing, they default to the first and second level of the marks factor, respectively.

The “cross-type” (type $i$ to type $j$) $K$
function of a stationary multitype point process $X$ is defined so that $\\lambda_j K_{ij}(r)$
equals the expected number of additional random points of type $j$
within a distance $r$ of a typical point of type $i$ in the process $X$. Here
$\\lambda_j$ is the intensity of the type $j$ points, i.e. the expected number of points of type $j$ per unit area. The function $K_{ij}$ is determined by the second order moment properties of $X$.

An estimate of $K_{ij}(r)$ is a useful summary statistic in exploratory data analysis of a multitype point pattern. If the process of type $i$ points were independent of the process of type $j$ points, then $K_{ij}(r)$ would equal $\\pi r^2$. Deviations between the empirical $K_{ij}$ curve and the theoretical curve $\\pi r^2$ may suggest dependence between the points of types $i$ and $j$.

This algorithm estimates the distribution function
$K_{ij}(r)$ from the point pattern \code{X}. It assumes that \code{X} can be treated as a realisation of a stationary (spatially homogeneous) random spatial point process in the plane, observed through a bounded window. The window (which is specified in \code{X} as \code{X$window}) may have arbitrary shape. Biases due to edge effects are treated in the same manner as in \code{Kest}, using the border correction.

The argument $r$
is the vector of values for the distance $r$ at which $K_{ij}(r)$ should be evaluated. The values of $r$ must be increasing nonnegative numbers and the maximum $r$ value must not exceed the radius of the largest disc contained in the window.

The pair correlation function can also be applied to the result of \code{Kcross}; see \code{pcf}.
}
\value{
An object of class \code{"fv"} (see \code{fv.object}).

Essentially a data frame containing numeric columns


  \item{r}{the values of the argument $r$ at which the function $K_{ij}(r)$ has been estimated}
  \item{theo}{the theoretical value of $K_{ij}(r)$ for a marked Poisson process, namely $\\pi r^2$}

together with a column or columns named
\code{"border", "bord.modif", "iso"} and/or \code{"trans"}, according to the selected edge corrections. These columns contain estimates of the function $K_{ij}(r)$ obtained by the edge corrections named.

If \code{ratio=TRUE} then the return value also has two attributes called \code{"numerator"} and \code{"denominator"} which are \code{"fv"} objects containing the numerators and denominators of each estimate of $K(r)$.
%% ...
}
\section{Warnings}{
The arguments \code{i} and \code{j} are always interpreted as levels of the factor
\code{X$marks}. They are converted to character strings if they are not already character strings. The value \code{i=1} does \\textbf{not} refer to the first
level of the factor.
}
\references{Cressie, N.A.C. \\emph{Statistics for spatial data.} John Wiley and Sons, 1991.
Diggle, P.J. \\emph{Statistical analysis of spatial point patterns.} Academic Press, 1983.
Harkness, R.D and Isham, V. (1983) A bivariate spatial point pattern of ants’ nests. \\emph{Applied Statistics} \\textbf{32}, 293–303
Lotwick, H. W. and Silverman, B. W. (1982). Methods for analysing spatial processes of several types of points.
\\emph{J. Royal Statist. Soc. Ser. B} \\textbf{44}, 406–413.
Ripley, B.D. \\emph{Statistical inference for spatial processes.} Cambridge University Press, 1988.
Stoyan, D, Kendall, W.S. and Mecke, J. \\emph{Stochastic geometry and its applications.} 2nd edition. Springer Verlag, 1995.
}
\author{Adrian Baddeley \code{<Adrian.Baddeley@uwa.edu.au> http://www.maths.uwa.edu.au/~adrian/}
and Rolf Turner \code{<r.turner@auckland.ac.nz>}}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{Kdot, Kest, Kmulti, pcf}
}
\examples{
## Not run:
# amacrine cells data
K01 <- Kcross(amacrine, "off", "on")
plot(K01)
K10 <- Kcross(amacrine, "on", "off")
# synthetic example: point pattern with marks 0 and 1
pp <- runifpoispp(50)
pp <- pp %mark% factor(sample(0:1, npoints(pp), replace=TRUE))
K <- Kcross(pp, "0", "1")
K <- Kcross(pp, 0, 1) # equivalent
## End(Not run)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
