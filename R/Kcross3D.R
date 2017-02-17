#' K-function cross-type in 3D
#'
#' @description Calculates an estimate of the cross-type K-function for a multitype point pattern.
#'
#' @param X X coordinate of first observed point pattern in microns.
#' @param Y Y coordinate
#' @param Z Z coordinate
#' @param X2 X coordinate of second observed point pattern
#' @param Y2 Y coordinate
#' @param Z2 Z coordinate
#' @param psz pointsize used for discetization. Smaller values are more precise, but need more computation time.
#' @param width maximum distance
#' @param intensity intensity of first pattern. Only if \deqn{\lambda(s)!=\lambda}.
#' @param intensity2 intensity of second pattern
#' @param parallel Logical. Can we use parallel computing?
#' @param verbose Plot verbose information
#'
#' @return a list of breaks and counts.
#' @export
K.cross.3D<-
function(X,Y,Z,X2,Y2,Z2,psz=25,width=1,intensity=NULL,intensity2=NULL,parallel=FALSE,verbose=FALSE)
{
actualwidth=width
width=1.1*width
N<-length(X)
N2<-length(X2)
width.psz<-floor(width*psz)+1

x<-round(X*psz,0)+width.psz+1
y<-round(Y*psz,0)+width.psz+1
z<-round(Z*psz,0)+width.psz+1
x2<-round(X2*psz,0)+width.psz+1
y2<-round(Y2*psz,0)+width.psz+1
z2<-round(Z2*psz,0)+width.psz+1

ID<-1:N
ID2<-1:N2
if(!is.null(intensity))ID.matrix<-ID.matrix2<-array(NA,c(max(dim(intensity)[1],dim(intensity2)[1])+(2*width.psz+1),max(dim(intensity)[2],dim(intensity2)[2])+(2*width.psz+1),max(dim(intensity)[3],dim(intensity2)[3])+(2*width.psz+1)))
if(is.null(intensity))ID.matrix<-ID.matrix2<-array(NA,c(round(max(c(X,X2))*psz,0)+(2*width.psz+1),round(max(c(Y,Y2))*psz,0)+(2*width.psz+1),round(max(c(Z,Z2))*psz,0)+(2*width.psz+1)))

if (!is.null(intensity))
{
intensity.mx<-array(0,dim(ID.matrix))
intensity.mx[width.psz+(1:dim(intensity)[1]),width.psz+(1:dim(intensity)[2]),width.psz+(1:dim(intensity)[3])]<-intensity
intensity<-TRUE
}
if (!is.null(intensity2))
{
intensity.mx2<-array(0,dim(ID.matrix2))
intensity.mx2[width.psz+(1:dim(intensity2)[1]),width.psz+(1:dim(intensity2)[2]),width.psz+(1:dim(intensity2)[3])]<-intensity2
intensity2<-TRUE
}

for (i in 1:N)
{
ID.matrix[x[i],y[i],z[i]]<-ID[i]
}
for (i in 1:N2)
{
ID.matrix2[x2[i],y2[i],z2[i]]<-ID2[i]
}

dist<-0:width.psz

extractneighbour<-function(i,verbose)
{
if(verbose)cat(".")
res<-NULL
int<-NULL
neighbours<-ID.matrix2[x[i]+dist,y[i]+dist,z[i]+dist]
neighbours<-as.vector(neighbours)
neighbours<-neighbours[!is.na(neighbours)]
for (j in neighbours)
{
res<-c(res,sqrt(sum((c(X[i],Y[i],Z[i])-c(X2[j],Y2[j],Z2[j]))^2)))
if (!is.null(intensity))int<-c(int,intensity.mx[x[i],y[i],z[i]]*intensity.mx2[x2[j],y2[j],z2[j]])
}
return(rbind(res,int))
}

if(parallel)dist<-parallel::mclapply(1:N,extractneighbour,verbose)
if(!parallel)dist<-lapply(1:N,extractneighbour,verbose)
dist<-unlist(dist)
if(!is.null(intensity))
{
weight<-dist[seq(2,length(dist),by=2)]
dist<-dist[seq(1,length(dist),by=2)]
weight<-weight[dist<=width]
dist<-dist[dist<=width]
}
else
{
dist<-dist[dist<=width]
weight<-rep(1,length(dist))
}


cumsumi<-function(cut,x,weight)
return(sum((1/weight)[x<=cut]))

breaks=seq(0,width,length=500)
counts<-lapply(breaks,cumsumi,x=dist,weight=weight)
counts<-unlist(counts)
counts[is.na(counts)]<-0
return(list("x"=breaks,"y"=counts))
}

