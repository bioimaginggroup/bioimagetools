#' Intensity of a 3d Dataset or a Model
#'
#' @description Computing the intensity of a 3d point pattern using kernel smoothing.
#' 
#' @param X X coordinate
#' @param Y Y coordinate
#' @param Z Z coordinate
#' @param bw bandwidth
#' @param psz pointsize used for discretization (large: fast, but not precise)
#' @param kernel "Square" or "Uniform"
#'
#' @return 3d Array
#' @export
#'
intensity3D<-function(X,Y,Z,bw=NULL,psz=25,kernel="Square")
{
N<-length(X)

if (is.null(bw))break("please specify bw")

bw.psz<-floor(bw*psz)+1
x<-round(X*psz,0)+bw.psz+1
y<-round(Y*psz,0)+bw.psz+1
z<-round(Z*psz,0)+bw.psz+1

ID<-1:N
ID.matrix<-array(NA,c(round(max(X)*psz,0)+(2*bw.psz+1),round(max(Y)*psz,0)+(2*bw.psz+1),round(max(Z)*psz,0)+(2*bw.psz+1)))

for (i in 1:N)
{
ID.matrix[x[i],y[i],z[i]]<-ID[i]
}

if (kernel=="Square")
{
kernel<-array(0,c(2*bw.psz+1,2*bw.psz+1,2*bw.psz+1))
mid<-c(bw.psz+1,bw.psz+1,bw.psz+1)/psz
for (i in 1:(2*bw.psz+1))
for (j in 1:(2*bw.psz+1))
for (k in 1:(2*bw.psz+1))
kernel[i,j,k]<- max(0,bw-(sqrt(sum((c(i/psz,j/psz,k/psz)-mid)^2))))
kernel<-(psz^3)*kernel/sum(kernel)
}
if (kernel=="Uniform")
{
kernel<-array(0,c(2*bw.psz+1,2*bw.psz+1,2*bw.psz+1))
mid<-c(bw.psz+1,bw.psz+1,bw.psz+1)/psz
for (i in 1:(2*bw.psz+1))
for (j in 1:(2*bw.psz+1))
for (k in 1:(2*bw.psz+1))
kernel[i,j,k]<- ifelse(bw<(sqrt(sum((c(i/psz,j/psz,k/psz)-mid)^2))),0,1)
kernel<-(psz^3)*kernel/sum(kernel)
}

intensity<-array(0,dim(ID.matrix))

for (i in 1:N)
{
xx<-((x[i]-bw.psz):(x[i]+bw.psz))
yy<-((y[i]-bw.psz):(y[i]+bw.psz))
zz<-((z[i]-bw.psz):(z[i]+bw.psz))
intensity[xx,yy,zz]<-intensity[xx,yy,zz]+kernel
}
dims<-dim(intensity)

intensity<-(intensity[(bw.psz+1):(dims[1]-bw.psz),(bw.psz+1):(dims[2]-bw.psz),(bw.psz+1):(dims[3]-bw.psz)])
intensity<-N*intensity/mean(intensity)/(max(X)*max(Y)*max(Z))
return(intensity)
}
