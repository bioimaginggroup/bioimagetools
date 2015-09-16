#' image for microscopy
#'
#' @param x Image, 2D Matrix
#' @param col Color, "grey", "red" ("r"), "green" ("g") or "blue" ("b")
#' @param ... other parameters for graphics::image
#'
#' @return no return
#' @export
image<-function(x,col="grey",...)
{
  if(length(dim(x))==3)image.rgb(x,...)
  a<-seq(0,1,length=1000)
  b=rep(0,1000)
  if(col=="grey")colo=grey(a)
  if(col=="red"|col=="r")colo=rgb(a,b,b)
  if(col=="green"|col=="g")colo=rgb(b,a,b)
  if(col=="blue"|col=="b")colo=rgb(b,b,a)
  x<-aperm(x,c(2,1))
  x[,dim(x)[2]:1]<-x
  graphics::image(1:dim(x)[1],1:dim(x)[2],x,axes=FALSE,col=colo,xlab="",ylab="",...)
}

image.rgb<-function(x,n=100,...)
{
  n<-100
  a<-b<-c<-seq(0,1,length=100)
  a<-rep(a,each=n*n)
  b<-rep(rep(b,each=n),times=n)
  c<-rep(c,times=n*n)
  colo=rgb(a,b,c)
  xx<-floor(1+x[,,1]*(n-1))*n*n+floor(1+x[,,2]*(n-1))*n+floor(1+x[,,3]*(n-1))
  xx<-aperm(xx,c(2,1))
  xx[,dim(xx)[2]:1]<-xx
  graphics::image(1:dim(x)[2],1:dim(x)[1],xx,axes=FALSE,col=colo,xlab="",ylab="",zlim=c(1,n*n*n),...)
}
