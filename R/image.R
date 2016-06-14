#' image for microscopy
#'
#' @param x Image, 2D or 3D Matrix
#' @param col Color: "grey", "red" ("r"), "green" ("g") or "blue" ("b"), "rgb" for 3D matrices
#' @param low minimal value of shown intensity
#' @param up maximal value of shown intensity
#' @param ... other parameters for graphics::image
#'
#' @return no return
#' @export
img<-function(x,col="grey",low=0,up=NULL,...)
{
  if (is.null(up))up=ifelse(length(dim(x))==2,max(x),apply(x,3,max))
  if(col=="rgb"){img.rgb(x,low=low, up=up, ...);return()}
  a<-seq(0,1,length=1000)
  b=rep(0,1000)
  if(col=="grey")colo=grey(a)
  if(col=="red"|col=="r")colo=rgb(a,b,b)
  if(col=="green"|col=="g")colo=rgb(b,a,b)
  if(col=="blue"|col=="b")colo=rgb(b,b,a)
  x<-aperm(x,c(2,1))
  x[,dim(x)[2]:1]<-x
  #x<-x-low
  #x=x/(low-up)
  #x[x<0]<-0
  #x[x>1]<-1
  graphics::image(1:dim(x)[1],1:dim(x)[2],x,axes=FALSE,col=colo,xlab="",ylab="", zlim=c(0,1),...)
}

img.rgb<-function(x,n=100,low,up,...)
{
  n<-100
  a<-b<-c<-seq(0,1,length=100)
  a<-rep(a,each=n*n)
  b<-rep(rep(b,each=n),times=n)
  c<-rep(c,times=n*n)
  colo=rgb(a,b,c)
  if (length(low)==1)x=x-low
  if (length(low)==3)for (i in 1:3)x[,,i]=x[,,i]-low[i]
  if (length(up)==1&length(low)==1)x=x/(up-low)
  if (length(up)==3&length(low)==3)for (i in 1:3)x[,,i]=x[,,i]/(up[i]-low[i])
  if (length(up)==1&length(low)==3)for (i in 1:3)x[,,i]=x[,,i]/(up-low[i])
  if (length(up)==3&length(low)==1)for (i in 1:3)x[,,i]=x[,,i]/(up[i]-low)
  x[x<0]<-0
  x[x>1]<-1
  
  xx<-floor(1+x[,,1]*(n-1))*n*n+floor(1+x[,,2]*(n-1))*n+floor(1+x[,,3]*(n-1))
  xx<-aperm(xx,c(2,1))
  xx[,dim(xx)[2]:1]<-xx
  
  graphics::image(1:dim(x)[2],1:dim(x)[1],xx,axes=FALSE,col=colo,xlab="",ylab="",zlim=c(1,n*n*n),...)
}
