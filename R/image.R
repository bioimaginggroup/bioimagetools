#' Display an image stack
#'
#' @param x Image, 2D or 3D Matrix
#' @param z slice to show, default: NULL, expects x to be 2d or 2d+channels
#' @param ch channel. Default: NULL, either only one channel, rgb or channel will be assumed from col
#' @param mask mask for image, voxel outside the mask will be transparent (default: NULL, no mask)
#' @param col Color, either a character ("grey", "greyinvert", "red" ("r"), "green" ("g") or "blue" ("b"), rgb" for 3D matrices),
#'            a vector of character with hex rgb values or a function.
#' @param low minimal value of shown intensity. Default: NULL: use min(x, na.rm=TRUE).
#' @param up maximal value of shown intensity. Default: NULL: use max(x, na.rm=TRUE).
#' @param ... other parameters for graphics::image
#'
#' @return no return
#' @export
#' @import grDevices
img<-function(x,z=NULL,ch=NULL,mask=NULL,col="grey",low=NULL,up=NULL,...)
{
  if(class(col)!="function")
  {
    if (class(col)!="character")stop("col must be character or function")
    if(length(col)==1)if(col=="rgb"){return(img.rgb(x, z=z, mask=mask, low=low, up=up, ...))}
  }

  x[mask==0]<-NA
  if (is.null(low))low=min(x,na.rm=TRUE)
  if (is.null(up))up=max(x,na.rm=TRUE)
  D <- length(dim(x))

  if(is.null(ch)&length(col)==1)  {
    if(col=="r"|col=="red")cha=1
    if(col=="g"|col=="green")cha=2
    if(col=="b"|col=="blue"|col=="grey"|col=="greyinvert")cha=3
  }
  else
  {
    cha=ch
  }
  
  # sanity checks and dimension reduction
  dd <- 2 +!is.null(z)
  if (dd==D&!is.null(ch))warning("ch ignored.")
  if (dd==D)cha=NULL
    
  if (is.null(cha)&is.null(z)&D!=2){stop("Dimension of x is wrong.")}
  if (is.null(cha)&!is.null(z)&(!(D==2|D==3))){stop("Dimension of x is wrong.")}
  if (!is.null(z))
  {
    if (is.null(cha)&D==2)warning("x is 2d, z ignored.")
    if (is.null(cha)&D==3)x<-x[,,z]
    if (!is.null(cha)&D==4)x<-x[,,cha,z]
  }
  if (is.null(z)&!is.null(cha))
  {
    if(D==3)x<-x[,,cha]
  }
  
  
  T=1000
  a<-seq(0,1,length=T)
  b=rep(0,T)
  if(class(col)=="character")
      {
        if (length(col)>1)
        {
          colo=col
        }
    else
    {
        colo=switch(col,
               "grey" = grey(a),
               "greyinvert" = rev(grey(a)),
               "red" = rgb(a,b,b),
               "r" = rgb(a,b,b),
               "green" = rgb(b,a,b),
               "g" = rgb(b,a,b),
               "blue" = rgb(b,b,a),
               "b" = rgb(b,b,a)
               )
    }
  }
  if (class(col)=="function")
  {
    colo=col(a)
  }
  x<-aperm(x,c(2,1))
  x[,dim(x)[2]:1]<-x
  x<-x-low
  x=x/(up-low)
  x[x<0]<-0
  x[x>1]<-1
  image(1:dim(x)[1],1:dim(x)[2],array(0,dim(x)),axes=FALSE,xlab="",ylab="",col="black",...)
  image(1:dim(x)[1],1:dim(x)[2],x,col=colo,zlim=c(0,1),add=TRUE)
}

img.rgb<-function(x,z=NULL,mask=NULL, n=100,low,up,...)
{
  n<-100
  a<-b<-c<-seq(0,1,length=100)
  a<-rep(a,each=n*n)
  b<-rep(rep(b,each=n),times=n)
  c<-rep(c,times=n*n)
  colo=grDevices::rgb(a,b,c)
  if (length(low)==1)x=x-low
  if (length(low)==3)for (i in 1:3)x[,,i]=x[,,i]-low[i]
  if (length(up)==1&length(low)==1)x=x/(up-low)
  if (length(up)==3&length(low)==3)for (i in 1:3)x[,,i]=x[,,i]/(up[i]-low[i])
  if (length(up)==1&length(low)==3)for (i in 1:3)x[,,i]=x[,,i]/(up-low[i])
  if (length(up)==3&length(low)==1)for (i in 1:3)x[,,i]=x[,,i]/(up[i]-low)
  x[x<0]<-0
  x[x>1]<-1

  if(!is.null(z))x<-x[,,,z];mask<-mask[,,z]
  if(!is.null(mask))x[mask==0]<-NA
  
  xx<-floor(1+x[,,1]*(n-1))*n*n+floor(1+x[,,2]*(n-1))*n+floor(1+x[,,3]*(n-1))
  xx<-aperm(xx,c(2,1))
  xx[,dim(xx)[2]:1]<-xx
  
  graphics::image(1:dim(x)[2],1:dim(x)[1],xx,axes=FALSE,col=colo,xlab="",ylab="",zlim=c(1,n*n*n),...)
}
