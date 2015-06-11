segment.outside<-function(img,blobsize=1)
{
  nimg<-img/max(img)
  dims<-dim(img)
  
  thresh<-c()
  
  XX<-dims[1]
  YY<-dims[1]
  ZZ<-dims[1]
  
  for (i in 1:5)
  {
    try({
      Y<-round(dims[2]*runif(1,.4,.6),0)
      Z<-round(dims[3]*runif(1,.4,.6),0)
      m<-mean(nimg[1:3,Y,Z])
      s<-sd(nimg[1:3,Y,Z])
      X<-4
      while(((m+3*s)>nimg[X,Y,Z])|(s<1e-4))
      {
        m<-mean(nimg[1:X,Y,Z])
        s<-sd(nimg[1:X,Y,Z])
        X<-X+1
      }
      thresh<-c(thresh,nimg[X+1,Y,Z])
    })
    
    try({
      Y<-round(dims[2]*runif(1,.4,.6),0)
      Z<-round(dims[3]*runif(1,.4,.6),0)
      m<-mean(nimg[XX-(0:2),Y,Z])
      s<-sd(nimg[XX-(0:2),Y,Z])
      X<-3
      while(((m+3*s)>nimg[XX-X,Y,Z])|(s<1e-4))
      {
        m<-mean(nimg[XX-(0:X),Y,Z])
        s<-sd(nimg[XX-(0:X),Y,Z])
        X<-X+1
      }
      thresh<-c(thresh,nimg[XX-X-1,Y,Z])
    })
    
    try({
      X<-round(dims[1]*runif(1,.4,.6),0)
      Z<-round(dims[3]*runif(1,.4,.6),0)
      m<-mean(nimg[X,1:3,Z])
      s<-sd(nimg[X,1:3,Z])
      Y<-4
      while(((m+3*s)>nimg[X,Y,Z])|(s<1e-4))
      {
        m<-mean(nimg[X,1:Y,Z])
        s<-sd(nimg[X,1:Y,Z])
        Y<-Y+1
      }
      thresh<-c(thresh,nimg[X,Y+1,Z])
    })
    
    try({
      X<-round(dims[1]*runif(1,.4,.6),0)
      Z<-round(dims[3]*runif(1,.4,.6),0)
      m<-mean(nimg[X,YY-(0:2),Z])
      s<-sd(nimg[X,YY-(0:2),Z])
      Y<-3
      while(((m+3*s)>nimg[X,YY-Y,Z])|(s<1e-4))
      {
        m<-mean(nimg[X,YY-(0:Y),Z])
        s<-sd(nimg[X,YY-(0:Y),Z])
        Y<-Y+1
      }
      thresh<-c(thresh,nimg[X,YY-Y-1,Z])
    })
  }
  thresh<-quantile(thresh,.1)
  cat(paste("Threshold is ",round(thresh*100,1),"%\n",sep=""))
  nimg<-array(ifelse(nimg<thresh,0,1),dims)
  cat("Starting Segmentation.\n")
  return(outside(nimg,0,blobsize))
}