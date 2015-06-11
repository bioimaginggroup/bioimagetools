K.cross.3D.envelope<-function(X,Y,Z,X2,Y2,Z2,range,bw.int,psz,width,parallel=FALSE,N=100,use.intensity=TRUE)
  {
K.envelope<-c()
if(use.intensity)intensity.red<-intensity3D(X+range,Y+range,Z+range,bw=bw.int,psz=psz)
if(!use.intensity){intensity.red<-NULL;intensity.green<-NULL}
shuffle<-function(i)
  {
    XX<-X2+runif(1,0,2*range)
    YY<-Y2+runif(1,0,2*range)
    ZZ<-Z2+runif(1,0,2*range)
    if(use.intensity)intensity.green<-intensity3D(XX,YY,ZZ,bw=bw.int,psz=psz)
    K.temp<-K.cross.3D(X,Y,Z,XX,YY,ZZ,psz=psz,width=width,intensity=intensity.red,intensity2=intensity.green,parallel=parallel)
    return(K.temp$y)
  }
if(!parallel)envelope<-lapply(1:N,shuffle)
if(parallel)envelope<-mclapply(1:N,shuffle)
envelope<-unlist(envelope)
envelope<-array(envelope,c(length(envelope)/N,N))

if(use.intensity)intensity.green<-intensity3D(X2+range,Y2+range,Z2+range,bw=bw.int,psz=psz)
K<-K.cross.3D(X+range,Y+range,Z+range,X2+range,Y2+range,Z2+range,psz=psz,width=width,intensity=intensity.red,intensity2=intensity.green,parallel=parallel)

#for (i in 1:N)
#envelope[,i]<-K$y-envelope[,i]

e.max<-apply(envelope,1,max)
e.min<-apply(envelope,1,min)
return(list("y"=K$y,"x"=K$x,"max"=e.max,"min"=e.min,"envelope"=envelope))
}



