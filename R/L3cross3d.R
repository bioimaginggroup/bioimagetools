L.cross.3D<-function(X,Y,Z,X2,Y2,Z2,psz=25,width=1,intensity=NULL,intensity2=NULL,parallel=FALSE)
{
K<-K.cross.3D(X,Y,Z,X2,Y2,Z2,25,1,intensity,intensity2,parallel)
breaks<-K$x
counts<-K$y
counts<-(counts*3/4/pi)^(1/3)
counts<-counts-breaks
return(list("x"=breaks,"y"=counts))
}

if(0){
plot(breaks,counts)
plot(breaks,(counts*3/4/pi/1000)^(1/3)-breaks)
#dr<-width/100
#counts<-counts/N/4/pi/dr/(seq(0,width,length=100)[-100])/(seq(0,width,length=100)[-100])
}

