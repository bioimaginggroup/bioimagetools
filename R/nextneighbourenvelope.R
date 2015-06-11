nextneighbourenvelope<-function(X,Y,Z,X2,Y2,Z2,psz=25,n=100,range=.5)
{


do<-function(i,X,Y,Z,X2,Y2,Z2,psz,range)
{
X2<-X2+runif(1,-range,range)
Y2<-Y2+runif(1,-range,range)
Z2<-Z2+runif(1,-range,range)
redgreen<-nextneighbourdistribution(X,Y,Z,X2,Y2,Z2,same=TRUE,psz=psz)
hist<-hist(redgreen,n=100,plot=FALSE)
return(list("x"=hist$mids,"y"=cumsum(hist$density)/100))
}

runs<-lapply(1:n,do,X,Y,Z,X2,Y2,Z2,psz=psz,range=range)
#run.max<-apply(runs,1,max)
#run.min<-apply(runs,1,min)


true<-nextneighbourdistribution(X,Y,Z,X2,Y2,Z2,same=TRUE,psz=psz)
truehist<-hist(true,n=100,plot=FALSE)
true<-cumsum(hist$density)/100

return(list("true"=true,"runs"=runs,"x"=truehist$mids))
}

