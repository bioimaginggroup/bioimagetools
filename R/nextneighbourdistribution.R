nextneighbourdistribution<-function(X,Y,Z,X2=X,Y2=Y,Z2=Z,same=TRUE,psz=25,main="Next neighbour distribution",file=NULL, return=FALSE)
{
  if(!is.null(file))png(file)
  nn<-nextneighbours(X,Y,Z,X2,Y2,Z2,same=same,psz=psz)
  hist.nn<-hist(nn,freq=FALSE,n=100,xlab="Distance",main=main)
  lines(density(nn,na.rm=TRUE))
  if(!is.null(file))dev.off()
  if(return)return(hist.nn)
}