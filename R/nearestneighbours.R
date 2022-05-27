nearest.neighbours<-function(X,Y,Z,X2=X,Y2=Y,Z2=Z,same=TRUE,psz=25)
{
  N<-length(X)
  N2<-length(X2)
  
  x<-floor(X*psz)+(2*psz)+1
  y<-floor(Y*psz)+(2*psz)+1
  z<-floor(Z*psz)+(2*psz)+1
  x2<-floor(X2*psz)+(2*psz)+1
  y2<-floor(Y2*psz)+(2*psz)+1
  z2<-floor(Z2*psz)+(2*psz)+1
  ID<-1:N2
  ID.matrix<-array(NA,c(floor(max(X2)*psz)+(4*(psz)+1),floor(max(Y2)*psz)+(4*(psz)+1),floor(max(Z2)*psz)+(4*(psz)+1)))
  
  for (i in 1:N2)
  {
    ID.matrix[x2[i],y2[i],z2[i]]<-ID[i]
  }
  
  nextneighbour<-rep(NA,N)
  nr<-ifelse(same,1,0)
  for (i in 1:N)
  {
    do<-TRUE
    dist<-0
    while(do)
    {
      dist<-min(dist-1):max(dist+1)
      do<-try(sum(!is.na(ID.matrix[x[i]+dist,y[i]+dist,z[i]+dist]))<=nr,silent=TRUE)
      if(inherits(do,"try-error")){do<-FALSE;dist<-min(dist+1):max(dist-1)}    
    }
    
    potential<-try(as.vector(ID.matrix[x[i]+dist,y[i]+dist,z[i]+dist]),silent=TRUE)
    if(inherits(potential,"try-error")){potential<-c()}
    
    potential<-potential[!is.na(potential)]
    if(same)potential<-potential[potential!=i]
    
    if (length(potential)>1)
    {
      d<-c()
      for (j in potential)
        d<-c(d,sqrt(sum((c(X[i],Y[i],Z[i])-c(X2[j],Y2[j],Z2[j]))^2)))
      test.d<-which(d==min(d))
      test.d<-sample(test.d,1)
      nextneighbour[i]<-potential[test.d]
    }
    else
    {
      if(length(potential)==1)nextneighbour[i]<-potential
    }
  }
  
  dist.nextneighbour<-cbind(X,Y,Z)-cbind(X2[nextneighbour],Y2[nextneighbour],Z2[nextneighbour])
  dist.nextneighbour<-dist.nextneighbour^2
  dist.nextneighbour<-apply(dist.nextneighbour,1,sum)
  dist.nextneighbour<-sqrt(dist.nextneighbour)
  return(dist.nextneighbour)
}