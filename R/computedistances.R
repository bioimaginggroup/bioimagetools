compute.distances<-function(list,Z.scale=1)
{
  N<-dim(list)[1]
  N1<-rep(1:(N-1),(N-1):1)
  N2<-2:N
  for (i in 3:N)N2<-c(N2,i:N)
  all<-cbind(list[N1,],list[N2,])
  dist <- apply(all,1,.distance)
  return(cbind(N1,N2,dist))
}

.distance<-function(p,Z.scale=1)
{
  return(sqrt((p[4]-p[1])^2+(p[5]-p[2])^2+((p[6]-p[3])/Z.scale)^2))
}
