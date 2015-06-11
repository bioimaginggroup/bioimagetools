diameter <- function(obj)
{
  Z<-max(obj)
  if(library(parallel))list<-mclapply(1:Z,.diameter,obj)
  if(library(parallel))list<-lapply(1:Z,.diameter,obj)
  return(list)
}

.diameter<-function(i,obj)
{
  one<-obj==i
  X<-range(which(apply(one,1,any)))
  Y<-range(which(apply(one,2,any)))
  Z<-range(which(apply(one,3,any)))
  one<-one[X[1]:X[2],Y[1]:Y[2],Z[1]:Z[2]]
  winkel=1/2
  
}
