#table0<-function(x,m=max(x))
#{
#  cc<-rep(0,m)
#  for (i in 1:m)
#    cc[i]<-sum(x==i,na.rm=TRUE)
#  return(cc)
#}

table.n<-function(x,m=max(x,na.rm=TRUE))
{
  cc<-1:m
  if(require(parallel))
  {
    cc<-unlist(mclapply(cc,function(i,x)sum(x==i,na.rm=TRUE),x=x))
  }
  else
  {
    cc<-unlist(lapply(cc,function(i,x)sum(x==i,na.rm=TRUE),x=x))    
  }
  return(cc)
}