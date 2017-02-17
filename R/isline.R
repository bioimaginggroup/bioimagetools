is.line<-function(a,b,object)
{
  N<-2*sum(abs(a-b))
  x<-round(seq(a[1],b[1],length=N))
  y<-round(seq(a[2],b[2],length=N))
  z<-round(seq(a[3],b[3],length=N))
  do<-TRUE
  for (i in 1:N)
    do<-do&object[x[i],y[i],z[i]]
  return(do)
}