mexican.hat.brush<-function(n=7,sigma2=1)
{
  matrix<-array(0,c(n,n))
  m<-(n+1)/2
  for (x in (1:n)-m)
    for (y in (1:n)-m)
    {
      matrix[x+m,y+m]<- -exp(-(x*x+y*y)/2/sigma2)*(1-(x*x+y*y/2/sigma2))/sigma2/sigma2/pi
    }
  matrix<-matrix/sum(matrix)
}
