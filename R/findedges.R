find.edges <- function(object)
{
  X<-dim(object)[1]
  Y<-dim(object)[2]
  Z<-dim(object)[3]
  edges<-array(FALSE,c(X,Y,Z))
  if (X>1)
  {
    object2<-array(FALSE,c(X,Y,Z))
    object2[1:(X-1),,]<-object[2:X,,]
    edges<-edges|(object&!object2)
    object2<-array(FALSE,c(X,Y,Z))
    object2[2:X,,]<-object[1:(X-1),,]
    edges<-edges|(object&!object2)
  }
  if (Y>1)
  {
    object2<-array(FALSE,c(X,Y,Z))
    object2[,1:(Y-1),]<-object[,2:Y,]
    edges<-edges|(object&!object2)
    object2<-array(FALSE,c(X,Y,Z))
    object2[,2:Y,]<-object[,1:(Y-1),]
    edges<-edges|(object&!object2)
  }
  if (Z>1)
  {
    object2<-array(FALSE,c(X,Y,Z))
    object2[,,1:(Z-1)]<-object[,,2:Z]
    edges<-edges|(object&!object2)
    object2<-array(FALSE,c(X,Y,Z))
    object2[,,2:Z]<-object[,,1:(Z-1)]
    edges<-edges|(object&!object2)
  }
  return(list("mask"=edges,"coords"=which(edges,arr.ind=TRUE)))
}