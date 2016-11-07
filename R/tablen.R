#' Cross Tabulation and Table Creation (including empty classes)
#'
#' @param x R object with classes
#' @param m maximum number of classes
#' @param percentage boolean. If TRUE result is in percentages.
#' @param weight weight for each voxel
#' @param parallel Logical. Can we use parallel computing?
#'
#' @return vector with (weighted) counts (including empty classes)
#' @export
#' @author Volker Schmid 2013-2016
#' @examples 
#' x <- c(1,1,2,2,4,4,4)
#' table.n(x)
#' # [1] 2 2 0 3
#' table.n(x, m=5)
#' # [1] 2 2 0 3 0
#' table.n(x, weight=c(1,1,1,2,.5,.5,.5))
#' # [1] 2.0 3.0 0.0 1.5
#' 
table.n<-function(x,m=max(x,na.rm=TRUE), percentage=TRUE, weight=NULL, parallel=FALSE)
{
  if (!is.null(weight))return(table.n.weight(x,m,percentage,weight,parallel))
  cc<-1:m
  if(parallel)cc<-unlist(parallel::mclapply(cc,function(i,x)sum(x==i,na.rm=TRUE),x=x))
  else cc<-unlist(lapply(cc,function(i,x)sum(x==i,na.rm=TRUE),x=x))   
  if (percentage)cc<-cc/sum(cc)
  return(cc)
}

table.n.weight<-function(x,m,percentage,weight,parallel){
cc<-1:m
if(parallel)
{
  cc<-unlist(parallel::mclapply(cc,function(i,x,w)sum(w*(x==i),na.rm=TRUE),x=x,w=weight))
}
else
{
  cc<-unlist(lapply(cc,function(i,x,w)sum(w*(x==i),na.rm=TRUE),x=x, w=weight))    
}
if (percentage)cc<-cc/sum(cc)
return(cc)
}