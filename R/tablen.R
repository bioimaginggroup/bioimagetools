#' Cross Tabulation and Table Creation (including empty classes)
#'
#' @param x R object to be tabulated
#' @param m Maximum number of classes
#' @param parallel Logical. Should we use parallel computing?
#'
#' @return
#' @export
#'
#' @examples
#' 
table.n<-function(x, m=max(x,na.rm=TRUE), parallel=require(parallel))
{
  cc<-1:m
  if(parallel)cc<-unlist(parallel::mclapply(cc,function(i,x)sum(x==i,na.rm=TRUE),x=x))
  else cc<-unlist(lapply(cc,function(i,x)sum(x==i,na.rm=TRUE),x=x))    
  return(cc)
}