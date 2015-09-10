#' image for microscopy
#'
#' @param x Image, 2D Matrix
#' @param col Color, "grey", "red" ("r"), "green" ("g") or "blue" ("b")
#' @param ... other parameters for graphics::image
#'
#' @return no return
#' @export
image<-function(x,col="grey",...)
{
  a<-seq(0,1,length=1000)
  b=rep(0,1000)
  if(col=="grey")colo=grey(a)
  if(col=="red"|col=="r")colo=rgb(a,b,b)
  if(col=="green"|col=="g")colo=rgb(b,a,b)
  if(col=="blue"|col=="b")colo=rgb(b,b,a)
  graphics::image(1:dim(x)[1],1:dim(x)[2],x,axes=FALSE,col=colo,xlab="",ylab="",...)
}

