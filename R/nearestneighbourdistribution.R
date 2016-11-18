#' Nearest neighbour distribution (D curve) 
#'
#' @param X X coordinates of point pattern 1
#' @param Y Y coordinates of point pattern 1
#' @param Z Z coordinates of point pattern 1
#' @param X2 X coordinates of point pattern 2
#' @param Y2 Y coordinates of point pattern 2
#' @param Z2 Z coordinates of point pattern 2
#' @param same binary, FLASE for cross D curve
#' @param psz pointsize for discretization
#' @param main Title for graphic
#' @param file File name for PNG file. If NULL, plots to standard device. 
#' @param return Logical. Return histogramm?
#'
#' @return histogramm of nearest neighbours
#' @export
#' @examples
#' p<-read.csv(system.file("extdata","cell.csv",package="bioimagetools")) 
#' nearest.neighbour.distribution(p$X,p$Y,p$Z)
nearest.neighbour.distribution<-function(X,Y,Z,X2=X,Y2=Y,Z2=Z,same=TRUE,psz=25,main="Nearest neighbour distribution",file=NULL, return=FALSE)
{
  if(!is.null(file))png(file)
  nn<-nearest.neighbours(X,Y,Z,X2,Y2,Z2,same=same,psz=psz)
  hist.nn<-hist(nn,freq=FALSE,n=100,xlab="Distance",main=main)
  graphics::lines(density(nn,na.rm=TRUE))
  if(!is.null(file))dev.off()
  if(return)return(hist.nn)
}