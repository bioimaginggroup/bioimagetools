#' A function to compute the distance from spots to borders of classes
#'
#' @param points Data frame containing the coordinates of points in microns as X-, Y-, and Z-variables.
#' @param img.classes 3D array (or image) of classes for each voxel.
#' @param x.microns Size of image in x-direction in microns.
#' @param y.microns Size of image in y-direction in microns.
#' @param z.microns Size of image in z-direction in microns.
#' @param class1 Which class is the reference class. If is.null(class2), the function computes the distance of points to the border of class (in img.classes).
#' @param class2 Which class is the second reference class. If not is.null(class2), the function computes the distance of points from the border between classes class1 and class2. Default: class2=NULL.
#' @param mask Array of mask. Needs to have same dimension as img.classes. Only voxels with mask[i,j,k]==TRUE are used. Default: array(TRUE,dim(img.classes))
#' @param voxel Logial. If TRUE, points coordinates are given as voxels rathers than in microns.
#' @param hist Automatically plot histogram using hist() function. Default: FALSE.
#' @param main If (hist) title of histogramm. Default: "Minimal distance to border".
#' @param xlab If (hist) description of x axis. Default: "Distance in Microns".
#' @param xlim If (hist) vector of range of x axis (in microns). Default: c(-.3,.3)
#' @param n If (hist) number of bins used in hist(). Default: 20.
#' @param stats If (hist) write statistics into plot. Default: TRUE.
#' @param file If (hist) the file name of the produced png. If NULL, the histogram is plotted to the standard device. Default: NULL.  
#' @param silent if TRUE, function remains silent during running time 
#' @param parallel Logical. Can we use parallel computing?
#'
#' @details This function computes the distances from points to the border of a class or the border between two classes. For the latter, only points in these two classes are used.
#' 
#' @return The function returns a vector with distances. Negative values correspond to points lying in class1. 
#' @export
#' @import parallel
#' @import stats
#' @import grDevices
#' 
#' @note Warning: So far no consistency check for arguments is done. E.g., distance2border(randompoints,img.classes=array(1,c(100,100,2)),3,3,1,class1=2) will fail with some cryptic error message (because class1 > max(img.classes)).
#' @examples 
#' \dontrun{
#' #simulate random data
#' randompoints<-data.frame("X"=runif(100,0,3),"Y"=runif(100,0,3),"Z"=runif(100,0,.5))
#' # coordinates in microns!
#' plot(randompoints$X,randompoints$Y,xlim=c(0,3),ylim=c(0,3),pch=19)
#' 
#' # points in a circle
#' circlepoints<-read.table(system.file("extdata","kreispunkte.table",
#'                                package="bioimagetools"),header=TRUE)
#' plot(circlepoints$X,circlepoints$Y,xlim=c(0,3),ylim=c(0,3),pch=19)
#' 
#' # a circle like image
#' img<-readTIF(system.file("extdata","kringel.tif",package="bioimagetools"))
#' img<-array(img,dim(img)) # save as array for easier handling
#' img(img, z=1)
#' 
#' #and a mask
#' mask<-readTIF(system.file("extdata","amask.tif",package="bioimagetools"))
#' img(mask, z=1, col="greyinverted")
#' 
#' xy.microns <- 3 # size in x and y direction (microns)
#' z.microns <- 0.5 # size in z direction (microns)
#' 
#' # distance from points to class 
#' d1<-distance2border(randompoints, img, xy.microns, xy.microns, z.microns, class1=1,hist=TRUE)
#' d2<-distance2border(circlepoints, img, xy.microns, xy.microns, z.microns, class1=1,hist=FALSE)
#' plot(density(d2),type="l")
#' lines(c(0,0),c(0,10),lty=3)
#' lines(density(d1),col="blue")
#' 
#' # use mask, should give some small changes
#' d3<-distance2border(circlepoints, img, xy.microns, xy.microns, z.microns, 
#'                                                 class1=1,mask=mask,hist=FALSE)
#' plot(density(d2),type="l")
#' lines(c(0,0),c(0,10),lty=3)
#' lines(density(d3),col="blue")
#' 
#' # distance from border between classes
#' anotherimg<-img+mask
#' image(seq(0,3,length=300),seq(0,3,length=300),anotherimg[,,1])
#' points(circlepoints,pch=19)
#' d4<-distance2border(circlepoints, anotherimg, xy.microns, xy.microns, z.microns, 
#'                                                                class1=1,class2=2)
#' plot(density(d4),lwd=2)
#' 
#' # this should give the same answer
#' d5<-distance2border(circlepoints, anotherimg, xy.microns, xy.microns, z.microns, 
#'                                                                 class1=2,class2=1)
#' lines(density(-d5),lty=3,col="blue",lwd=1.5)
#' }
 
distance2border<-function (points, img.classes, x.microns, y.microns, z.microns, 
                           class1, class2 = NULL, mask = array(TRUE, dim(img.classes)), voxel=FALSE,
                           hist = FALSE, main = "Minimal distance to border", xlab = "Distance in Microns", 
                           xlim = c(-0.3, 0.3), n = 20, stats = TRUE, file = NULL, silent=FALSE, parallel=FALSE) 
{
  dims <- dim(img.classes)
  X <- dims[1]
  Y <- dims[2]
  Z <- dims[3]
  
  if(voxel)
  {
    points.discrete <- points
    points <- data.frame(x = (points.discrete[,1]-1)*x.microns/X, 
                                  y = (points.discrete[,2]-1)*y.microns/Y, z = (points.discrete[,3]-1)*z.microns/Z)
  }
  
  if(!voxel)
    points.discrete <- data.frame(x = 1 + floor(X * points[,1]/x.microns), 
                                y = 1 + floor(Y * points[,2]/y.microns), z = 1 + floor(Z * points[,3]/z.microns))
  
  
  if(!silent)cat("-")
 
  if(parallel)valid<-mclapply(1:dim(points.discrete)[1],bioimagetools..validate,points,points.discrete,mask,img.classes,class1,silent)
  if(!parallel)valid<-lapply(1:dim(points.discrete)[1],bioimagetools..validate,points,points.discrete,mask,img.classes,class1,silent)
  valid<-unlist(valid)
  valid<-matrix(valid,nrow=3)
  valid<-t(valid)
  colnames(valid) <- c("x", "y", "z")

  if(!silent)cat("\b\\")
  x <- rep(1:X, Y * Z)
  y <- rep(rep(1:Y, each = X), Z)
  z <- rep(1:Z, each = X * Y)
  which <- (mask == 1)
  nucleus <- data.frame(x = x[which], y = y[which], z = z[which], 
                        class = (img.classes)[which])
  if (is.null(class2)) 
    chromatin <- nucleus[nucleus$class != class1, 1:3]
  if (!is.null(class2)) 
    chromatin <- nucleus[nucleus$class == class2, 1:3]
  if(!silent)cat("\b|")
  chromatin$x <- (chromatin$x - 1)/X * x.microns
  chromatin$y <- (chromatin$y - 1)/Y * y.microns
  chromatin$z <- (chromatin$z - 1)/Z * z.microns

  if(parallel)abstand1 <- mclapply(1:dim(valid)[1], bioimagetools..find.min.distance2, valid, 
                     chromatin, c(x.microns/X, y.microns/Y, z.microns/Z), silent)
  if(!parallel)abstand1 <- lapply(1:dim(valid)[1], bioimagetools..find.min.distance2, valid, 
                                            chromatin, c(x.microns/X, y.microns/Y, z.microns/Z), silent)
  abstand1<-unlist(abstand1)
  
  
  if(!silent)cat("\b/")
  if (is.null(class2)) {
    if(parallel)valid<-mclapply(1:dim(points.discrete)[1],bioimagetools..validate.uneq,points,points.discrete,mask,img.classes,class1,silent)
    if(!parallel)valid<-lapply(1:dim(points.discrete)[1],bioimagetools..validate.uneq,points,points.discrete,mask,img.classes,class1,silent)
  }
  else {
    if(parallel)valid<-mclapply(1:dim(points.discrete)[1],bioimagetools..validate,points,points.discrete,mask,img.classes,class2,silent)
    if(!parallel)valid<-lapply(1:dim(points.discrete)[1],bioimagetools..validate,points,points.discrete,mask,img.classes,class2,silent)
  }
  valid<-unlist(valid)
  valid<-matrix(valid,nrow=3)
  valid<-t(valid)
  colnames(valid) <- c("x", "y", "z")
  
  if(!silent)cat("\b-")
  chromatin <- nucleus[nucleus$class == class1, 1:3]
  chromatin$x <- (chromatin$x - 1)/X * x.microns
  chromatin$y <- (chromatin$y - 1)/Y * y.microns
  chromatin$z <- (chromatin$z - 1)/Z * z.microns
  if(!silent)cat("\b//")
  if(parallel)abstand2 <- mclapply(1:dim(valid)[1], bioimagetools..find.min.distance2, valid, 
                                            chromatin, c(x.microns/X, y.microns/Y, z.microns/Z), silent)
  if(!parallel)abstand2 <- lapply(1:dim(valid)[1], bioimagetools..find.min.distance2, valid, 
                                           chromatin, c(x.microns/X, y.microns/Y, z.microns/Z), silent)
  abstand2<-unlist(abstand2)
  
  abstand <- c(abstand1, -abstand2)
  if(!silent)cat("\b-")
  if (hist) {
    if (!is.null(file)) 
      png(file)
    temp <- graphics::hist(abstand[abstand < xlim[2] & abstand > xlim[1]], 
                 breaks = seq(xlim[1], xlim[2], length = n), main = main, 
                 xlab = xlab)
    if (stats) 
      graphics::text(xlim[2] * 0.85, 0.85 * max(temp$counts), paste("mean: ", 
                                                          round(mean(1000 * abstand), 1), "\n median: ", 
                                                          round(median(1000 * abstand), 1), "\n st.dev.: ", 
                                                          round(sd(1000 * abstand), 2)))
    graphics::box()
    if (!is.null(file)) 
      dev.off()
  }
  if(!silent)cat("\b")
  return(abstand)
}

bioimagetools..find.min.distance2<-function(i,points,voxels,microns,silent)
{
  point<-points[i,]
  x<-y<-z<-rep(0,dim(voxels)[1])
  which<-(point[1]<voxels$x)
  if(!silent)if(sample(100,1)<5)cat(".")
  x[which]<-point[1]-voxels$x[which]
  which<-(point[1]>(voxels$x+microns[1]))
  x[which]<-voxels$x[which]+microns[1]-point[1]
  which<-(point[2]<voxels$y)
  y[which]<-point[2]-voxels$y[which]
  which<-(point[2]>(voxels$y+microns[2]))
  y[which]<-voxels$y[which]+microns[2]-point[2]
  which<-(point[3]<voxels$z)
  z[which]<-point[3]-voxels$z[which]
  which<-(point[3]>(voxels$z+microns[3]))
  z[which]<-voxels$z[which]+microns[3]-point[3]
  dist<-sqrt(x^2+y^2+z^2)
  found<-which(dist==min(dist))
  return(dist[found][1])
}
bioimagetools..find.min.distance<-function(point,voxels,microns,silent)
{
  x<-y<-z<-rep(0,dim(voxels)[1])
  which<-(point[1]<voxels$x)
  if(!silent)if(sample(100,1)<5)cat(".")
  x[which]<-point[1]-voxels$x[which]
  which<-(point[1]>(voxels$x+microns[1]))
  x[which]<-voxels$x[which]+microns[1]-point[1]
  which<-(point[2]<voxels$y)
  y[which]<-point[2]-voxels$y[which]
  which<-(point[2]>(voxels$y+microns[2]))
  y[which]<-voxels$y[which]+microns[2]-point[2]
  which<-(point[3]<voxels$z)
  z[which]<-point[3]-voxels$z[which]
  which<-(point[3]>(voxels$z+microns[3]))
  z[which]<-voxels$z[which]+microns[3]-point[3]
  dist<-sqrt(x^2+y^2+z^2)
  found<-which(dist==min(dist))
  return(dist[found][1])
}

bioimagetools..validate<-function(i,points,pd,mask,ic,class,silent){
  if (mask[pd[i,1], pd[i, 2], pd[i, 3]]) 
    if (ic[pd[i, 1], pd[i, 2], pd[i, 3]] == class) 
      return(points[i,])
}

bioimagetools..validate.uneq<-function(i,points,pd,mask,ic,class,silent){
  if (mask[pd[i,1], pd[i, 2], pd[i, 3]]) 
    if (ic[pd[i, 1], pd[i, 2], pd[i, 3]] != class) 
      return(points[i,])
}


