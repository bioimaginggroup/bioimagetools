distance2border<-function (points, img.classes, x.microns, y.microns, z.microns, 
                           class1, class2 = NULL, mask = array(TRUE, dim(img.classes)), voxel=FALSE,
                           hist = FALSE, main = "Minimal distance to border", xlab = "Distance in Microns", 
                           xlim = c(-0.3, 0.3), n = 20, stats = TRUE, file = NULL) 
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
  
  
  cat("-")
 
  if(require(parallel))valid<-mclapply(1:dim(points.discrete)[1],bioimagetools..validate,points,points.discrete,mask,img.classes,class1)
  if(!require(parallel))valid<-lapply(1:dim(points.discrete)[1],bioimagetools..validate,points,points.discrete,mask,img.classes,class1)
  valid<-unlist(valid)
  valid<-matrix(valid,nrow=3)
  valid<-t(valid)
  colnames(valid) <- c("x", "y", "z")

  cat("\b\\")
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
  cat("\b|")
  chromatin$x <- (chromatin$x - 1)/X * x.microns
  chromatin$y <- (chromatin$y - 1)/Y * y.microns
  chromatin$z <- (chromatin$z - 1)/Z * z.microns

  if(require(parallel))abstand1 <- mclapply(1:dim(valid)[1], bioimagetools..find.min.distance2, valid, 
                     chromatin, c(x.microns/X, y.microns/Y, z.microns/Z))
  if(!require(parallel))abstand1 <- lapply(1:dim(valid)[1], bioimagetools..find.min.distance2, valid, 
                                            chromatin, c(x.microns/X, y.microns/Y, z.microns/Z))
  abstand1<-unlist(abstand1)
  
  
  cat("\b/")
  if (is.null(class2)) {
    if(require(parallel))valid<-mclapply(1:dim(points.discrete)[1],bioimagetools..validate.uneq,points,points.discrete,mask,img.classes,class1)
    if(!require(parallel))valid<-lapply(1:dim(points.discrete)[1],bioimagetools..validate.uneq,points,points.discrete,mask,img.classes,class1)
  }
  else {
    if(require(parallel))valid<-mclapply(1:dim(points.discrete)[1],bioimagetools..validate,points,points.discrete,mask,img.classes,class2)
    if(!require(parallel))valid<-lapply(1:dim(points.discrete)[1],bioimagetools..validate,points,points.discrete,mask,img.classes,class2)
  }
  valid<-unlist(valid)
  valid<-matrix(valid,nrow=3)
  valid<-t(valid)
  colnames(valid) <- c("x", "y", "z")
  
  cat("\b-")
  chromatin <- nucleus[nucleus$class == class1, 1:3]
  chromatin$x <- (chromatin$x - 1)/X * x.microns
  chromatin$y <- (chromatin$y - 1)/Y * y.microns
  chromatin$z <- (chromatin$z - 1)/Z * z.microns
  cat("\b//")
  if(require(parallel))abstand2 <- mclapply(1:dim(valid)[1], bioimagetools..find.min.distance2, valid, 
                                            chromatin, c(x.microns/X, y.microns/Y, z.microns/Z))
  if(!require(parallel))abstand2 <- lapply(1:dim(valid)[1], bioimagetools..find.min.distance2, valid, 
                                           chromatin, c(x.microns/X, y.microns/Y, z.microns/Z))
  abstand2<-unlist(abstand2)
  
  abstand <- c(abstand1, -abstand2)
  cat("\b-")
  if (hist) {
    if (!is.null(file)) 
      png(file)
    temp <- hist(abstand[abstand < xlim[2] & abstand > xlim[1]], 
                 breaks = seq(xlim[1], xlim[2], length = n), main = main, 
                 xlab = xlab)
    if (stats) 
      text(xlim[2] * 0.85, 0.85 * max(temp$counts), paste("mean: ", 
                                                          round(mean(1000 * abstand), 1), "\n median: ", 
                                                          round(median(1000 * abstand), 1), "\n st.dev.: ", 
                                                          round(sd(1000 * abstand), 2)))
    box()
    if (!is.null(file)) 
      dev.off()
  }
  cat("\b")
  return(abstand)
}

bioimagetools..find.min.distance2<-function(i,points,voxels,microns)
{
  point<-points[i,]
  x<-y<-z<-rep(0,dim(voxels)[1])
  which<-(point[1]<voxels$x)
  if(sample(100,1)<5)cat(".")
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
bioimagetools..find.min.distance<-function(point,voxels,microns)
{
  x<-y<-z<-rep(0,dim(voxels)[1])
  which<-(point[1]<voxels$x)
  if(sample(100,1)<5)cat(".")
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

bioimagetools..validate<-function(i,points,pd,mask,ic,class){
  if (mask[pd[i,1], pd[i, 2], pd[i, 3]]) 
    if (ic[pd[i, 1], pd[i, 2], pd[i, 3]] == class) 
      return(points[i,])
}

bioimagetools..validate.uneq<-function(i,points,pd,mask,ic,class){
  if (mask[pd[i,1], pd[i, 2], pd[i, 3]]) 
    if (ic[pd[i, 1], pd[i, 2], pd[i, 3]] != class) 
      return(points[i,])
}


