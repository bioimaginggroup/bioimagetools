#' Find all distances to next neighbour of all classes
#'
#' @param img Image array of classes
#' @param voxelsize Vector of length three. Size of voxel in X-/Y-/Z-direction
#' @param classes Number of classes
#' @param silent Remain silent?
#' @param cores Number of cores available for parallel computing
#'
#' @return array with distances
#' @export
#' @useDynLib bioimagetools

nearestClassDistances3<-function(img,voxelsize,classes=7,silent=FALSE,cores=1)
{
  img[is.na(img)]<-0
  alist=vector(length=classes,mode = "list")
  tt<-table.n(img,m=classes)
  zscale<-mean(voxelsize[1:2])/voxelsize[3]
  for (class in 1:classes)
  {
    alist[[class]]<-vector(length=classes,mode = "list")
    if (tt[class]>0)
    {
      ww<-as.matrix(which(img==class,arr.ind = TRUE))
      n<-dim(ww)[1]
      dist<-vector(mode="numeric",length=n)
      if(cores>1)alist[[class]]<-mclapply(1:classes,nearestClassDistancesClass,dist,ww,zscale,n,img, mc.cores=cores)
      if(cores==1)alist[[class]]<-lapply(1:classes,nearestClassDistancesClass,dist,ww,zscale,n,img)
    }
  }
  return(alist)
}

nearestClassDistancesClass<-function(j,dist,ww,zscale,n,img)
{
   .C("nearestClassDistancesClass",
   as.double(dist), as.integer(t(ww)), as.integer(c(0,0,0)),
   as.double(zscale), as.integer(j), as.integer(n),
   as.integer(img), as.integer(dim(img)),
   package="bioimagetools")
   return(dist)
}

