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
#' 

nearestClassDistances<-function(img,voxelsize,classes=7,silent=FALSE,cores=1,algorithm="new")
{
  img[is.na(img)]<-0
  alist=vector(length=classes,mode = "list")
  tt<-table.n(img,m=classes)
  zscale<-mean(voxelsize[1:2])/voxelsize[3]
  for (class in 1:classes)
  {
    if(!silent)cat(paste0("\n",class,":"))
    alist[[class]]<-vector(length=classes,mode = "list")
    if (tt[class]>0)
    {
      ww<-as.matrix(which(img==class,arr.ind = TRUE))
      n<-dim(ww)[1]
      dist<-vector(mode="numeric",length=n)
      if(cores>1)alist[[class]]<-mclapply(1:classes,nearestClassDistancesClass,dist,ww,zscale,n,img)
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
   as.integer(img), as.integer(dim(img)))
   return(dist)
}

#' Title Find distance to next neighbour of a specific class
#'
#' @param coord coordinate of relevant voxel
#' @param img image array of classes
#' @param class class to find
#' @param voxelsize vector of length three. size of voxel in X-/Y-/Z-direction
#'
#' @return distance to nearest voxel of class "class"
#' @export
#' 
nearestClassDistance<-function(coord,img,class,voxelsize, step=NULL)
{
  coord<-coord[[1]]
  zscale<-mean(voxelsize[1:2])/voxelsize[3]
  d<-.C("nearestClassDistances",
                  c(as.integer(img),as.integer(10^6)),
                          as.integer(coord), as.integer(dim(img)),
                          as.integer(c(zscale,class)))
  return(d)
}
