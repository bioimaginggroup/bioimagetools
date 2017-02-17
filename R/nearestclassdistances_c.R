#' Find all distances to next neighbour of all classes
#'
#' @param img Image array of classes
#' @param voxelsize Real size of voxels in microns.
#' @param size Real size of image in microns. Either size or voxelsize must be given.
#' @param classes Number of classes
#' @param maxdist Maximum distance to consider
#' @param silent Remain silent?
#' @param cores Number of cores available for parallel computing
#'
#' @return array with distances
#' @export
#' @useDynLib bioimagetools

nearestClassDistances<-function(img, voxelsize=NULL, size=NULL, classes=7, maxdist=NULL, silent=FALSE, cores=1)
{
  if (is.null(size)&is.null(voxelsize)){stop("Either size or voxelsize is required")}
  if(is.null(voxelsize))voxelsize<-size/dim(img)
  img[is.na(img)]<-0
  alist=vector(length=classes,mode = "list")
  tt<-table.n(img,m=classes)
  zscale<-mean(voxelsize[1:2])/voxelsize[3]
  
  ww.list <- parallel::mclapply(1:classes,ncdWorker1,img,tt)
  
  worker.list<-vector(length=classes*classes*(classes+1)/2,mode = "list")
  
  counter<-0
  for (i in 1:classes)
    if (tt[i]>0)
    for (j in 1:classes)
      if (tt[j]>0)
      {
      ij<-abs(i-j)+1
      breaks<-c(floor(tt[i]/ij)*(0:(ij-1)),tt[i])
      for (k in 1:ij)
      {
        counter<-counter+1
        worker.list[[counter]]<-list("id"=c(i,j),"ww"=ww.list[[i]][(breaks[k]+1):breaks[k+1],])
      }
    }
  
  if (is.null(maxdist))maxdist=max(dim(img)*voxelsize)

  alist <- parallel::mclapply(worker.list, ncdWorker2, img, zscale, maxdist, mean(voxelsize[1:2]), mc.cores=cores, mc.preschedule=FALSE)
  
  alist2 <- vector(length=classes,mode = "list")

  for (i in 1:classes)
    alist2[[i]] <- vector(length=classes,mode = "list")
  
  for (i in 1:length(worker.list))
    if (!is.null(worker.list[[i]]))
        {
        counter<-worker.list[[i]][["id"]]
        alist2[[counter[1]]][[counter[2]]]<-c(alist2[[counter[1]]][[counter[2]]],alist[[i]])
        }

  return(alist2)
}

ncdWorker2<-function(worker, img, zscale, maxdist, voxelsize)
{
  if (is.null(worker))return(NULL)
  c1<-worker[["id"]][1]
  c2<-worker[["id"]][2]
  ww<-worker[["ww"]]
  n<-dim(ww)[1]
  dist<-vector(mode="numeric",length=n)
  return(nearestClassDistancesClass(c2,dist,ww,zscale, maxdist/voxelsize, n,img)*voxelsize)
}

ncdWorker1<-function(j,img,tt)
{
  if (tt[j]==0)return(NULL)
  return(as.matrix(which(img==j,arr.ind = TRUE)))
}

nearestClassDistancesClass<-function(j,dist,ww,zscale,maxdist,n,img)
{
   temp=.C("nearestClassDistancesClass",
   as.double(dist), as.integer(t(ww)), as.integer(c(0,0,0)),
   as.double(c(zscale,maxdist)), as.integer(j), as.integer(n),
   as.integer(img), as.integer(dim(img)),
   package="bioimagetools")
   return(temp[[1]])
}

