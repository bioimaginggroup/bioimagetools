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
  
  ww.list <- parallel::mclapply(1:classes,ncdWorker1,img,tt)
  
  worker.list<-vector(length=classes*classes,mode = "list")
  
  counter<-0
  for (i in 1:classes)
    for (j in 1:classes)
    {
      counter<-counter+1
      worker.list[[counter]]<-c(i,j)
    }

  alist <- parallel::mclapply(worker.list, ncdWorker2, img, tt, ww.list, zscale, mean(voxelsize[1:2]), mc.cores=cores, mc.preschedule=FALSE)
  
  alist2 <- vector(length=classes,mode = "list")

  for (i in 1:classes)
    alist2[[i]] <- vector(length=classes,mode = "list")
  
    for (i in 1:(classes*classes))
    {
      counter<-worker.list[[i]]
      alist2[[counter[1]]][[counter[2]]]<-alist[[i]]
    }

  return(alist2)
}

ncdWorker2<-function(worker.list, img, tt, ww.list, zscale, voxelsize)
{
  c1<-worker.list[1]
  c2<-worker.list[2]
  if (tt[c1]==0)return(NULL)
  ww<-ww.list[[c1]]
  n<-dim(ww)[1]
  dist<-vector(mode="numeric",length=n)
  return(nearestClassDistancesClass(c2,dist,ww,zscale,n,img)*voxelsize)
}

ncdWorker1<-function(j,img,tt)
{
  if (tt[j]==0)return(NULL)
  return(as.matrix(which(img==j,arr.ind = TRUE)))
}

nearestClassDistancesClass<-function(j,dist,ww,zscale,n,img)
{
   temp=.C("nearestClassDistancesClass",
   as.double(dist), as.integer(t(ww)), as.integer(c(0,0,0)),
   as.double(zscale), as.integer(j), as.integer(n),
   as.integer(img), as.integer(dim(img)),
   package="bioimagetools")
   return(temp[[1]])
}

