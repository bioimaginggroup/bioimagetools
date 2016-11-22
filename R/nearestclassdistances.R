#' Find all distances to next neighbour of all classes
#'
#' @param img Image array of classes
#' @param voxelsize Vector of length three. Real size of voxel in microns.
#' @param classes Number of classes
#' @param silent Remain silent?
#' @param cores Number of cores available for parallel computing
#'
#' @return array with distances
#' @export
#' 

nearestClassDistances<-function(img,voxelsize,classes=7,silent=FALSE,cores=1)
{
  img[is.na(img)]<-0
  alist=vector(length=classes,mode = "list")
  tt<-table.n(img,m=classes)
  for (class in 1:classes)
  {
    if(!silent)cat(paste0("\n",class,":"))
    alist[[class]]<-vector(length=classes,mode = "list")
    if (tt[class]>0)
    {
      ww<-as.matrix(which(img==class,arr.ind = TRUE))
      www<-apply(ww,1,function(x)return(list(x)))
      for (j in ((1:classes)))
      {
        if(!silent)cat(paste0("_",j))
        if(tt[j]>0)
        {
          if(cores>1)alist[[class]][[j]]<-unlist(parallel::mclapply(www,nearestClassDistance,img,j,voxelsize,mc.cores=cores),use.names = FALSE)
          if(cores==1)alist[[class]][[j]]<-unlist(lapply(www,nearestClassDistance,img,j,voxelsize),use.names = FALSE)
        }
      }
    }
  }
  return(alist)
}

#' Title Find distance to next neighbour of a specific class
#'
#' @param coord coordinate of relevant voxel
#' @param img image array of classes
#' @param class class to find
#' @param voxelsize vector of length three. size of voxel in X-/Y-/Z-direction
#' @param step size of window to start with
#' @return distance to nearest voxel of class "class"
#' @export
#' 
nearestClassDistance<-function(coord,img,class,voxelsize,step=0)
{
  coord<-coord[[1]]
  dims<-dim(img)
  step=step+1
  zscale<-mean(voxelsize[1:2])/voxelsize[3]
  stepz=floor(step*zscale)
  xx<-yy<-step+1
  zz<-stepz+1
  x0<-coord[1]-step
  x1<-min(coord[1]+step,dims[1])
  y0<-coord[2]-step
  y1<-min(coord[2]+step,dims[2])
  z0<-coord[3]-stepz
  z1<-min(coord[3]+stepz,dims[3])
  if (x0<1){xx=xx+x0-1;x0=1}
  if (y0<1){yy=yy+y0-1;y0=1}
  if (z0<1){zz=zz+z0-1;z0=1}
  y<-y0:y1
  z<-z0:z1
  x<-x0:x1
  part=img[x,y,z]
  if (!any(part==class,na.rm=TRUE))
  {
    return(nearestClassDistance(list(coord),img,class,voxelsize,step))
  }
  else{
    wk<-which(part==class,arr.ind = TRUE)
    if (dim(wk)[2]==2)wk<-cbind(wk,rep(1,dim(wk)[1]))
    dist<-apply(wk,1,function(a,b)return(sqrt(sum(((a-b)*voxelsize)^2))),b=c(xx,yy,zz))
    dist<-dist[dist!=0]
    if (length(dist)==0)return(nearestClassDistance(list(coord),img,class,voxelsize,step))
    return(min(dist))
  }
}


