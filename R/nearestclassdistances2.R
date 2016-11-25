distanceonvoxelgrid<-function(n.max=10,zscale=3)
{
  result<-vector(length=n.max,mode="list")
  for (x in 0:n.max)
    for (y in 0:x)
      for (z in 0:(floor(n.max/zscale)))
      {
        d=round(sqrt(x^2+y^2+(z*zscale)^2),0)
        if (d!=0&d<=n.max)
        {
          result[[d]]<-cbind(result[[d]],c(x,y,z))
          if (x!=y)result[[d]]<-cbind(result[[d]],c(y,x,z))
          if (x!=0)result[[d]]<-cbind(result[[d]],c(-x,y,z))
          if (x!=0&x!=y)result[[d]]<-cbind(result[[d]],c(y,-x,z))
          if (y!=0)result[[d]]<-cbind(result[[d]],c(x,-y,z))
          if (y!=0&x!=y)result[[d]]<-cbind(result[[d]],c(-y,x,z))
          if (x!=0&y!=0)result[[d]]<-cbind(result[[d]],c(-x,-y,z))
          if (x!=0&y!=0&x!=y)result[[d]]<-cbind(result[[d]],c(-y,-x,z))
          if (z!=0)result[[d]]<-cbind(result[[d]],c(x,y,-z))
          if (x!=y&z!=0)result[[d]]<-cbind(result[[d]],c(y,x,-z))
          if (x!=0&z!=0)result[[d]]<-cbind(result[[d]],c(-x,y,-z))
          if (x!=0&&x!=y&z!=0)result[[d]]<-cbind(result[[d]],c(y,-x,-z))
          if (y!=0&z!=0)result[[d]]<-cbind(result[[d]],c(x,-y,-z))
          if (x!=y&y!=0&z!=0)result[[d]]<-cbind(result[[d]],c(-y,x,-z))
          if (x!=0&y!=0&z!=0)result[[d]]<-cbind(result[[d]],c(-x,-y,-z))
          if (x!=0&y!=0&z!=0&x!=y)result[[d]]<-cbind(result[[d]],c(-y,-x,-z))
        }
        
      }
  return(result)
}

find.class.in.voxel.grid<-function(img, zero, classes, voxel, voxelsize)
{
  for (i in 1:3)voxel[i,]<-voxel[i,]+zero[i]
  for (i in 1:3)voxel<-voxel[,voxel[i,]>0]
  for (i in 1:3)voxel<-voxel[,voxel[i,]<=(dim(img)[i])]
  result<-vector(mode="list",length=max(classes))
  for (i in 1:dim(voxel)[2])
    if (!is.na(img[voxel[1,i],voxel[2,i],voxel[3,i]]))
    if (img[voxel[1,i],voxel[2,i],voxel[3,i]]!=0)
    if (any(img[voxel[1,i],voxel[2,i],voxel[3,i]]==classes))
    {
    d=sqrt(sum(((voxel[,i]-zero)*voxelsize)^2))
    result[[img[voxel[1,i],voxel[2,i],voxel[3,i]]]]<-c(result[[img[voxel[1,i],voxel[2,i],voxel[3,i]]]],d)
    }
  return(result)
}

find.all.classes.in.voxelgrid<-function(zero,img, classes, voxelgrid, voxelsize)
{
  found<-rep(TRUE,max(classes))
  found[classes]<-FALSE
  results<-rep(NA,length(classes))
  zone<-1
  zonemax<-length(voxelgrid)
  while(any(!found)&zone<zonemax)
  {
    temp<-find.class.in.voxel.grid(img, zero, which(!found), voxelgrid[[zone]], voxelsize)
    m<-max(which(!found))
    for (i in 1:m)
      if(!is.null(temp[[i]]))
      {
          results[which(classes==i)]<-min(temp[[i]])
          found[i]<-TRUE
      }
    zone<-zone+1
  }
  return(results)
}
  
#' Find all distances to next neighbour of all classes
#'
#' @param img Image array of classes
#' @param voxelsize Vector of length three. Size of voxel in X-/Y-/Z-direction
#' @param classes Number of classes
#' @param n.max Maximum distance in voxel
#' @param cores Number of cores available for parallel computing
#'
#' @return array with distances
#' @export
#' 
nearestClassDistances2<-function(img,voxelsize,classes=7,n.max=10, cores=1)
{
  zscale<-mean(voxelsize[1:2])/voxelsize[3]
  voxelgrid<-distanceonvoxelgrid(n.max=n.max,zscale=zscale)

  result=vector(mode="list",length=classes)
  
  ii<-1:classes
  result<-lapply(ii, nearestClassDistances.oneclass, img,voxelsize,classes, zscale, voxelgrid, n.max=10, cores=cores)
  return(result)
}

nearestClassDistances.oneclass<-function(i, img,voxelsize,classes, zscale, voxelgrid, n.max=10, cores=1)
{

    w=which(img==i,arr.ind=TRUE)
    w<-as.data.frame(t(w))
    w<-as.list(w)
    if(cores>1)w<-parallel::mclapply(w,find.all.classes.in.voxelgrid,img, 1:classes, voxelgrid, voxelsize,mc.cores=cores)
    if(cores==1)w<-lapply(w,find.all.classes.in.voxelgrid,img, 1:classes, voxelgrid, voxelsize)
    w<-array(unlist(w),c(3,length(w)))
    result<-as.list(as.data.frame(t(w)))
  return(result)
}