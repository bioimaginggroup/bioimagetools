#' Extract spots using adaptive thresholding and minimum spot intensitiy
#'
#' @param file name of file, this file needs to be in red/, green/, class7/ and dapimask/
#' @param folder name of root folder
#' @param thresh.offset thresholf value for adaptive thresholding
#' @param min.sum.intensity threshold for spot sum intensity
#' @param full.voxels if TRUE returen 0/1, if FALSE returns spots with intensitites
#' @param output output folder name, output will be an rgb stack with DAPI class and red/green spots
#'
#' @return NULL
#' @export
#' @import bioimagetools 
#'
extract.spots<-function(file, folder="./", thresh.offset=0.1, min.sum.intensity=2, full.voxels=FALSE, output="markers")
{
  oldwd=getwd()
  setwd(folder)
  mask<-readTIF(paste0("dapimask/",file))

    red<-readTIF(paste0("red/",file))
  red[mask==0]<-0
  green<-readTIF(paste0("green/",file))
  green[mask==0]<-0
  
  red.spots<-EBImage::thresh(red,offset=thresh.offset)
  green.spots<-EBImage::thresh(green,offset=thresh.offset)
  
  red.s<-bwlabel3d(red.spots)
  green.s<-bwlabel3d(green.spots)
  
  red.c<-cmoments3d(red.s,red)
  green.c<-cmoments3d(green.s,green)
  
  red.c<-red.c[red.c[,5]>min.sum.intensity,]
  green.c<-green.c[green.c[,5]>min.sum.intensity,]
  
  if (is.null(dim(red.c)))red.c<-rbind(array(red.c,c(1,length(red.c))),c(0,0,0))
  if (is.null(dim(green.c)))green.c<-rbind(array(green.c,c(1,length(green.c))),c(100,100,10))
  
  labels.red<-red.c[,1]
  labels.green<-green.c[,1]
  
  new.red<-array(0,dim(red))
  new.green<-array(0,dim(red))
  new.blue<-array(0,dim(red))
  if (!full.voxels)  for (i in 1:length(labels.red))new.red[red.s==labels.red[i]]<-red[red.s==labels.red[i]]
  if (!full.voxels)  for (i in 1:length(labels.green))new.green[green.s==labels.green[i]]<-green[green.s==labels.green[i]]
  if (full.voxels)   for (i in 1:length(labels.red))new.red[red.s==labels.red[i]]<-1
  if (full.voxels)  for (i in 1:length(labels.green))new.green[green.s==labels.green[i]]<-1
  
  blue<-readTIF(paste("class7/",file,sep=""))
  new.blue[mask==1]<-blue[mask==1]
  
  all<-array(0,c(dim(new.blue)[1:2],3,dim(new.blue)[3]))
  all[,,1,]<-new.red
  all[,,2,]<-new.green
  all[,,3,]<-new.blue
  print(file)
  writeTIF(all,file=paste0(output,"/",file),bps=16L,reduce=TRUE)
  setwd(oldwd)
}