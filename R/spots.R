#' Find spots based on threshold and minimum total intensity
#'
#' @param img image array
#' @param mask mask
#' @param thresh.offset threshold for minimum voxel intensity
#' @param min.sum.intensity threshold for minimum total spot intensity
#' @param zero if NA, background is set to NA, if 0,background is set to 0
#' @param max.spots find max.spots spots with highest total intensity
#' @param return "mask" returns binarized mask, "intensity" returns intensity for spots, zero or NA otherwise
#'        "label" return labeled (numbered) spots 
#'
#' @return array
#' @export
#'
spots<-function(img, mask, thresh.offset=0.1, min.sum.intensity=0, zero=NA, max.spots=NULL, return="full")
{
  img[mask==0]<-0
  spots<-thresh(img,offset=thresh.offset)

  s<-bwlabel3d(spots)
  cm<-cmoments3d(s,img)
  cm<-cm[cm[,5]>min.sum.intensity,]
  labels=cm[,1]
  
  if(!is.null(max.spots))
  {
    if(length(labels)>max.spots)labels<-cm[rev(order(cm[,5]))[1:max.spots]]
  }
  
  new<-array(NA,dim(img))
  if (return=="intensity"|return=="i") for (i in 1:length(labels))new[s==labels[i]]<-img[s==labels[i]]
  if (return=="mask"|return=="mask") for (i in 1:length(labels))new[s==labels[i]]<-1
  if (return=="label"|return=="l") for (i in 1:length(labels))new[s==labels[i]]<-i
  
  if (is.na(zero))new[new==0]=NA
  if (!is.na(zero))if(zero==0)new[is.na(new)]=0
  pritn("new")
  return(new)
}