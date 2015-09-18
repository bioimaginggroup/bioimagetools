#' Read tif stacks
#'
#' @param file Name of the file to read from.
#' @param native determines the image representation - if FALSE (the default) then the result is an array, if TRUE then the result is a native raster representation (suitable for plotting).
#' @param as.is attempt to return original values without re-scaling where possible
#' @param channels number of channels
#' @return 3d or 4d array
#' @export
#' @import tiff
readTIF<-function(file=file.choose(),native=FALSE,as.is=FALSE,channels=NULL)
{
  require(tiff)
  li<-readTIFF(file,all=TRUE,info=TRUE,as.is=as.is,native=native)
  Z<-length(li)
  img<-array(0,c(dim(li[[1]]),Z))
  if(length(dim(li[[1]]))==2)for (i in 1:Z)img[,,i]<-li[[i]]
  if(length(dim(li[[1]]))==3)for (i in 1:Z)img[,,,i]<-li[[i]]
  #storage.mode(img)<-"integer"
  temp<-attributes(li[[1]])
  tmp<-gregexpr("\n",temp$description)
  if(length(tmp)>0)if (tmp[[1]][1]!=-1)
  {
    temp2<-regmatches(temp$description,tmp,invert=TRUE)[[1]]
    temp3<-temp4<-c()
    for (i in temp2)
    {
      j<-gregexpr("=",i)[[1]]
      j<-regmatches(i,j,invert=TRUE)[[1]]
      temp3<-c(temp3,j[1])
      temp4<-c(temp4,j[2])
    }
    names(temp4)<-temp3
    temp<-c(temp,temp4)
  }
  temp<-temp[!(names(temp)=="")]
  K<-as.integer(temp$channels)
  if (!is.null(channels)){K<-channels}
  if(length(K)==0)K<-1
  if ((K>1)&(length(dim(img))<4))
  {
    img<-array(img,c(dim(li[[1]])[1:2],K,Z/K))
    #storage.mode(img)<-"integer"
    #for (i in 1:K)img0[,,i,]<-img[,,seq(i,Z,by=K)]
    #img<-img0
  }
  #if(min(img)<0){require(bitops);img<-array(bitFlip(img,bitWidth=temp$bits.per.sample),dim(img))}
  #img<-img/(2^(temp$bits.per.sample))
  if (!as.is)
  {
    if (min(img)<0)img=img-min(img)
    if (max(img)>1)img<-img/max(img)
  }
  temp$dim<-dim(img)
  temp$file<-file
  attributes(img)<-temp
  return(img)
}