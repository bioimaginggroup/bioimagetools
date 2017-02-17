#' Standardize images
#'
#' @description Standardizes images in order to compare different images. Mean of standardized image is 0.5, standard deviation is sd.
#' @param img is a 2d/3d arrary representing an image.
#' @param mask a mask.
#' @param log Logical. Transform to log scale before standardization?
#' @param N number of classes.
#' @param sd standard deviation.
#'
#' @return Multi-dimensional array of standardized image.
#' @export
#' @examples #simuliere Daten zum Testen
#' test2<-runif(128*128,0,1)
#' test2<-sort(test2)
#' test2<-array(test2,c(128,128))
#' img(test2)
#' # Standardisiere test2 in 32 Klassen
#' std<-standardize(test2,N=32,sd=4)

standardize<-function(img,mask=array(TRUE,dim(img)),log=FALSE,N=32,sd=1/6)
{
  if (length(dim(img))!=length(dim(mask)))stop("Dimensions of img and mask do not match 1.")
  if (!(all.equal(dim(img),dim(mask))))stop("Dimensions of img and mask do not match 2.")
if (log)img<-log(img)
img[mask==0]<-NA
int<-img[mask==1]
if(log)int<-int[int!=-Inf]
mb<-mean(int,na.rm=TRUE)
sdb<-sd(int,na.rm=TRUE)
img<-img-mb
img<-img/sdb
img<-img/sd # Stauchung
if(log)img<-exp(img)
img<-img*N
img<-img+(N/2)+.5
img<-round(img)
img[img<1]<-0
img[img>N]<-N
img[mask!=1]<-0
img[is.na(img)]<-0
img<-img/N
return(img)
}

