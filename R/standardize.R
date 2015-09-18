#' Standardize images
#'
#' @description Standardizes images in order to compare different images. Mean of standardized image is 0.5, standard deviation is sd.
#' @param img is a 2d/3d arrary representing an image.
#' @param mask a mask.
#' @param N number of classes.
#' @param sd standard deviation.
#'
#' @return Multi-dimensional array of standardized image.
#' @export
#'
#' @examples #simuliere Daten zum Testen
#' test2<-runif(128*128,0,1)
#' test2<-sort(test2)
#' test2<-array(test2,c(128,128))
#' img(test2)
#' # Standardisiere test2 in 32 Klassen
#' std<-standardize(test2,N=32,sd=4)

standardize<-function(img,mask=array(TRUE,dim(img)),N=32,sd=1/6)
{
  if (length(dim(img))!=length(dim(mask)))stop("Dimensions of img and mask do not match 1.")
  if (!(all.equal(dim(img),dim(mask))))stop("Dimensions of img and mask do not match 2.")

int<-img[mask==1]
mb<-mean(int,na.rm=TRUE)
sdb<-sd(int,na.rm=TRUE)
img<-img-mb
img<-img/sdb
img<-img/sd # Stauchung
img<-img*N
img<-img+(N/2)+.5
img<-round(img)
img[img<1]<-0
img[img>N]<-N+1
img[mask!=1]<-0
return(img)
}

