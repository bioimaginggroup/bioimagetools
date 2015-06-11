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

