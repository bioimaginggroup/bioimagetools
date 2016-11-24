#' Segmentation of 3D images using EM algorithms
#' @description egmentation of 3D images using EM algorithms
#'
#' @param img is a 3d arrary representing an image.
#' @param nclust is the number of clusters/classes to be segmented.
#' @param beta is a matrix of size nclust x nclust, representing the prior weight of classes neighbouring each other.
#' @param z.scale ratio of voxel dimension in x/y direction and z direction. Will be multiplied on beta for neighbouring voxel in z direction.
#' @param method only "cem" classification EM algorithm implemented.
#' @param varfixed is a logical variable. If TRUE, variacne is equal in each class.
#' @param maxit is the maximum number of iterations.
#' @param mask is a logical array, representing the voxels to be used in the segmentation.
#' @param priormu is a vector with mean of the normal prior of the expected values of all classes. Default is NA, which represents no prior assumption.
#' @param priormusd is a vector with standard deviations of the normal prior of the expected values of all classes.
#' @param min.eps stop criterion. Minimal change in sum of squared estimate of mean in order to stop.
#' @param inforce.nclust if TRUE enforces number of clusters to be nclust. Otherwise classes might be removed during algorithm.
#' @param start ?
#' @param silent if TRUE, function remains silent during running time 
#'
#' @return A list with "class": 3d array of class per voxel; "mu" estimated means; "sigma": estimated standard deviations. 
#' @export
#' @examples 
#' \dontrun{
#' original<-array(1,c(300,300,50))
#' for (i in 1:5)original[(i*60)-(0:20),,]<-original[(i*60)-(0:20),,]+1
#' for (i in 1:10)original[,(i*30)-(0:15),]<-original[,(i*30)-(0:15),]+1
#' original[,,26:50]<-4-aperm(original[,,26:50],c(2,1,3))
#' 
#' img<-array(rnorm(300*300*50,original,.2),c(300,300,50))
#' img<-img-min(img)
#' img<-img/max(img)
#' 
#' try1<-segment(img,3,beta=0.5,z.scale=.3)
#' print(sum(try1$class!=original)/prod(dim(original)))
#' 
#' beta<-matrix(rep(-.5,9),nrow=3)
#' beta<-beta+1.5*diag(3)
#' try2<-segment(img,3,beta,z.scale=.3)
#' print(sum(try2$class!=original)/prod(dim(original)))
#' 
#' par(mfrow=c(2,2))
#' img(original)
#' img(img)
#' img(try1$class)
#' img(try2$class)
#' }
#' @useDynLib bioimagetools
segment <- function(img, nclust, beta, z.scale=0, method="cem", varfixed=TRUE,maxit=30, mask=array(TRUE,dim(img)), priormu=rep(NA,nclust), 
                    priormusd=rep(NULL,nclust), min.eps=10^{-7}, inforce.nclust=FALSE,start=NULL, silent=FALSE) {

mask<-as.vector(mask)
dims<-dim(img)
dims.m<-dim(mask)
dd<-length(dims)

if (length(dims.m)==2)
  {
    mask<-array(mask,c(dims.m,dims[3]))
  }

N <- prod(dims)
img<-as.vector(img)

qu<-quantile(img,(1:(nclust-1))/nclust)

class <- rep(0,length(img))
for (i in 1:(nclust-1))
class[img>qu[i]]<-i

if (length(beta)==1)
beta<-rep(beta,nclust)

if (length(beta)==nclust)
beta<-diag(beta)

beta<-as.vector(beta)

mu<-rep(0,nclust)
if(is.null(start)){start=""}
if(is.na(priormu[1]))
{
if ((method=="cem")&(start!="equal"))
{
    for (i in 1:nclust)
	{
	mu[i]<-mean(img[class==(i-1)],na.rm=TRUE)
	}
 
    for (i in 2:(nclust-1))
	if (is.na(mu)[i])
	{
	mu[i]<-mean(mu[c(i-1,i+1)],na.rm=TRUE)
	}
    if (is.na(mu)[1])mu[1]<-min(img,na.rm=TRUE)
    if (is.na(mu)[nclust])mu[nclust]<-max(img,na.rm=TRUE)
    if (sum(is.na(mu))>0)mu<-seq(mu[1],mu[nclust],length=nclust)
}
if ((method=="em")|(start=="equal"))
{
mu<-seq(min(img),max(img),length=nclust)
sigma<-rep(1/nclust,nclust)
}
}
else
{
mu<-priormu
class <- rep(0,length(img))
qu<-priormu[-1]-diff(priormu)/2
for (i in 1:(nclust-1))
class[img>qu[i]]<-i
}

sigma<-rep(.01,nclust)
    if(varfixed)
	{
	sigma<-sd(mu[class+1]-img,na.rm=TRUE)
	sigma<-rep(sigma,nclust)
	}
    if (!varfixed)for (i in 1:nclust){sigma[i]<-sd(img[class==(i-1)],na.rm=TRUE)}

    for (i in 1:nclust)
	if (is.na(sigma)[i])
	{
	sigma[i]<-median(sigma,na.rm=TRUE)
	}

counter<-0
criterium <- TRUE
pdach<-matrix(rep(1/nclust,nclust*prod(dims)),ncol=prod(dims))
pij <- rep(1,nclust)/nclust
nclust0<-nclust
if(silent)status=.status(NULL)
while(criterium)
{	
  if(silent)status=.status(status)
	counter<-counter+1
	if(!silent)cat(paste("Iteration",counter,"."))
if(method=="cem")
{
    class[!mask]<-0

    if (dd==3)
    class<-.C("segment_cem",
                    as.double(img),
                    as.integer(class),
		                as.integer(mask),
                    as.double(mu),
                    as.double(sigma),
                    as.integer(dims),
                    as.integer(nclust),
                    as.double(rep(0,nclust)),
                    as.double(beta), 
                    as.double(beta*z.scale), 
                    PACKAGE="bioimagetools")[[2]]    
    if (dd==2)
    class<-.C("segment_cem2d",
                    as.double(img),
                    as.integer(class),
		                as.integer(mask),
                    as.double(mu),
                    as.double(sigma),
                    as.integer(dims),
                    as.integer(nclust),
                    as.double(rep(0,nclust)),
                    as.double(beta), 
                    as.double(beta*z.scale), 
                    PACKAGE="bioimagetools")[[2]]    
    if(!silent)cat(".")

    class[!mask]<-NA

    
    for (i in (nclust-1):0)
	if(sum(class==i,na.rm=TRUE)==0)
	{
	  if(!silent)cat(paste("class",i+1,"removed "))
	class[(class>i)&(!is.na(class))]<-class[(class>i)&(!is.na(class))]-1    
	nclust<-nclust-1
	}
    oldmu<-mu[1:nclust]
    mu <- rep(NA,nclust)
    for (i in 1:nclust)
	{
	mu[i]<-mean(img[class==(i-1)],na.rm=TRUE)
	}
    for (i in 1:nclust)
    if (!is.na(priormu[i]))
	{
        mu[i]<-(priormusd[i]*sum(img[class==(i-1)],na.rm=TRUE)+sigma[i]*sigma[i]*priormu[i])/(sigma[i]*sigma[i]+sum(class==(i-1),na.rm=TRUE)*priormusd[i])
	}


    sigma <- rep(NA,nclust)
if(varfixed)
	{
	sigma<-sd(mu[class+1]-img,na.rm=TRUE)
	sigma<-rep(sigma,nclust)
	}
    if (!varfixed)for (i in 1:nclust){sigma[i]<-sd(img[class==(i-1)],na.rm=TRUE)}
    sigma[is.na(sigma)]<-1e-6
    if(!silent)cat(". class mean: ")
    if(!silent)cat(round(mu,3))
    if(!silent)cat(", class sigma: ")
    if(!silent)cat(round(sigma,3))
        #cat(", class sizes: ")
        #cat(table(class))
    if(!silent)cat("\n")
	

    if (counter==maxit){criterium<-FALSE}
    if (sum((mu-oldmu)^2)<min.eps){criterium<-FALSE}

    if (inforce.nclust)
    while(nclust!=nclust0)
	{
        criterium<-TRUE
        if(!silent)cat ("inforce nclust ")
	#t<-table(class)
	sigm=rep(NA,nclust)
  if (!varfixed)for (i in 1:nclust){sigm[i]<-sd(img[class==(i-1)],na.rm=TRUE)}
  if (!varfixed)sigm=sigma
	w<-which(sigma==max(sigma))
	if (length(w)>1)w<-w[sample(length(w),1)]
	class[(class>w)&(!is.na(class))]<-class[(class>w)&(!is.na(class))]+1   	
	nn<-sum(class==w)
	class[(class==w)&(img>mu[w])&(!is.na(class))]<-class[(class==w)&(img>mu[w])&(!is.na(class))]+1	
	if(w==1){
    d<-(mu[2]-mu[1])/2
    mu<-c(mu[1]+c(-d,d),mu[-1])
  }
	if(w==nclust){
    d<-(mu[nclust]-mu[nclust-1])/2
    mu<-c(mu[-nclust],mu[nclust]+c(-d,d))
  }
	if((w!=1)&(w!=nclust)){
   d<-min(mu[w+1]-mu[w],mu[w]-mu[w-1])/2
   mu<-c(mu[1:(w-1)],mu[w]+c(-d,d),mu[(w+1):nclust])
  }
	
	nclust<-nclust+1
	    sigma <- rep(NA,nclust)
if(varfixed)
	{
	sigma<-sd(mu[class+1]-img,na.rm=TRUE)
	sigma<-rep(sigma,nclust)
	}
    if (!varfixed)for (i in 1:nclust){sigma[i]<-sd(img[class==(i-1)],na.rm=TRUE)}
    sigma[is.na(sigma)]<-1e-6
	}

}#endif cem

if (method=="em")
{
class[!mask]<-0

    class<-.C("segment_cem",
                    as.double(img),
                    as.integer(class),
		    as.integer(mask),
                    as.double(mu),
                    as.double(sigma),
                    as.integer(dims),
                    as.integer(nclust),
                    as.double(rep(0,nclust)),
                    as.double(beta), 
                    as.double(beta*z.scale), 
                    PACKAGE="bioimagetools")[[2]]  

if(!silent)print(table(class))

plyi<-array(NA,c(length(img),nclust))
for (i in 1:nclust)
  {
 
    plyi[,i]<-dnorm(img,mu[i],sigma[i])
    plyi[mask==1,i]<-0
 
    plyi[,i]<- .C("segment_em",    # Berechen plyi * P(l|X)
                  as.double(img),
                  as.double(plyi[,i]),
                  as.integer(mask),
                  as.integer(class),
                  as.integer(dims),
                  as.integer(i-1),
                  as.double(beta[(i-1)*nclust+i]), 
                  as.double(beta[(i-1)*nclust+i]*z.scale), 
                  PACKAGE="bioimagetools")[[2]]  
    plyi[mask==1,i]<-0
 
    }
    plyisum<-apply(plyi,1,sum)
    plyisum[plyisum==0]<-1
 
    for (i in 1:nclust)plyi[,i]<-plyi[,i]/plyisum

    oldmu<-mu
    for (i in 1:nclust)print(summary(plyi[,i]))
    for (i in 1:nclust)mu[i]<-sum(plyi[,i]*img)/sum(plyi[,i])
    for (i in 1:nclust)sigma[i]<-sqrt(sum(plyi[,i]*(img-mu[i])^2)/sum(plyi[,i]))
  
    if(!silent)print(mu)
    if(!silent)print(sigma)

    if (counter==maxit){criterium<-FALSE}
    if (sum((mu-oldmu)^2)<min.eps){criterium<-FALSE}
    if(!silent)cat(".")
}
}#endif while loop

#class<-class+100
#for (i in 100+(0:(nclust-1)))
#class[class==(100+i)]<-order(mu)[i+1]
class[!mask]=-1
class<-array(class+1,dims)

return(list("class"=class,"mu"=mu,"sigma"=sigma))
}
