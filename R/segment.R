##
##
## Copyright (c) 2011 Volker Schmid
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are
## met:
## 
##     * Redistributions of source code must retain the above copyright
##       notice, this list of conditions and the following disclaimer. 
##     * Redistributions in binary form must reproduce the above
##       copyright notice, this list of conditions and the following
##       disclaimer in the documentation and/or other materials provided
##       with the distribution.
##     * The names of the authors may not be used to endorse or promote
##       products derived from this software without specific prior
##       written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
## A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
## LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
## DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
## THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
## 



segment <- function(img, nclust, beta, z.scale=0, method="cem", varfixed=TRUE,maxit=30, mask=array(TRUE,dim(img)), priormu=rep(NA,nclust), priormusd=rep(NULL,nclust), min.eps=10^{-7}, inforce.nclust=FALSE,start=NULL ) {

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
while(criterium)
{	
	counter<-counter+1
	cat(paste("Iteration",counter,"."))
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
	cat(".")

    class[!mask]<-NA

    
    for (i in (nclust-1):0)
	if(sum(class==i,na.rm=TRUE)==0)
	{
	cat(paste("class",i+1,"removed "))
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
	cat(". class mean: ")
        cat(round(mu,3))
        cat(", class sigma: ")
        cat(round(sigma,3))
        #cat(", class sizes: ")
        #cat(table(class))
        cat("\n")
	

    if (counter==maxit){criterium<-FALSE}
    if (sum((mu-oldmu)^2)<min.eps){criterium<-FALSE}

    if (inforce.nclust)
    while(nclust!=nclust0)
	{
        criterium<-TRUE
	cat ("inforce nclust ")
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

print(table(class))

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
  
  print(mu)
  print(sigma)

    if (counter==maxit){criterium<-FALSE}
    if (sum((mu-oldmu)^2)<min.eps){criterium<-FALSE}
cat(".")
}
}#endif while loop

#class<-class+100
#for (i in 100+(0:(nclust-1)))
#class[class==(100+i)]<-order(mu)[i+1]
class[!mask]=-1
class<-array(class+1,dims)

return(list("class"=class,"mu"=mu,"sigma"=sigma))
}
