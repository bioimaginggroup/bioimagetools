pcf3D<-function(X,Y,Z,psz=25,width=1,intensity=NULL,parallel=FALSE,bw=0.01)
{
actualwidth=width
width=1.1*width
N<-length(X)
width.psz<-floor(width*psz)+1

x<-round(X*psz,0)+width.psz+1
y<-round(Y*psz,0)+width.psz+1
z<-round(Z*psz,0)+width.psz+1

ID<-1:N
ID.matrix<-array(NA,c(round(max(X)*psz,0)+(2*width.psz+1),round(max(Y)*psz,0)+(2*width.psz+1),round(max(Z)*psz,0)+(2*width.psz+1)))

if (!is.null(intensity))
{
#intensity<-intensity/mean(intensity)
intensity.mx<-array(0,dim(ID.matrix))
intensity.mx[width.psz+(1:dim(intensity)[1]),width.psz+(1:dim(intensity)[2]),width.psz+(1:dim(intensity)[3])]<-intensity
intensity<-TRUE
}

for (i in 1:N)
{
ID.matrix[x[i],y[i],z[i]]<-ID[i]
}

dist<-0:width.psz

extractneighbour<-function(i)
{
res<-NULL
int<-NULL
neighbours<-ID.matrix[x[i]+dist,y[i]+dist,z[i]+dist]
neighbours<-as.vector(neighbours)
neighbours<-neighbours[!is.na(neighbours)]
neighbours<-neighbours[neighbours!=i]
for (j in neighbours)
{
res<-c(res,sqrt(sum((c(X[i],Y[i],Z[i])-c(X[j],Y[j],Z[j]))^2)))
if (!is.null(intensity))int<-c(int,intensity.mx[x[i],y[i],z[i]]*intensity.mx[x[j],y[j],z[j]])
}
return(rbind(res,int))
}

if(parallel)dist<-parallel::mclapply(1:N,extractneighbour)
if(!parallel)dist<-lapply(1:N,extractneighbour)
dist<-unlist(dist)
if(!is.null(intensity))
{
weight<-dist[seq(2,length(dist),by=2)]
dist<-dist[seq(1,length(dist),by=2)]
weight<-weight[dist<=width]
dist<-dist[dist<=width]
}
else
{
dist<-dist[dist<=width]
weight<-rep(1,length(dist))
}

breaks=seq(0,width,length=100)
dr<-width/100
counts<-rep(0,99)
for (i in 1:99)
counts[i]<-sum(1/weight[dist>breaks[i]&dist<=breaks[i+1]])
counts<-counts/N/4/pi/dr/(seq(0,width,length=100)[-100])/(seq(0,width,length=100)[-100])

breaks<-0.5*(breaks[2:100]+breaks[1:99])
dist<-rep(0,99)
for (i in 2:99)
{
dist0<-rep(0,99)
for (j in 2:99)
dist0[j]<-dnorm(breaks[i],breaks[j],bw)
dist<-dist+counts[i]*(dist0/sum(dist0))
}
dist<-dist[breaks<=actualwidth]
breaks<-breaks[breaks<=actualwidth]
return(list("x"=breaks,"y"=dist))
}

