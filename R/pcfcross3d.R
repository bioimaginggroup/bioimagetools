pcf.cross.3D<-
function(X,Y,Z,X2,Y2,Z2,psz=25,width=1,intensity=NULL,intensity2=NULL,parallel=FALSE,bw=0.01)
{
actualwidth=width
width=1.1*width
N<-length(X)
N2<-length(X2)
width.psz<-floor(width*psz)+1

x<-round(X*psz,0)+width.psz+1
y<-round(Y*psz,0)+width.psz+1
z<-round(Z*psz,0)+width.psz+1
x2<-round(X2*psz,0)+width.psz+1
y2<-round(Y2*psz,0)+width.psz+1
z2<-round(Z2*psz,0)+width.psz+1

ID<-1:N
ID2<-1:N2
ID.matrix<-ID.matrix2<-array(NA,c(round(max(c(X,X2))*psz,0)+(2*width.psz+1),round(max(c(Y,Y2))*psz,0)+(2*width.psz+1),round(max(c(Z,Z2))*psz,0)+(2*width.psz+1)))

if (!is.null(intensity))
{
#intensity<-intensity/mean(intensity)
intensity.mx<-array(0,dim(ID.matrix))
intensity.mx[width.psz+(1:dim(intensity)[1]),width.psz+(1:dim(intensity)[2]),width.psz+(1:dim(intensity)[3])]<-intensity
intensity<-TRUE
}
if (!is.null(intensity2))
{
#intensity<-intensity/mean(intensity)
intensity.mx2<-array(0,dim(ID.matrix2))
intensity.mx2[width.psz+(1:dim(intensity2)[1]),width.psz+(1:dim(intensity2)[2]),width.psz+(1:dim(intensity2)[3])]<-intensity2
intensity2<-TRUE
}

for (i in 1:N)
{
ID.matrix[x[i],y[i],z[i]]<-ID[i]
}
for (i in 1:N2)
{
ID.matrix2[x2[i],y2[i],z2[i]]<-ID2[i]
}

dist<-0:width.psz

extractneighbour<-function(i)
{
res<-NULL
int<-NULL
neighbours<-ID.matrix2[x[i]+dist,y[i]+dist,z[i]+dist]
neighbours<-as.vector(neighbours)
neighbours<-neighbours[!is.na(neighbours)]
neighbours<-neighbours[neighbours!=i]
for (j in neighbours)
{
res<-c(res,sqrt(sum((c(X[i],Y[i],Z[i])-c(X2[j],Y2[j],Z2[j]))^2)))
if (!is.null(intensity))int<-c(int,intensity.mx[x[i],y[i],z[i]]*intensity.mx2[x2[j],y2[j],z2[j]])
}
return(rbind(res,int))
}

if(parallel)dist<-mclapply(1:N,extractneighbour)
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
  {
    single<-1/weight[dist>breaks[i]&dist<=breaks[i+1]]
    single[single==Inf]<-0
    counts[i]<-sum(single)
  }
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

