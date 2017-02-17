points2class<-function(points,img.class,x.microns,y.microns,z.microns,mask=array(TRUE,dim(img.class)))
{
X<-dim(img.class)[1]
Y<-dim(img.class)[2]
Z<-dim(img.class)[3]

color<-data.frame("x"=1+floor(X*points$X/x.microns),"y"=1+floor(Y*points$Y/y.microns),"z"=1+floor(Z*points$Z/z.microns))
class<-rep(NA,dim(color)[1])
for (i in 1:dim(color)[1])
if(mask[color[i,1],color[i,2],color[i,3]]==1)
class[i]<-img.class[color[i,1],color[i,2],color[i,3]]
class<-class[!is.na(class)]
return(table(class))
}

