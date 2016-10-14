require(bioimagetools)
punktdaten<-read.csv("/home/schmid/Ubuntu One/projects/bioimagetools/pkg/bioimagetools/test/cell_149_polII_in_nucleus.csv",skip=1,dec=",",na.strings="N/A",check.names=FALSE) 
punktdaten<-punktdaten[-1,] 
X<-punktdaten[,11]
Y<-punktdaten[,12]
Z<-punktdaten[,13]
X<-round(X,1)
Y<-round(Y,1)
Z<-round(Z,1)
nextneighbourdistribution(X,Y,Z)

require(spatstat)
points<-pp3(X,Y,Z,box3(xrange=range(X),yrange=range(Y),zrange=range(Z)))
K3est(points)

