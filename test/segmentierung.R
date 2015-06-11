require(bioimagetools)
require(EBImage)

test<-readImage("/home/schmid/bioimg/software/bioimagetools/test/b_090623_25_DAPI.tif")

test2<-test[,,35]
image(test2)

Z<-which(apply(test,3,mean)>.04)
mask<-maskdapi(img=test,thresh=.2,filter=3)
image(mask[,,35])


seg1<-segment(test,nclust=5,beta=.2,mask=mask)
image(seg1$class[,,35])

seg1<-segment(test,nclust=5,beta=.2,mask=mask,varfixed=FALSE)
image(seg1$class[,,35])

seg1<-segment(test2,nclust=5,beta=.2,mask=mask[,,35],inforce.nclust=TRUE)
image(seg1$class)

seg1<-segment(test2,nclust=5,beta=.2,mask=mask[,,35],varfixed=FALSE)
image(seg1$class)

seg1<-segment(test2,nclust=5,beta=.2,mask=mask[,,35],varfixed=FALSE)
image(seg1$class[,,35])


std<-standardize(test,mask=mask,N=32,sd=6)
table.n(std,32)
colors.in.classes(std,std)
  
plup<-function(i)
{
library(foreach)
library(doParallel)
registerDoParallel()
foreach(i=1:10000) %dopar%
{
#simuliere Daten zum Testen
test2<-runif(128*128,0,1)
test2<-sort(test2)
test2<-array(test2,c(128,128))
image(test2,col=grey(seq(0,1,by=1/1000)))


# Simulierter grüner Kanal hängt von test2 ab
green<-array(test2*runif(128*128,0,1),c(128,128))
image(green,col=grey(seq(0,1,by=1/1000)))

# Simulierter roter Kanal unabhängig von test2
red<-array(runif(128*128,0,1)^2,c(128,128))
image(red,col=grey(seq(0,1,by=1/1000)))

mask<-array(0,c(128,128))
mask[10:100,10:100]<-TRUE

test2[c(1:9,101:128),]<-runif(37*128,0,.1)
test2[,c(1:9,101:128)]<-runif(37*128,0,.1)
image(test2,col=grey(seq(0,1,by=1/1000)))

# Standardisiere test2 in 32 Klassen
std<-standardize(test2,N=32,sd=4,mask=mask)
image(std,col=grey(seq(0,1,by=1/1000)))

# Berechne Tabelle der Klassenzugehörigkeiten
t<-table.n(std,32)
# und plotte 
barplot(t)
# Unterste und oberste Klassen hier nicht besetzt

#Auswertung: Wieviel grünes und rotes Signal in standardisierten Klassen, einschl. Tests
cc<-colors.in.classes(std,green,red,col1="green",col2="red",test=TRUE,mask=mask)
# Verändere Threshold für Farben
cc<-colors.in.classes(std,green,red,sd1=1,sd2=2,col1="green",col2="blue",test=TRUE)

# Segmentiere test2-Bild in 7 Klassen 
test.seg<-segment(test2,nclust=7,beta=.3)
image(test.seg$class)
# Auswertung, diesmal in 7 Klassen
cc<-colors.in.classes(test.seg$class,green,red,col1="green",col2="blue",test=TRUE)
}
}

library(parallel)
mclapply(1:10000,plup)
