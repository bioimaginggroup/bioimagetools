#' Title Plot nearest class distances
#'
#' @param longlist array with distances as produces by nearest.class.distances()
#' @param method "boxplot", "min" or "quantile" 
#' @param classes number of classes, default=7
#' @param ylim limits for distances, default=c(0,1)
#' @param qu quantile for method="quantile", default=0.01
#'
#' @return plots
#' @export
#' @import graphics
#'
plotNearestClassDistances<-function(longlist,method,classes=dim(longlist)[1],ylim=c(0,1),qu=0.01)
{
  switch(method,
         "boxplot"={
           
           par(mfrow=c(1,classes))
           for (i in 1:classes)
           {
             boxplot(longlist[i,classes:1,],na.rm=TRUE,use.cols=FALSE,outline=TRUE,horizontal=TRUE,
                     names=classes:1,boxwex=0.5,pch=19,cex=.1,range=0,main=paste("class",i),ylim=ylim,
                     xlab="distance to nearest neighbour in microns",ylab="class", axes=FALSE)
             axis(1,at=seq(0,ylim[2],by=.1))
             axis(2, lwd=0, labels=classes:1, at=1:classes)
           }
         },
         "min"={
           milo <- apply(longlist,1:2,min)
           par(mfrow=c(1,classes))
           for (i in 1:classes)
           {
             barplot(milo[i,],xlab=paste("neighbours of class",i),ylim=ylim,width=0.8,space=.25)
           }
          },
           
           "quantile"={
             milo <- apply(longlist,1:2,quantile,qu)
             par(mfrow=c(1,classes))
             for (i in 1:classes)
             {
               barplot(milo[i,],xlab=paste("neighbours of class",i),ylim=ylim,width=0.8,space=.25)
             }
             
           }
  )
         }
  