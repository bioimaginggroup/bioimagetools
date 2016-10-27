#' Title Plot nearest class distances
#'
#' @param distances list of list with distances as produced by nearestClassDistances()
#' @param method "boxplot", "min" or "quantile" 
#' @param classes number of classes, default=7
#' @param ylim limits for distances, default=c(0,1)
#' @param qu quantile for method="quantile"; default 0.01
#' @param mfrow mfrow option forwarded to par; default NULL, computes some optimal values
#' @return plots
#' @export
#'
plotNearestClassDistances<-function(distances,method,classes=length(distances),ylim=c(0,1),qu=0.01,mfrow=NULL)
{
  if(is.null(mfrow))
    {
    mfrow=ceiling(classes^(2/3))
    mfrow=c(ceiling(classes/mfrow),mfrow)
  }
  graphics::par(mfrow=mfrow)
  switch(method,
         "boxplot"={
           for (i in 1:classes)
           {
             graphics::boxplot(rev(distances[[i]]),na.rm=TRUE,use.cols=FALSE,outline=FALSE,horizontal=TRUE,
                     names=classes:1,boxwex=0.5,pch=19,cex=.1,range=1,main=paste("class",i),ylim=ylim,
                     xlab="distance to nearest neighbour in microns",ylab="class", axes=FALSE)
             graphics::axis(1,at=seq(0,ylim[2],by=.1))
             graphics::axis(2, lwd=0, labels=classes:1, at=1:classes)
           }
         },
         "min"={
           for (i in 1:classes)
           {
             milo<-lapply(distances[[i]],min)
             graphics::barplot(unlist(milo),xlab=paste("neighbours of class",i),ylim=ylim,width=0.8,space=.25,names.arg=1:classes)
           }
          },
           
           "quantile"={
             for (i in 1:classes)
             {
               milo<-lapply(distances[[i]],quantile,qu)
               graphics::barplot(unlist(milo),xlab=paste("neighbours of class",i),ylim=ylim,width=0.8,space=.25,names.arg=1:classes)
             }
             
           }
  )
         }
  