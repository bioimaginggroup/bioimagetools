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
             col=rep("black",classes)
             col[classes+1-i]="red"
             graphics::boxplot(rev(distances[[i]]),na.rm=TRUE,use.cols=FALSE,outline=FALSE,horizontal=TRUE,
                     names=classes:1,boxwex=0.5,pch=19,cex=.1,range=1,ylim=ylim,
                     xlab="",ylab="", axes=FALSE, border=col)
             graphics::axis(1)#,at=seq(0,ylim[2],by=.1))
             graphics::axis(2, lwd=0, labels=classes:1, at=1:classes,las=2)
           }
         },
         "min"={
           for (i in 1:classes)
           {
             border=rep(NA,classes)
             border[classes+1-i]="red"
             col=rep("grey",classes)
             col[classes+1-i]="white"
             milo<-lapply(distances[[i]],min)
             graphics::barplot(rev(unlist(milo)),xlab="",names.arg=classes:1,horiz=TRUE,border=border,col=col,las=1,xlim=ylim,space=0.5)
             #graphics::barplot(unlist(milo),xlab=paste("minimal distance to nearest neighbour for class",i),ylim=ylim,width=0.8,space=.25,names.arg=1:classes)
           }
          },
           
           "quantile"={
             for (i in 1:classes)
             {
               milo<-lapply(distances[[i]],quantile,qu)
               border=rep(NA,classes)
               border[classes+1-i]="red"
               col=rep("grey",classes)
               col[classes+1-i]="white"
               graphics::barplot(rev(unlist(milo)),xlab="",names.arg=classes:1,horiz=TRUE,border=border,col=col,las=1,xlim=ylim,space=0.5)
               #graphics::barplot(unlist(milo),xlab=paste("distance to nearest neighbour for class",i),ylim=ylim,width=0.8,space=.25,names.arg=1:classes)
             }
             
           }
  )
         }
  