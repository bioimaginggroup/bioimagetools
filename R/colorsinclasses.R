#' Title Compute colors in classes distribution
#'
#' @param classes Image of classes
#' @param color1 Image of first color
#' @param color2 Image of second color
#' @param mask Image mask
#' @param N Maximum number of classes
#' @param type Type of spot definition, see details
#' @param thresh1 Threshold for first color image
#' @param thresh2 Threshold for second color image
#' @param sd1 For automatic threshold, that is: mean(color1)+sd1*sd(color1)
#' @param sd2 For automatic threshold of color2
#' @param col1 Name of color 1
#' @param col2 Name of color 2
#' @param test Compute tests
#' @param plot Plot barplots
#' @param beside a logical value. If FALSE, the columns of height are portrayed as stacked bars, and if TRUE the columns are portrayed as juxtaposed bars.
#' @param ylim limits for the y axis (plot)
#' @param ... additional plotting parameters
#' 
#' @details Type of spot definitions:
#' "thresh" or "t": Threshold based (threshold can be given by thresh1/2 or automatically derived)
#' "voxel" or "v": Spots are given as binary voxel mask
#' "intensity" or "i": Voxels are weighted with voxel intensity
#'
#' @return Table of classes with color 1 (and 2)
#' @export
#' @import stats
colors.in.classes<-function(classes,color1,color2=NULL,mask=array(TRUE,dim(classes)),N=max(classes,na.rm=TRUE),type="tresh",thresh1=NULL,thresh2=NULL,sd1=2,sd2=2,col1="green",col2="red",test=FALSE,plot=TRUE,beside=TRUE,ylim=NULL,...)
{
  no2<-ifelse(is.null(color2),TRUE,FALSE)
  classes<-array(classes,dim(classes))
  color1<-array(color1,dim(color1))
  if(!no2)color2<-array(color2,dim(color2))
  mask<-array(mask==1,dim(mask))
  classes<-classes[mask]
  color1<-color1[mask]
  if(!no2)color2<-color2[mask]
  
  if (type=="t")type="thresh"
  if (type=="v")type="voxel"
  if (type=="i")type="intensity"
  
  if(type=="thresh")
  {
  
    if(is.null(thresh1))thresh1<-mean(color1)+sd1*sd(color1)
    if(!no2)if(is.null(thresh2))thresh2<-mean(color2)+sd2*sd(color2)

    color1<-color1>thresh1
    if (!no2) color2<-color2>thresh2
  }
  
  if (type=="voxel")
  {
    color1 <- color1==1
    if (!no2) color2<-color2==1
  }
  
  weight <- weight2 <- NULL
  type==("intensity")
  {
    weight<-color1
    color1<-color1>0
    weight<-weight[color1]
    
    if (!no2){
      weight2<-color2
      color2<-color2>0
      weight2<-weight2[color2]
    }
  }
  
  t10<-table.n(classes,N)
  t1<-t10/sum(t10)
  
  t20<-table.n(classes[color1],N, weight)
  t2<-t20/sum(t20)
  
  t3<-t30<-0
  if(!no2){
    t30<-table.n(classes[color2],N, weight)
    t3<-t30/sum(t30)
  }
  
  if(plot){
    tt<-rbind(t1,t2)
    if (!no2)tt<-rbind(tt,t3)
    colo<-c("grey",col1)
    if (!no2)colo<-c(colo,col2)
    print(ylim)
    if (is.null(ylim))ylim=c(0,max(c(t1,t2,t3)))
    print(ylim)
  barplot(tt,ylim=ylim,beside=beside,col=colo,...)
  }
  if (test==TRUE)
  {
    ch1<-wilcox.test(classes,classes[color1])
    print("Test dapi vs. channel 1")
    print(ch1)
    if (!no2)
    {
      print("Test dapi vs. channel 2")
      ch2<-wilcox.test(classes,classes[color2])
      print(ch2)
      print("Test channel 1 vs. channel 2")
      ch3<-wilcox.test(classes[color1],classes[color2])
      print(ch3)
    }
  }
  
  ret1<-list()
  ret1[["dapi"]]<-t1
  ret1[["col1"]]<-t2
  if(!no2)ret1[["col2"]]<-t3
  ret1[["dapi.n"]]<-t10
  ret1[["col1.n"]]<-t20
  if(!no2)ret1[["col2.n"]]<-t30
  if (type=="thresh")
    {
    ret<-thresh1
    if(!no2)ret<-c(thresh1,thresh2)
    ret1[["thresh"]]<-ret
  }

  if(test)
  {
    ret1[["test1"]]<-ch1
    if (!no2)
    {
      ret1[["test2"]]<-ch2
      ret1[["test12"]]<-ch3
    }
  }
  return(ret1)
}
