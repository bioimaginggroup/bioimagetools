
colors.in.classes<-function(classes,color1,color2=NULL,mask=array(TRUE,dim(classes)),N=max(classes,na.rm=TRUE),thresh1=NULL,thresh2=NULL,sd1=2,sd2=2,col1="green",col2="red",test=FALSE,plot=TRUE)
{
  no2<-ifelse(is.null(color2),TRUE,FALSE)
  classes<-array(classes,dim(classes))
  color1<-array(color1,dim(color1))
  if(!no2)color2<-array(color2,dim(color2))
  mask<-array(mask==1,dim(mask))
  classes<-classes[mask]
  color1<-color1[mask]
  if(!no2)color2<-color2[mask]
  
  if(is.null(thresh1))thresh1<-mean(color1)+sd1*sd(color1)
  if(!no2)if(is.null(thresh2))thresh2<-mean(color2)+sd2*sd(color2)
  
  t10<-table.n(classes,N)
  t1<-t10/sum(t10)
  
  t20<-table.n(classes[color1>thresh1],N)
  t2<-t20/sum(t20)
  
  t3<-t30<-0
  if(!no2){
    t30<-table.n(classes[color2>thresh2],N)
    t3<-t30/sum(t30)
  }
  
  if(plot){
  barplot(t1,ylim=c(0,max(c(t1,t2,t3))))
  barplot(t2,xlab=NULL,col=col1,density=40,border=col1,add=TRUE,axes=FALSE)
  if(!no2)barplot(t3,xlab=NULL,col=col2,density=40,border=col2,add=TRUE,axes=FALSE)
  }
  if (test==TRUE)
  {
    ch1<-wilcox.test(classes,classes[color1>thresh1])
    print("Test dapi vs. channel 1")
    print(ch1)
    if (!no2)
    {
      print("Test dapi vs. channel 2")
      ch2<-wilcox.test(classes,classes[color2>thresh2])
      print(ch2)
      print("Test channel 1 vs. channel 2")
      ch3<-wilcox.test(classes[color1>thresh1],classes[color2>thresh2])
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
  ret<-thresh1
  if(!no2)ret<-c(thresh1,thresh2)
  ret1[["thresh"]]<-ret
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
