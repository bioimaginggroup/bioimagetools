#' Binary segmentation in 3d
#'
#' @param img A 3d array. x is considered as a binary image, whose pixels of value 0 are considered as background ones and other pixels as foreground ones.
#'
#' @return A Grayscale 3d array, containing the labelled version of x.
#' @export
#' @import EBImage
#'
bwlabel3d<-function(img)
{
  .status=.status(NULL)
  Z <- dim(img)[3]
  X <- dim(img)[1]
  Y <- dim(img)[2]
  
  obj<-bwlabel(img)
  plus <- apply(obj,3,max)
  plus<-cumsum(plus)
  plus <- array(rep(c(0,plus[-Z]),each=X*Y),c(X,Y,Z))
  plus[obj==0]<-0
  obj <- obj+plus

  temp2 <- obj[,,1]!=0
  
  for (i in 2:Z)
  {
    .status=.status(.status)
    temp1 <- temp2
    temp2 <- obj[,,i]!=0
    overlap <- temp1&temp2
    if(any(overlap))
    {
      labels1 <-unique(obj[,,i-1][overlap])
      for (j in labels1)
      {
        w <- which(obj[,,i-1]==j)
        labels2 <- unique(obj[,,i][w])
        labels2 <- labels2[labels2!=0]
        for (k in labels2)
        {
          obj[obj==k] <- j
        }
      }
    }
  }
  labels<-unique(as.vector(obj))
  labels<-sort(labels[labels!=0])
  for (i in 1:length(labels))
  {
    .status=.status(.status)
    obj[obj==labels[i]]=i
  }
  return(obj)
}