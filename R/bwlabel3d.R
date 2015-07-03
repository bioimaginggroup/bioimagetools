bwlabel3d<-function(img)
{
  .status=.status(NULL)
  Z <- dim(img)[3]
  obj <- array(0,dim(img))
  for (i in 1:Z)
  {
    temp<-bwlabel(img[,,i])
    temp[temp!=0]=temp[temp!=0]+max(obj)
    obj[,,i]<-temp
  }
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