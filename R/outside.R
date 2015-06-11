outside <- function(img, what, blobsize=1) {
  
  dims<-dim(img)
  N <- prod(dims)
  classimg<-as.vector(img)
  tocheck<-rep(0,N)
  tocheck[1]<-1
  if (classimg[1]!=what)
  {
    print(paste("Error, img[1,1,1,] should be",what))
    return()
  }
  
  outside <- .C("outside",
                as.integer(img),
                as.integer(dims),
                as.integer(c(what,blobsize)),
                as.integer(rep(0,N)),
                as.integer(tocheck),
                as.integer(rep(0,N)),
                as.integer(rep(0,3)), 
                PACKAGE="bioimagetools")[[4]]    
  
  outside<-array(outside,dims)
  return(outside)
}