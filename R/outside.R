#' Segmentation of the background of 3D images based on classes
#'
#' @param img is a 3d arrary representing an image.
#' @param what is an integer of the class of the background.
#' @param blobsize is an integer, representing the minimal diameter for bridges from the outside. E.g., a blobsize=3 allows for holes of size 2*(blobsize-1)=4 in the edge of the object.
#'
#' @return A binary 3d array: 1 outside the object, 0 inside the object
#' @export

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