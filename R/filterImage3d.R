#' Apply filter to 3D images
#'
#' @description A filter is applied to a 3D array representing an image. So far only variance filters are supported.
#' 
#' @param img is a 3d arrary representing an image.
#' @param filter is the filter to be applied. Options: var: Variance filter.
#' @param window half size of window; i.e. window=1 uses a window of 3 voxels in each direction.
#' @param z.scale ratio of voxel dimension in x/y direction and z direction.
#' @param silent Logical. If FALSE, information on progress will be printed.
#'
#' @return Multi-dimensional array of filtered image data.
#' @export
#'
#' 
filterImage3d<- function(img, filter="var", window, z.scale=1, silent=FALSE) 
{
  
  dims<-dim(img)
  N <- prod(dims)
  img<-as.vector(img)
  filtered<-rep(0,N)
  
  if (filter=="var")
  {
    filtered <- .C("varfilter",
                   as.double(img),
                   as.double(filtered),
                   as.double(c(window,z.scale)),
                   as.integer(dims), 
                   as.integer(filtered),
                   as.double(c(0,0)),
                   as.integer(ifelse(silent,1,0)),
                   PACKAGE="bioimagetools")
    minmax <- filtered[[6]]
    filtered <- filtered[[5]]
  }
  if (filter=="max")
  {
    filtered <- .C("maxfilter",
                   as.double(img),
                   as.double(filtered),
                   as.double(c(window,z.scale)),
                   as.integer(dims), 
                   as.integer(filtered),
                   as.double(c(0,0)),
                   as.integer(ifelse(silent,1,0)),
                   PACKAGE="bioimagetools")
    minmax <- filtered[[6]]
    filtered <- filtered[[5]]
  }
  if (filter=="min")
  {
    filtered <- .C("minfilter",
                   as.double(img),
                   as.double(filtered),
                   as.double(c(window,z.scale)),
                   as.integer(dims), 
                   as.integer(filtered),
                   as.double(c(0,0)),
                   as.integer(ifelse(silent,1,0)),
                   PACKAGE="bioimagetools")
    minmax <- filtered[[6]]
    filtered <- filtered[[5]]
  }
  filtered<-array(filtered,dims)
  filtered<-minmax[1]+(minmax[2]-minmax[1])*filtered/65535
  return(filtered)
}  