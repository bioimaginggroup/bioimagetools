
#' Computes moments from image objects
#' 
#' Computes intensity-weighted centers of objects and their mass (sum of intensities)
#' 
#' @param mask a labeled stack as returned from bwlabel3d
#' @param ref the original image stack
#' @return a matrix with the moments of the objects in the stack
cmoments3d <- function(mask, ref){
  labels <- 1:max(mask)
  ret <- t(sapply(labels, function(x){
    ind <- which(mask == x, arr.ind=T)
    w <- ref[ind]
    return(c(x, apply(ind, 2, weighted.mean, w=w), sum(w)))
  }))
  colnames(ret) <- c("label","m.x","m.y","m.z","w")
  return(ret)
}	