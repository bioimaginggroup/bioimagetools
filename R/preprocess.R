#' Get central moments of objects in a single-channel image stack
#' 
#' Uses the methodology used for segmentation in the RBImage vignette 
#'  (threshhold->opening->fillHull) from all 3 spatial directions and 
#'  overlays these results to get a binary image which is then segmented 
#'  with bwlabel3d. Central moments are extracted with cmoments3d 
#' 
#' @param file the path of the image stack
#' @param threshold  the quantile of intensities used for thresholding if quantile=TRUE 
#'   	  or the intensity value if quantile=FALSE, defaults to the 80% quantile
#' @param threshW, threshH width and height of the moving rectangular window for threshold, defaults to 5.  
#' @param brushsize the brushsize for makeBrush for opening, defaults to 3 
#' @param quantile defaults to TRUE
#' @return a list with the original stack, the labeled stack, and the matrix of central moments of the found objects 
preprocess <- function(file, threshold=.95, threshW = 5,  threshH = 5, brushsize=3, quantile=TRUE){
  stopifnot(require(EBImage))
  
  cat("reading file....")
  #read file
  im <- readImage(file)
  
  cat("thresholding & smoothing image....")
  #threshold
  #	if(quantile){
  #		thresh <- quantile(im, threshold)
  #		while(thresh == 0 &&  threshold < 1){
  #			threshold <- threshold + .01
  #			thresh <- quantile(im, threshold)
  #			cat("\n cutoff was 0. increasing threshold to ", threshold, "\n")
  #		}
  #	} else {
  #		thresh <- threshold
  #	} 
  if(quantile){
    thresh <- quantile(im, threshold)
    stopifnot(thresh != 0)
    thresh <- quantile(im, threshold)
  } else {
    thresh <- threshold
  } 
  
  
  
  imThresh <- thresh(im, threshW, threshH, thresh)
  
  #smooth binary image from all directions
  mask1 <- opening(imThresh, makeBrush(brushsize, shape='disc'))
  mask1 <- fillHull(mask1)
  mask2 <- opening(aperm(imThresh, c(1,3,2)), makeBrush(brushsize, shape='disc'))
  mask2 <- fillHull(mask2)
  mask3 <- opening(aperm(imThresh, c(2,3,1)), makeBrush(brushsize, shape='disc'))
  mask3 <- fillHull(mask3)
  mask <- fillHull(mask1 + aperm(mask2, c(1,3,2)) + aperm(mask3, c(3, 1, 2)))
  stopifnot(any(as.logical(mask)))
  
  cat("segmentation....")
  #segmentation
  label <- bwlabel3d(mask)
  
  cat("get moments\n")
  #extract moments 
  mom <- cmoments3d(label, im)
  
  cat("found", max(label), "objects in image.\n")
  return(list(im=im, 
              label=label, 
              moments=mom))	
}