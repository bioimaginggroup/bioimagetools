#' Writes image stack into a TIFF file.
#' Wrapper for writeTIFF
#'
#' @param img An image, a 3d or 4d array.
#' @param file File name.
#' @param bps number of bits per sample (numeric scalar). Supported values in this version are 8, 16, and 32.
#' @param twod Dimension of channels. TRUE for 2d images, FALSE for 3d images.
#' @param attr Attributes of image stack. Will be propagated to each 2d image. 
#' @param reduce if TRUE then writeTIFF will attempt to reduce the number of planes in native rasters by analyzing the image to choose one of RGBA, RGB, GA or G formats, whichever uses the least planes without any loss. Otherwise the image is always saved with four planes (RGBA).
#' 
#' @export
#' @import tiff
writeTIF<-  function (img, file, bps = NULL, twod=FALSE, reduce=TRUE, attr = attributes(img)) 
{
  require(tiff)
  if (is.null(bps)) 
    if (!is.null(attr$bits.per.sample)) 
      bps <- attr$bits.per.sample
    if (is.null(bps)) 
      bps <- 8L
    imglist <- list()
    if (length(dim(img)) == 3) {
      Z <- dim(img)[3]
      if(twod) for (i in 1:Z) imglist[[i]] <- img[, , i]/max(1,max(img[, , i]))
      if(!twod) {
        maxi<-max(1,max(img))
        for (i in 1:Z) imglist[[i]] <- img[, , i]/maxi
      }
    }
    if (length(dim(img)) == 4) {
      C <- dim(img)[3]
      Z <- dim(img)[4]
      k <- 0
      maxi <- 1:C
      for (j in 1:C) 
        {
        maxi[j] <- max(1,max(img[, , j, ], na.rm = TRUE))
        img[,,j,]<-img[,,j,]/maxi[j]
        }
      for (i in 1:Z){
        k <- k + 1
        imglist[[k]] <- img[, , , i]
      }
    }
    Z <- length(imglist)
    ati <- attributes(img)
    ati$dim <- dim(imglist[[1]])
    for (i in 1:Z) attributes(imglist[[i]]) <- ati
    writeTIFF(what = imglist, where = file, reduce = reduce, bits.per.sample = bps)
}