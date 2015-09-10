#' Writes image stack into a TIFF file.
#' Wrapper for writeTIFF
#'
#' @param img An image, a 3d or 4d array.
#' @param file File name.
#' @param bps number of bits per sample (numeric scalar). Supported values in this version are 8, 16, and 32.
#' @param twod Dimension of channels. TRUE for 2d images, FALSE for 3d images.
#' @param attr Attributes of image stack. Will be propagated to each 2d image. 
#' @export
writeTIF<-  function (img, file, bps = NULL, twod=FALSE, attr = attributes(img)) 
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
      if(twod) for (i in 1:Z) imglist[[i]] <- img[, , i]/max(img[, , i])
      if(!twod) {
        maxi<-max(img)
        for (i in 1:Z) imglist[[i]] <- img[, , i]/maxi
      }
    }
    if (length(dim(img)) == 4) {
      C <- dim(img)[3]
      Z <- dim(img)[4]
      k <- 0
      maxi <- 1:C
      for (j in 1:C) maxi[j] <- max(img[, , j, ], na.rm = TRUE)
      for (i in 1:Z) for (j in 1:C) {
        k <- k + 1
        imglist[[k]] <- img[, , j, i]/maxi[j]
      }
    }
    Z <- length(imglist)
    ati <- attributes(img)
    ati$dim <- dim(imglist[[1]])
    for (i in 1:Z) attributes(imglist[[i]]) <- ati
    writeTIFF(what = imglist, where = file, reduce = FALSE, bits.per.sample = bps)
}