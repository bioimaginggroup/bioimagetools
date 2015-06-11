#' Permutation Test for cross-type nearest neighbor distances
#' @param im1, im2  image stacks as returned by preprocess
#' @param hres, vres horizontal and vertical resolution of the stacks
#' @param B number of permutations to generate
#' @param alternative alternative hypothesis ("less" to test H0:Colocalization )
#' @param returnSample return sampled null distibution
#' @param ... additional arguments for papply
#' @return a list with the p.value, the observed weighted mean of the cNN-distances
testColoc <- function(im1, im2, hres = 0.1023810, vres = 0.2500000, B=999, alternative = "less", returnSample = TRUE, ...){
  #extract centers and adjust to resolution
  centers <- rbind(im1$moments[,c('m.x','m.y','m.z')], im2$moments[,c('m.x','m.y','m.z')])
  centers <- t(t(centers) * c(rep(hres,2),vres))
  
  n1 <- nrow(im1$moments)
  n2 <- nrow(im2$moments)
  w <- c(im1$moments[,'w'], im2$moments[,'w'])
  dist <- as.matrix(dist(centers)) 
  return(cnnTest(dist, n1, n2, w, B, alternative, returnSample, ...))
}
