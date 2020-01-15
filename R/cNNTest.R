#' Permutation Test for cross-type nearest neighbor distances
#' @param dist a distance matrix, the upper n1 x n1 part contains distances between objects of type 1
#'   		the lower n2 x n2 part contains distances between objects of type 2
#' @param n1  numbers of objects of type 1 
#' @param n2  numbers of objects of type 2
#' @param w (optional) weights of the objects (length n1+n2)
#' @param B number of permutations to generate
#' @param alternative alternative hypothesis ("less" to test H0:Colocalization )
#' @param returnSample return sampled null distribution
#' @param parallel Logical. Should we use parallel computing?
#' @param ... additional arguments for mclapply
#' @import stats
#' @return a list with the p.value, the observed weighted mean of the cNN-distances, alternative and (if returnSample) the simulated null dist 
#' @author Fabian Scheipl
cnnTest <- function(dist, n1, n2, w = rep(1, n1+n2), 
                    B = 999, alternative = "less", returnSample = TRUE,  
                    parallel = FALSE, 
                    ...){
  
  teststat <- function(dist, n1, n2, w){
    cnn <- crossNN(dist, n1, n2, w)
    return(stats::weighted.mean(x = cnn[,'cnn'], w = cnn[,'w']))
  }
  
  obs <- teststat(dist, n1, n2, w)
  
  permutations <- replicate(B, sample(n1+n2), simplify = FALSE)
  if(parallel)nulldist <- unlist(parallel::mclapply(permutations, function(x){
    teststat(dist[x, x], n1, n2, w[x])
  }, ...))
  if(!parallel)nulldist <- unlist(lapply(permutations, function(x){
    teststat(dist[x, x], n1, n2, w[x])
  }, ...))
  p.value <- switch(alternative, 
                    greater = mean(obs < nulldist),
                    less  = mean(obs > nulldist),
                    two.sided = 0.5 * min(mean(obs < nulldist), mean(obs > nulldist)))
  ret <- list(statistic = "weighted mean of cNN-distances", 
              p.value = p.value, estimate = obs, alternative = alternative)
  ret$sample <- nulldist
  return(ret)
}