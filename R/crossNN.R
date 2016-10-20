#' Compute cross-type nearest neighbor distances 
#' @param dist a distance matrix, the upper n1 x n1 part contains distances between objects of type 1
#'   		the lower n2 x n2 part contains distances between objects of type 2
#' @param n1  numbers of objects of type 1
#' @param n2  numbers of objects of type 2
#' @param w optional weights of the objects (length n1+n2), defaults to equal weights
#' @return  a (n1+n2) x 2 matrix with the cross-type nearest neighbor distances and 
#' 			weights given as the sum of the weights of the involved objects
#' @author Fabian Scheipl
crossNN <- function(dist, n1, n2, w = rep(1, n1+n2)){
  use <- dist[-(1:n1),-((n1+1):(n1+n2))] #use only lower left block containing the cross type distances
  whereMin <- rbind(
    cbind( n1 + apply(use, 2, which.min), 1:n1), #for each type1 which is closest type2
    cbind((n1+1):(n1+n2), apply(use, 1, which.min))) #for each type2 which is closest type1
  
  cnn <- dist[whereMin]
  
  #use sum of weights of the involved objects as weight for the cross-type distance
  weights <- apply(whereMin, 1, function(x){
    sum(w[x])
  })
  return(cbind(cnn=cnn, w=w))
}