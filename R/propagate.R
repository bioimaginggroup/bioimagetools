propagate = function (x, seeds, mask=NULL, lambda=1e-4) {
  #  validImage(x)
  checkCompatibleImages(x, seeds)
  
  if (!is.null(mask)) {
    checkCompatibleImages(x, mask)
    mask = castImage(mask)
  }
  
  lambda = as.numeric(lambda)
  if (lambda<0.0) stop("'lambda' must be positive" )
  
  return(.Call( "propagate", castImage(x), castImage(seeds), mask, lambda, PACKAGE='bioimagetools2'))
}