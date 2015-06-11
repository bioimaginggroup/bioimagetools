makeBrush = function(size, shape=c('box', 'disc', 'diamond', 'gaussian', 'line'), step=TRUE, sigma=0.3, angle=45) {
  if(! (is.numeric(size) && (length(size)==1L) && (size>=1)) ) stop("'size' must be an odd integer.")
  shape=match.arg(shape)
  
  if(size %% 2 == 0){
    size = size + 1
    warning(paste("'size' was rounded to the next odd number: ", size))
  }
  
  if (shape=='box') z = array(1,dim=c(size,size))
  else if (shape == 'line') {
    angle = angle %% 180
    angle.radians = angle * pi / 180;
    z.y = ceiling(abs(cos(angle.radians) * size))
    z.x = ceiling(abs(sin(angle.radians) * size))
    if (z.y == 0)
      z.y = 1
    if (z.x == 0)
      z.x = 1
    z = array(0, dim=c(z.y, z.x));
    for (i in 1:size) {
      i.y = ceiling(cos(angle.radians) * i)
      i.x = ceiling(sin(angle.radians) * i)
      if (i.y < 0)
        i.y = z.y + i.y
      if (i.x < 0)
        i.x = z.x + i.x
      if (i.y == 0)
        i.y = 1
      if (i.x == 0)
        i.x = 1
      z[i.y, i.x] = 1
    }
  }
  else if (shape=='gaussian') {
    x = seq(-(size-1)/2, (size-1)/2, length=size)
    x = matrix(x, nrow=size, ncol=size)
    z = exp(- (x^2 + t(x)^2) / (2*sigma^2))
    z = z / sum(z)
  } else {
    ## pixel center coordinates
    x = 1:size -((size+1)/2)
    
    ## for each pixel, compute the distance from its center to the origin, using L1 norm ('diamond') or L2 norm ('disc')
    if (shape=='disc') {
      z = outer(x, x, FUN=function(X,Y) (X*X+Y*Y))
      mz = (size/2)^2
      z = (mz - z)/mz
      z = sqrt(ifelse(z>0, z, 0))
    } else {
      z = outer(x, x, FUN=function(X,Y) (abs(X)+abs(Y)))
      mz = (size/2)
      z = (mz - z)/mz
      z = ifelse(z>0, z, 0)
    }
    
    if (step) z = ifelse(z>0, 1, 0)
  }
  z
}