ocontour = function(x) {
  #  validImage(x)
  storage.mode(x)='integer'
  y = .Call('ocontour', x, PACKAGE='bioimagetools2')[-1]
  y = lapply(y, function(z) matrix(z, ncol=2, byrow=TRUE))
  names(y) = seq(along=y)
  y[sapply(y, nrow)>0]
}