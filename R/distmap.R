distmap = function (x, metric=c('euclidean', 'manhattan')) {
  if (any(is.na(x))) stop("'x' shouldn't contain any NAs")
  metric = match.arg(metric)
  imetric = switch(metric,euclidean=0,manhattan=1)
  return (.Call("distmap", x, as.integer(imetric), PACKAGE='bioimagetools'))
}