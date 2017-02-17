require(spatstat)
points<-pp3(p$X,p$Y,p$Z,box3(xrange=range(p$X),yrange=range(p$Y),zrange=range(p$Z)))
K3est(points)

