## ----loadlibrary, include=FALSE, cache=TRUE------------------------------
library(bioimagetools)

## ----readtif, warnings=FALSE, collapse=TRUE, cache=TRUE------------------
cell <- readTIF("http://ex.volkerschmid.de/cell.tif")
print(dim(cell))
print(attributes(cell)$dim)
print(attributes(cell)$bits.per.sample)
par(pty="s")
img(cell, z=25 ,col="rgb")

## ----writetif, warnings=FALSE, collapse=TRUE, cache=TRUE-----------------
writeTIF(cell, file="my_cell.tif")

## ----readclasstif, warnings=FALSE, collapse=TRUE, cache=TRUE-------------
writeTIF(2*EBImage::thresh(cell[,,1,])+EBImage::thresh(cell[,,2,]), file="simple.tif")
mysimple <- readClassTIF("simple.tif", n=3)
par(pty="s")
img(mysimple[,,25],col="red",up=3)

## ----echo=FALSE----------------------------------------------------------
file.remove("my_cell.tif")
file.remove("simple.tif")

