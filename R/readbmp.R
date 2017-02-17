#' Read bitmap files
#'
#' @param file A character vector of file names or URLs.
#' @description Read 2D grey-value BMP files
#' @return  Returns a matrix with BMP data as integer.
#' @export 
#' @author Volker J. Schmid
#' @examples bi<-readBMP(system.file("extdata/V.bmp",package="bioimagetools"))
#' image(bi,col=grey(seq(1,0,length=100)))

readBMP<-function(file)
{
input<-file(file,"rb")
if(readBin(input,integer(),size=2)!=19778)return(NULL)
readBin(input,integer(),size=4)
readBin(input,integer(),size=2)
readBin(input,integer(),size=2)
start=readBin(input,integer(),size=4)

readBin(input,integer(),size=4)
X=readBin(input,integer(),size=4)
Y=readBin(input,integer(),size=4)
readBin(input,integer(),size=2)
bpp=readBin(input,integer(),size=2)
start=start-32
for (i in 1:(start/2))
readBin(input,integer(),size=2)

result<-readBin(input,integer(),n=X*Y,size=bpp/8, signed=FALSE)
result<-array(result,c(X,Y))
close(input)

return(result)
}


