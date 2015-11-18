#' Read TIF file with classes
#'
#' @param file file
#' @param n number of classes
#'
#' @return array
#' @export
readClassTIF<-function(file,n=7)
{
  temp<-readTIF(file=file,native=FALSE,as.is=FALSE,channels=NULL)
  temp<-temp*7
  temp<-round(temp,0)
  temp[temp==0]<-NA
  return(temp)
}