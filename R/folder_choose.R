#' Choose a folder interactively
#'
#' @description Choose a folder interactively by choosing a file in that folder.
#' @return A character vector of length one giving the folder path.
#' @export
folder.choose <- function()
  {
  cat("Please choose a file in the folder.\n")
  file = base::file.choose()
if(.Platform$OS.type=="windows")
{
  f = unlist(gregexpr("\\\\",file))
  folder = substr(file,1,f[length(f)]-1)
}
else
{
  f = unlist(gregexpr("/",file))
  folder = substr(file,1,f[length(f)]-1)
}
  return(folder)
}