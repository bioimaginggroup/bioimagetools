.onAttach<-function(libname, pkgname)
{
  requireNamespace("EBImage")
  packageStartupMessage(paste0("Bioimagetools, ver.",utils::packageVersion("bioimagetools")))
}