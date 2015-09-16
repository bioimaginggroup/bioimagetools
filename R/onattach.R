.onAttach<-function(libname, pkgname)
{
  packageStartupMessage(paste0("Bioimagetools, ",packageVersion("bioimagetools")))
}
