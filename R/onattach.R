.onAttach<-function(libname, pkgname)
{
  packageStartupMessage(paste0("Bioimagetools ver.", utils::packageVersion("bioimagetools")))
}
