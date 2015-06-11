.onAttach<-function(libname, pkgname)
{
  packageStartupMessage(message(paste0("Bioimagetools, ",packageVersion("bioimagetools"))))
}
