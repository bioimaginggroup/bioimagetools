.onAttach<-function(libname, pkgname)
{
  packageStartupMessage(paste0("Bioimagetools ", utils::packageVersion("bioimagetools")))
}
