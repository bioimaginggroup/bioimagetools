#author: SilentBang, source: http://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages
is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
}