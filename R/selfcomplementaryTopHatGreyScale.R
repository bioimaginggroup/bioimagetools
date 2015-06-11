selfcomplementaryTopHatGreyScale = function (x, kern=makeBrush(5, shape='diamond')) {
  return (.Call("lib_tophat_greyscale", x, kern, as.integer(2), PACKAGE='bioimagetools') )
}