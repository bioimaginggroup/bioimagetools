erodeGreyScale = function (x, kern=makeBrush(5, shape='diamond')) {
  return (.Call("lib_erode_dilate_greyscale", x, kern, as.integer(0), PACKAGE='bioimagetools') )
}