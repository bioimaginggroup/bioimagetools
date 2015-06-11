openingGreyScale = function (x, kern=makeBrush(5, shape='diamond')) {
  return (.Call("lib_opening_closing_greyscale", x, kern, as.integer(0), PACKAGE='bioimagetools') )
}