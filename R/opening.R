opening = function (x, kern=makeBrush(5, shape='diamond')) {
  dilate(erode(x, kern), kern)
}