closing = function (x, kern=makeBrush(5, shape='diamond')) {
  erode(dilate(x, kern), kern)
}