thresh = function (x, w=5, h=5, offset=0.01) {
  if (w<2 || h<2) stop ("width 'w' and height 'h' must be larger than 1")
  return ( .Call("thresh", x, as.numeric( c(w, h, offset) ), PACKAGE='bioimagetools') )
}
