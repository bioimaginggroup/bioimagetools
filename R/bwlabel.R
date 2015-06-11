bwlabel = function(x) {
  #  validImage(x)
  #  .Call("bwlabel", castImage(x), PACKAGE='bioimagetools2')
  .Call("bwlabel", x, PACKAGE='bioimagetools')
}