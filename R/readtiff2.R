if(0)
  {
  readTIF<-function (files, type, all = TRUE, ...) 
{
  require(tiff)
  require(abind)
  readURL = function(url, buffer = 2^24) {
    f = try(file(url, "rb"), silent = TRUE)
    if (inherits(f, "try-error")) 
      stop(attr(f, "condition")$message)
    rawData = bufData = NULL
    while (length(bufData <- readBin(f, "raw", buffer)) > 
             0) rawData = c(rawData, bufData)
    try(close(f), silent = TRUE)
    rawData
  }

  readFun =  function(x, ...) readTIFF(x,  all = all, ...)
  stack = NULL
  for (i in seq_along(files)) {
    if (!file.exists(files[i])) {
      w = options(warn = 2)
      rawData = try(readURL(files[i]), silent = TRUE)
      options(w)
      if (inherits(rawData, "try-error")) {
        warning(paste(unlist(strsplit(attr(rawData, "condition")$message, 
                                      "(converted from warning) ", fixed = TRUE)), 
                      sep = "", collapse = ""))
        next
      }
      else img = readFun(rawData)
    }
    else if (file.info(files[i])$isdir) {
      warning(sprintf("Cannot open %s: Is directory.", 
                      files[i]))
      next
    }
    else img = readFun(files[i])
    nf = if (is.list(img)) 
      length(img)
    else 1
    for (j in seq_len(nf)) {
      frame = if (is.list(img)) 
        img[[j]]
      else img
      if (is.null(stack)) {
        refName = if (nf > 1) 
          paste(files[i], j, sep = ",")
        else files[i]
        dim = dim(frame)
        channels = channelLayout(frame)
        stack = frame
      }
      else {
        if (identical(dim, dim(frame))) 
          stack = abind(stack, frame, along = length(dim) + 
                          1)
        else if (!identical(dim[1:2], dim(frame)[1:2])) 
          stop(sprintf("%s: Image size (%s) does not match reference size (%s) of %s. All images need to have the same width and height.", 
                       if (nf > 1) 
                         paste(files[i], j, sep = ",")
                       else files[i], paste(dim(frame)[1:2], collapse = " x "), 
                       paste(dim[1:2], collapse = " x "), refName))
        else stop(sprintf("%s: Channel layout (%s) does not match reference channel layout (%s) of %s. All images need to have the same number of channels.", 
                          if (nf > 1) 
                            paste(files[i], j, sep = ",")
                          else files[i], channelLayout(frame), channels, 
                          refName))
      }
    }
  }
  if (is.null(stack)) 
    stop("Empty image stack.")
  else stack #Image(transpose(stack), colormode = if (isTRUE(charmatch(channels, 
    #                                                            "G") == 1)) 
    #"Grayscale"
    #         else "Color")
}

channelLayout = function(x){
  y = dim(x) 
  return( switch(if(length(y)==2) 1 else if (length(y)==3 && y[3]<=4) y[3] else 5, 'G', 'GA', 'RGB', 'RGBA', 'unknown') )
}
}
