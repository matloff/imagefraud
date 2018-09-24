
rmBoxAvErrors <- function(im_out, boxside, thresh){
  
  maxs <- 0
  width <- nrow(im_out)
  height <- ncol(im_out)
  for (i in 1:(width-boxside+1)){
    for (j in 1:(height-boxside+1)){
      endw <- i+(boxside-1)
      endh <- j+(boxside-1)
      new <- mean(im_out[i:endw,j:endh])
      if (new > maxs){ maxs <- new }
    }
  }

  im_out2 <- im_out
  
  for (i in 1:(width-boxside+1)){
    for (j in 1:(height-boxside+1)){
      endw <- i+(boxside-1)
      endh <- j+(boxside-1)
      if(mean(im_out[i:endw,j:endh]) < (maxs*thresh)){
        im_out2[i:endw,j:endh] <- 0}
    }
  }
  
  im_out2
  
}
