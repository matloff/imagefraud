ELAlocalization <- function(image, Q=0.95, scale=25, ratio=0.3){

require(jpeg)
require(EBImage)
  
im1<- readJPEG(image)

r<-writeJPEG(im1, raw(), quality=Q)

im2 <- readJPEG(r)

im_out<-(abs(im1-im2)*scale)

im_out <-apply(im_out, c(1,2), mean)

im_out <- t(im_out)

}

#image<- '/Users/robinyancey/desktop/Tp_D_NNN_M_N_ani10132_ani10123_12477.jpg'
image<- '/Users/robinyancey/desktop/Tp_D_NRN_S_N_ani10210_ani10209_12373.jpg'
Q <- 0.95
scale <- 25
ratio <- 0.25
im_out <- ELAlocalization(image, Q, scale, ratio)
display(im_out)

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

# this is just example input
boxside <- 16
thresh <- 0.1
im_out <- rmBoxAvErrors(im_out, boxside, thresh)
display(im_out)


zeroOne <- function(im_out, thresh){
  
size <-round(ncol(im_out)*nrow(im_out)/thresh)
elements <- tail(sort(im_out), size)
minVal <- elements[1]
im_out[(im_out < minVal)] <- 0
im_out[(im_out > minVal)] <- 1

im_out

}


im_out <- zeroOne(im_out, thresh)
display(im_out)
