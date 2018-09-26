noiseImage<- function(imageIn){
  
  require(jpeg)
  require(waveslim)
  imageIn <- readImage(imageIn)
  
  # Y color space
  red.weight<- .2989; green.weight <- .587; blue.weight <- 0.114
  imageInY <- red.weight * imageData(imageIn)[,,1] + green.weight * imageData(imageIn)[,,2] + blue.weight  * imageData(imageIn)[,,3]
  imageInY <- imageInY*255
  
  boxside <- 8
  
  # wavelet tranform of image:
  dwt <- dwt.2d(imageInY, "d8", J = 1, boundary = "periodic")
  # correct to be divisible by boxside:
  dwt$LL1 <- dwt$LL1[1:(floor(nrow(dwt$LL1)/boxside)*boxside),1:(floor(ncol(dwt$LL1)/boxside)*boxside)]
  # create an array for all boxs that fit in image:
  box <- array(0,c(floor(nrow(dwt$LL1)/boxside),floor(ncol(dwt$LL1)/boxside),boxside^2))
  
  for (i in seq(1, (nrow(dwt$LL1)-1), by=boxside)){
    for (j in seq(1, (ncol(dwt$LL1)-1), by=boxside)){
      
      oneBox <- dwt$LL1[i:(i+boxside-1),j:(j+boxside-1)]
      box[((i-1)/boxside+1),((j-1)/boxside+1),] <- array(oneBox,c(1, 1, boxside^2))
      
    }
  }
  
 box <- abs(box)
 im_out <- apply(box, c(1,2), median)
 im_out <- t(im_out)

 require('OpenImageR')
 im_out <- resizeImage(im_out, dim(imageIn)[2], dim(imageIn)[1], method = "nearest")
 
 # thresh <- 0.065
 # im_out <- zeroOne(im_out, thresh)
 # 
 # display(im_out)
 
 im_out2 <-abs(im_out-max(im_out))
 im_out2
 
}
