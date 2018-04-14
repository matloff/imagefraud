library(jpeg)
library(EBImage)
library(scales)
# test image
pic <- Image(flip(readImage("/Users/robinyancey/desktop/cat.jpg")))

#pca on black and white
red.weigth   <- .2989; green.weigth <- .587; blue.weigth  <- 0.114
pic <- red.weigth * imageData(pic)[,,1] + green.weigth * imageData(pic)[,,2] + blue.weigth  * imageData(pic)[,,3]
#image(pic, col = grey(seq(0, 1, length = 256)))

# pca on each color
# pic <- pic[,,1]
# g <- pic[,,2]
# b <- pic[,,3]

width<-ncol(pic)
height<-nrow(pic)
approx <- matrix(nrow = height, ncol = width)
boxside <- 8
size <- (height*width)/boxside
test <- matrix(nrow = size, ncol = (boxside^2) + 2)
q <- 256
print(system.time(

for (i in 1:(width-boxside+1)){
  for (j in 1:(height-boxside+1)){

  endw <- i+(boxside-1)
  endh <- j+(boxside-1)

  pca <- prcomp(pic[j:endh,i:endw])
  components <- 1:boxside
  features <- pca$rotation[,components]
  compact <- t(features) %*% t(pic[j:endh,i:endw])
  approx[j:endh,i:endw] <- t(features %*% compact)
  # prints approximated image
  # image(approx, col = grey(seq(0, 1, length = 256)))
  test[(i+j-1),1:boxside^2] <- as.vector(round(rescale(approx[j:endh,i:endw], to = c(0, q))))
  # save location
  test[(i+j-1),((boxside^2)+1):((boxside^2)+2)] <- c(i,j)
  }}

))
# sort by all columns
print(system.time(test <- test[do.call(order, lapply(1:(boxside^2), function(i) test[,i])),]))
