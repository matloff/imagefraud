library(jpeg)
library(EBImage)
library(scales)

# test image
pic <- Image(flip(readImage("/Users/robinyancey/desktop/cat.jpg")))


# pca on black and white
red.weigth   <- .2989; green.weigth <- .587; blue.weigth  <- 0.114
pic <- red.weigth * imageData(pic)[,,1] + green.weigth * imageData(pic)[,,2] + blue.weigth  * imageData(pic)[,,3]
# image(pic, col = grey(seq(0, 1, length = 256)))

# fake copyingg some of the image
pic[200:230,101:180] <- pic[100:130,1:80]

# pca on each color
# pic <- pic[,,1]
# g <- pic[,,2]
# b <- pic[,,3]

width<-ncol(pic)
height<-nrow(pic)
approx <- matrix(nrow = height, ncol = width)
boxside <- 8
thresh <- 16

size <- (width-boxside+1)*(height-boxside+1)
test <- matrix(nrow = size, ncol = (boxside^2) + 2)
Nd <-matrix(nrow=size,ncol=1)
q <- 256
k <- 1
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
      test[k,1:boxside^2] <- as.vector(round(rescale(approx[j:endh,i:endw], to = c(0, q))))
      # save location
      test[k,((boxside^2)+1):((boxside^2)+2)] <- c(i,j)
      k <- k + 1
    }}
  
))

# sort by all columns
print(system.time(test <- test[do.call(order, lapply(1:(boxside^2), function(i) test[,i])),]))
#
print(system.time(
# calculate offset distance and frequency of the point i,j from another for each row of test
for (i in 1:(size-51)){
  for (j in (i+1):(i+50)) {
    # if box is match
  if (all(test[i,1:(boxside^2)] == test[j,1:(boxside^2)]) && (j != i)){
    d1 <- test[j,((boxside^2)+1)] - test[i,((boxside^2)+1)]
    d2 <- test[j,((boxside^2)+2)] - test[i,((boxside^2)+2)]
    Nd[i,1] <- sqrt(d1^2 + d2^2)
    # if distance less than threshold it doesnt count as copied
    if (Nd[i] < thresh) {
      Nd[i] <- 0
    
    }
    else {
      # make copied segments black 
      approx[test[i,((boxside^2)+1)],test[i,((boxside^2)+2)]] <- 0

    }

  }
    }
      }
))
# then we need to make sure that the frequency of matching boxes is greater than the threshold Nf
#f1m <- mode(round(Nd))
print(f1m)
image(approx, col = grey(seq(0, 1, length = 256)))
