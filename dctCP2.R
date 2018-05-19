library(jpeg)
library(EBImage)
library('dtt')

# next five variable definitions will become user inputs to the detection function
imageIn <- readImage("/Users/robinyancey/desktop/copied.jpg")
#imageIn <- readJPEG("/Users/robinyancey/desktop/copied.jpg") # can use this too (but b&w and rotated)
#display(imageIn)
imageInCopy <-imageIn
dim3<-3
dim2<-512
dim1<-512

# Q and Nf varies based on amount/size of copied region
Q <- 63 #63 JPEG Quality factor: found by trial and error but might be command line arg to print this from image
Nf <- 110 #110 should print row/column pairs of distances greater than Nf (adjust to print number of copied regions)
Nd <- 8 #1 minimum offset of matching block(can be low as 2 for this image)

scale <-10 # 10: this DCT function produces very high variance so scale=10 (and variant=4) or NO matches will be found
boxside <- 16 #16: just like it says in the papers the box size needs to be 16 (or number of matches gets VERY large)

# normal way to convert to black and white
red.weigth   <- .2989; green.weigth <- .587; blue.weigth  <- 0.114
imageIn <- red.weigth * imageData(imageIn)[,,1] + green.weigth * imageData(imageIn)[,,2] + blue.weigth  * imageData(imageIn)[,,3]
imageIn <- round(255*imageIn[1:dim1,1:dim2])


T <- matrix(99,16,16) # (16-by-16) JPEG Chrominance Quantization Matrix (Luminance table didn’t work w/ any Q factors I tested)
T[1:4,1:4]<-c(17, 18, 24, 47, 18, 21, 26, 66, 24, 26, 56, 99, 47, 66, 99, 99) 
S <- 200-(2*Q)
T <- round((((T*S)+50)/100))

# note that images are read in differently (depending on function/package) but image is made square so this for now:
height <- nrow(imageIn)
width <- ncol(imageIn)

size <- (width-boxside+1) * (height-boxside+1)
testdct <- matrix(0, nrow=size, ncol=((boxside^2) + 2) ) # dct with loactions

k <- 1
print(system.time(
for (i in 1:(width-boxside+1)){
  for (j in 1:(height-boxside+1)){
    endw <- i+(boxside-1)
    endh <- j+(boxside-1)
    block <- round((mvdtt(imageIn[i:endw,j:endh], type='dct', variant=4)/scale)/T)
    block <- t(as.vector(block))
    testdct[k,] <- c(block, i, j)
    k <- k+1
  }
}
))

# sort lexographically or by all columns (accept location columns)
testdct <- testdct[do.call(order, lapply(1:(boxside^2), function(i) testdct[,i])),]

dctLocations <- testdct[,((boxside^2)+1):((boxside^2)+2)]
testdct <- testdct[,1:(boxside^2)]

numFound <- 1 # counts matching rows 
distancePair <- matrix(0, size, 2) # just make biggest possible
pairLoc1 <- matrix(0, size, 2) # these hold the just box locations of pairs with non-adjacent offset
pairLoc2 <- matrix(0, size, 2) 
pairFrequencies <- matrix(0, max(height+1, width+1), max(height+1, width+1))

print(system.time(
for (i in 1:(size-1)){
  if (all(testdct[i,] == testdct[(i+1),]) ){
    distancePair[numFound,1] <- abs(dctLocations[i,1] - dctLocations[(i+1),1]) # row offset
    distancePair[numFound,2] <- abs(dctLocations[i,2] - dctLocations[(i+1),2]) # column offset
    if (sqrt(distancePair[numFound,1]^2+distancePair[numFound,1]^2)>Nd){
      pairLoc1[numFound,] <- dctLocations[i,]
      pairLoc2[numFound,] <- dctLocations[(i+1),] # increment matrix counting offset frequencies:
      pairFrequencies[distancePair[numFound,1], distancePair[numFound,2]] <- pairFrequencies[distancePair[numFound,1], distancePair[numFound,2]] + 1
      numFound <- numFound + 1
    }
  }
} 
))
# print the frequencies above the threshold by running "which(pairFrequencies > Nf, arr.ind=TRUE)"
freqPairs <- which(pairFrequencies > Nf, arr.ind=TRUE) 
print(freqPairs)
print(system.time(

for (ii in 1:(numFound-1)){
  for (jj in 1:nrow(freqPairs)){
    if (distancePair[ii,] == freqPairs[jj,]){
    imageInCopy[pairLoc1[ii,1]:(pairLoc1[ii,1]+boxside - 1), pairLoc1[ii,2]:(pairLoc1[ii,2]+boxside - 1),1:dim] = 255
    imageInCopy[pairLoc2[ii,1]:(pairLoc2[ii,1]+boxside - 1), pairLoc2[ii,2]:(pairLoc2[ii,2]+boxside - 1),1:dim] = 255
    }
  }
}
))
# may need to rerun this line to show image
display(imageInCopy)
# A & A: if you dont want to diplay using EBimage package function just white-out the imageIn not copy and display that in B&W
