library(EBImage)

imageIn <- readImage("/Users/robinyancey/desktop/001_F.jpg")

# display(imageIn)
dim3 <- 3 # 3 for color and 1 for b/w

# Q and Nf varies based on amount/size of copied region
Q <- 47 # JPEG Quality factor: found by trial and error but might be command line arg to print this from image
Nf <- 30 # should print row/column pairs of distances greater than Nf (adjust to print # pairs = # copied regions)
Nd <- 16 # this is the minimum offset distance of matching block

c <- 0 # color (0-255) of copied regions in output image

par <- T # if true then image is split in half for parallel dct matrix computation, if false it runs in serial (slower)
# note: parallel version requires packages: partools


dctCP<-function(imageIn,c,par,dim3,Nf,Nd=2,Q=50){
  #require('dtt')
  # these will be made constants after more tests
  scale <-10 # 10: this DCT function produces very high variance so scale=10 and variant=4 (or NO matches will be found)
  boxside <- 16 # 16: just like it says in the papers the box size needs to be 16 (or number of matches gets VERY large)
  # note that images are read in differently (depending on function/package)
  width <- nrow(imageIn) #dim1
  height<- ncol(imageIn)
  
  imageInCopy <-imageIn
  # add "if dim3" here
  if (dim3 == 3){
    # normal way to convert to black and white
    red.weight<- .2989; green.weight <- .587; blue.weight <- 0.114
    imageIn <- red.weight * imageData(imageIn)[,,1] + green.weight * imageData(imageIn)[,,2] + blue.weight  * imageData(imageIn)[,,3]}
    imageIn <- round(255*imageIn[1:width,1:height])
  if (dim3 == 1){imageInCopy<-array(imageInCopy,dim=c(width,height,3))}
    
  # (16-by-16) JPEG Chrominance Quantization Matrix (Luminance table didn’t work w/ any Q factors I tested)
  T <- matrix(99,boxside,boxside) 
  T[1:4,1:4]<-c(17, 18, 24, 47, 18, 21, 26, 66, 24, 26, 56, 99, 47, 66, 99, 99)
  
  # IJG scaling:
  if (Q>=50){
    S <- 200-(2*Q)}
  if (Q<50){S <- 5000/Q}
  T <- round((((T*S)+50)/100))
  
  dctMatrix <- function(imageIn){
    require('dtt')
    imageIn <-as.matrix(imageIn) # distribsplit changes it to dataframe (which is not acceptable by dvtt)
    boxside <- 16
    width <- nrow(imageIn)
    height<- ncol(imageIn)
    # in parallel we will miss boxside - 1 blocks per worker in current form
    size <- (width-boxside+1) * (height-boxside+1) 
    testdct <- matrix(0, nrow=size, ncol=((boxside^2) + 2) ) # dct with loactions
    
    k <- 1
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
    testdct
  } 
  
  ### Parallel:
  if (par){
    require('parallel')
    require('partools')
    cls <-makeCluster(2)
    clusterExport(cls,'dctMatrix', envir=environment())
    clusterExport(cls, varlist=c("T", "scale"), envir=environment())
    clusterEvalQ(cls, require('dtt'))
    distribsplit(cls, 'imageIn')
    print(system.time(testdctC <- clusterEvalQ(cls, testdctC <- dctMatrix(imageIn))))
    # need to correct i, j locations so add height/(cls[[n]]$rank-1) to i 
    testdctC[[2]][,((boxside^2) + 1)] <- testdctC[[2]][,((boxside^2) + 1)] + (height/2) 
    # combine all testdct chunks to make new large testdct
    testdct<-rbind(testdctC[[1]],testdctC[[2]])
    # rewrite size since was divided on cls (shorter since misses rows of overlapping boxes)
    size <- dim(testdct)[1]
    }
  
  ### Serial:
  if (!par){
    print(system.time(testdct <- dctMatrix(imageIn)))}
  
  # sort lexographically or by all columns (accept location columns)
  testdct <- testdct[do.call(order, lapply(1:(boxside^2), function(i) testdct[,i])),]
  
  dctLocations <- testdct[,((boxside^2)+1):((boxside^2)+2)] #coefficients only
  testdct <- testdct[,1:(boxside^2)] #locations only
  
  
  numFound <- 1 # counts matching rows 
  distancePair <- matrix(0, size, 2) # just make biggest possible
  pairLoc1 <- matrix(0, size, 2) # these hold the just box locations of pairs with non-adjacent offset
  pairLoc2 <- matrix(0, size, 2) 
  pairFrequencies <- matrix(0, max(height, width), max(height, width))
  
  print(system.time(
    for (i in 1:(size-1)){
      if (all(testdct[i,] == testdct[(i+1),])){
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
  # print the frequencies above the threshold by running line by line in function:
  # "which(pairFrequencies > Nf, arr.ind=TRUE)"
  freqPairs <- which(pairFrequencies > Nf, arr.ind=TRUE) 
  if (nrow(freqPairs)>=1){
    print(freqPairs)}
  
  print(system.time(
    
    for (i in 1:(numFound-1)){
      for (j in 1:nrow(freqPairs)){
        if (distancePair[i,] == freqPairs[j,]){
          imageInCopy[pairLoc1[i,1]:(pairLoc1[i,1]+boxside - 1), pairLoc1[i,2]:(pairLoc1[i,2]+boxside - 1),1:dim3] = c
          imageInCopy[pairLoc2[i,1]:(pairLoc2[i,1]+boxside - 1), pairLoc2[i,2]:(pairLoc2[i,2]+boxside - 1),1:dim3] = c
        }
      }
    }
  ))
  imageInCopy
}


imageInCopy<-dctCP(imageIn,c,par,dim3,Nf,Nd,Q)
# need to rerun this line to refresh image:
display(imageInCopy)
