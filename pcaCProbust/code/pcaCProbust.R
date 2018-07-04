library(EBImage)

# PLEASE NOTE best so far but this can be improved to be exact with a few tweaks I am pretty sure 
# (probably Thursday Saturday) I will do these and will parallelize

imageIn <- readImage("/Users/robinyancey/desktop/copied.jpg")

#display(imageIn)

Nf <- 6 #20 #87 the program prints row/column pairs of offset frequencies greater than Nf 
# adjust Nf up or down so that the # pairs printed  = # copied regions, larger copied regions should have higher Nf
Nd <- 250  #280 # minimum offset distance of the matching block: increase as much as possible to remove any false
# positives (boxesnot in the copied region)

# user choices
dim3 <- 3 # 3 for color and 1 for b/w input image
c <- 0 # color (0-255) of copied regions in output image

# (will implement in parallel for this too very soon)
par <- 0 # if 2,4,8, or 16 then image is split in chunks for parallel pca matrix computation, if 0 it runs in serial
# for 512x512 image:  seconds if par=,  seconds if par=0 
# note1: parallel version requires partools package 
# note2: higher # of parallel clusters could result in a false positive occuring in the splitting line (see test images)

pcaCProbust<-function(imageIn,c=0,par=8,dim3=3,Nf=10,Nd=2,boxside=32){

  # note that images are read in differently (depending on function/package)
  width <- nrow(imageIn) 
  height<- ncol(imageIn)
  
  imageInCopy <-imageIn #we want to work witha b/w image
 
    # standard way to convert to black and white
    red.weight<- .2989; green.weight <- .587; blue.weight <- 0.114
    imageIn <- red.weight * imageData(imageIn)[,,1] + green.weight * imageData(imageIn)[,,2] + blue.weight  * imageData(imageIn)[,,3]
    # save colors for improved method
    imageInR <- imageData(imageInCopy)[,,1]
    imageInG <- imageData(imageInCopy)[,,2]
    imageInB <- imageData(imageInCopy)[,,3]


  pcaMatrix <- function(imageIn,imageInR,imageInG,imageInB){
    require('scales')
    imageIn <-as.matrix(imageIn) # distribsplit changes it to dataframe (which is not acceptable by dvtt)
    imageInR <-as.matrix(imageInR)
    imageInG <-as.matrix(imageInG)
    imageInB <-as.matrix(imageInB)
    width <- nrow(imageIn)
    height<- ncol(imageIn)
    
    size <- (width-boxside+1) * (height-boxside+1) 

    testpca <- matrix(0, nrow=size, ncol=((boxside^2)+9)) 
    k <- 1
    for (i in 1:(width-boxside+1)){
      for (j in 1:(height-boxside+1)){
        endw <- i+(boxside-1)
        endh <- j+(boxside-1)
        
        pca <- prcomp(imageIn[i:endw,j:endh])
        features <- pca$rotation[,1]
        compact <- t(features) %*% t(imageIn[i:endw,j:endh])
        block <- round(rescale(t(features %*% compact), to = c(0, 255)))
        
        # NEW NEW NEW

        sumRed <- sum(imageInR[i:endw,j:endh])
        sumGreen <- sum(imageInG[i:endw,j:endh])
        sumBlue <- sum(imageInB[i:endw,j:endh])
        
        sumBlock <- boxside * boxside
        sumRed <- sumRed / (sumBlock)  
        sumGreen <- sumGreen / (sumBlock)
        sumBlue <- sumBlue / (sumBlock)

        characteristics = c(sumRed,sumGreen,sumBlue)
        
        c4p1 <- 0; c4p2 <- 0
        c5p1 <- 0; c5p2 <- 0
        c6p1 <- 0; c6p2 <- 0
        c7p1 <- 0; c7p2 <- 0
        
        #c4
        c4p1 <- sum(imageIn[i:endw, j:((j+boxside / 2)-1)])  
        c4p2 <- sum(imageIn[i:endw, (j+boxside / 2):endh]) 
        
        #c5
        c5p1 <- sum(imageIn[i:((i+boxside / 2)-1),j:endh])  
        c5p2 <- sum(imageIn[(i+boxside / 2):endw,j:endh]) 
        
        
        for (yCoordinate in 1:boxside){ 
          for (xCoordinate in 1:boxside){
            
       #c6
        if ((xCoordinate) - (yCoordinate) >= 0){
          c6p1 <- c6p1 + imageIn[i+xCoordinate-1, j+yCoordinate-1]}
        else{c6p2 <- c6p2 + imageIn[i+xCoordinate-1, j+yCoordinate-1]}

       #c7
        if ((xCoordinate) + (yCoordinate)  <= boxside){
          c7p1 <- c7p1 + imageIn[i+xCoordinate-1, j+yCoordinate-1]}
        else{c7p2 <- c7p2 + imageIn[i+xCoordinate-1, j+yCoordinate-1]}
          }
        }
        characteristics <- c(characteristics, (c4p1 / (c4p1 + c4p2)))
        characteristics <- c(characteristics, (c5p1 / (c5p1 + c5p2)))
        characteristics <- c(characteristics, (c6p1 / (c6p1 + c6p2)))
        characteristics <- c(characteristics, (c7p1 / (c7p1 + c7p2)))
        
        block <- t(as.vector(block))
        testpca[k,] <- c(characteristics, block, i, j)
        #testpca[k,] <- c(block, characteristics, i, j)
        # (end) NEW NEW NEW

        k <- k+1
      }
    }
    testpca
  } 
  
  
  ### Serial:
  if (par==0){
    testpca <- pcaMatrix(imageIn,imageInR,imageInG,imageInB)}
  
  # rewrite size since was divided on cls (shorter since misses rows of overlapping boxes)
  size <- dim(testpca)[1]
  
  # sort lexographically by all columns (accept location columns)
  
  # NEW NEW NEW 2

  # sort by PCA features
  #testpca <- testpca[do.call(order, lapply(8:((boxside^2)+7), function(i) testpca[,i])),]
  #testpca <- testpca[do.call(order, lapply(1:((boxside^2)+7), function(i) testpca[,i])),]
  testpca <- testpca[do.call(order, lapply(1:7, function(i) testpca[,i])),]
  pcaLocations <- testpca[,((boxside^2)+8):((boxside^2)+9)] # locations only
  testpcaP <- testpca[,8:(7+(boxside^2))] # pca coefficients only
  testpca <- testpca[,1:7] # coefficients only
  #testpcaP <- testpca[,1:(boxside^2)] # pca coefficients only
  #testpca <- testpca[,((boxside^2)+1):((boxside^2)+8)] # coefficients only
  
  numFound <- 1 # counts matching rows 
  distancePair <- matrix(0, size, 2) # just make biggest possible
  pairLoc1 <- matrix(0, size, 2) # these hold the just box locations of pairs with > Nd offset
  pairLoc2 <- matrix(0, size, 2) 
  pairFrequencies <- matrix(0, width, height)
  for (i in 1:(size-1)){
    
    # only check first ten features of PCA (found by trial and error)
    if (all(testpcaP[i,1:10] == testpcaP[(i+1),1:10])){
    
    # check for AWGN (added white Guassian noise)
        # averagge of each color per block
      if (abs(testpca[i,1] - testpca[(i+1),1]) < 1.80){#1
      if (abs(testpca[i,1] - testpca[(i+1),1]) < 1.80){#2
      if (abs(testpca[i,2] - testpca[(i+1),2]) < 1.80){#3
        # intensity ratio per block
      if (abs(testpca[i,3] - testpca[(i+1),3]) < 0.0125){#4
      if (abs(testpca[i,4] - testpca[(i+1),4]) < 0.0125){#5
      if (abs(testpca[i,5] - testpca[(i+1),5]) < 0.0125){#6
      if (abs(testpca[i,6] - testpca[(i+1),6]) < 0.0125){#7
      if (abs(testpca[i,1] - testpca[(i+1),1]) + abs(testpca[i,2] - testpca[(i+1),2]) + abs(testpca[i,3] - testpca[(i+1),3]) < 2.80){#8
      if (abs(testpca[i,4] - testpca[(i+1),4]) + abs(testpca[i,5] - testpca[(i+1),5]) + abs(testpca[i,6] - testpca[(i+1),6]) + abs(testpca[i,7] - testpca[(i+1),7]) < 0.02){#9
            
    # (end) NEW NEW NEW 2
    
      distancePair[numFound,1] <- abs(pcaLocations[i,1] - pcaLocations[(i+1),1]) # row offset
      distancePair[numFound,2] <- abs(pcaLocations[i,2] - pcaLocations[(i+1),2]) # column offset
      if (sqrt(distancePair[numFound,1]^2+distancePair[numFound,1]^2)>Nd){ # absolute distance between matching blocks
        pairLoc1[numFound,] <- pcaLocations[i,] # record this location
        pairLoc2[numFound,] <- pcaLocations[(i+1),] # increment matrix counting offset frequencies:
        pairFrequencies[distancePair[numFound,1], distancePair[numFound,2]] <- pairFrequencies[distancePair[numFound,1], distancePair[numFound,2]] + 1
        numFound <- numFound + 1
      }
      }#1
      }#2
      }#3
      }#4
      }#5
      }#6
      }#7
      }#8
      }#9
}#for
  }  
  # print the frequencies above the threshold by running line by line in function:
  # "which(pairFrequencies > Nf, arr.ind=TRUE)" and changing Nf to check # pairs
  freqPairs <- which(pairFrequencies > Nf, arr.ind=TRUE) 
  if (nrow(freqPairs)>=1){
    print(freqPairs)}
  
  for (i in 1:(numFound-1)){
    for (j in 1:nrow(freqPairs)){
      if (distancePair[i,] == freqPairs[j,]){ # color matching boxes
        imageInCopy[pairLoc1[i,1]:(pairLoc1[i,1]+boxside - 1), pairLoc1[i,2]:(pairLoc1[i,2]+boxside - 1),1:dim3] = c
        imageInCopy[pairLoc2[i,1]:(pairLoc2[i,1]+boxside - 1), pairLoc2[i,2]:(pairLoc2[i,2]+boxside - 1),1:dim3] = c
      }
    }
  }
  
  imageInCopy
}


print(system.time(imageInCopy<-pcaCProbust(imageIn,c,par,dim3,Nf,Nd)))
# need to rerun this line to refresh image:
display(imageInCopy)
