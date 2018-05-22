library(EBImage)

imageIn <- readImage("/Users/robinyancey/desktop/001_F.jpg")

#display(imageIn)

Q <- 47 # first find the JPEG Quality factor: can be found by trial and error but might be command line 
# arg to print this from image (researching this)
Nf <- 30 # the program prints row/column pairs of offset frequencies greater than Nf 
# adjust Nf up or down so that the # pairs printed  = # copied regions, larger copied regions should have higher Nf
Nd <- 8 # minimum offset distance of the matching block: increase as much as possible to remove any false
# positives (boxesnot in the copied region)

# user choices
dim3 <- 3 # 3 for color and 1 for b/w input image
c <- 0 # color (0-255) of copied regions in output image
par <- 0 # if true then image is split in half for parallel dct matrix computation, if false it runs in serial (slower)
# note: parallel version requires packages: partools


dctCP<-function(imageIn,c=0,par,dim3=3,Nf=10,Nd=2,Q=50){
  # these should be made constants after more tests
  scale <-10 # 10: this DCT function produces very high variance so scale=10 and variant=4 (or NO matches will be found)
  boxside <- 16 # 16: just like it says in the papers the box size needs to be 16 (or number of matches gets VERY large)
  
  # note that images are read in differently (depending on function/package)
  width <- nrow(imageIn) 
  height<- ncol(imageIn)
  
  imageInCopy <-imageIn
  # add "if dim3" here
  if (dim3 == 3){
    # standard way to convert to black and white
    red.weight<- .2989; green.weight <- .587; blue.weight <- 0.114
    imageIn <- red.weight * imageData(imageIn)[,,1] + green.weight * imageData(imageIn)[,,2] + blue.weight  * imageData(imageIn)[,,3]}
  # scale used for   
  imageIn <- round(255*imageIn[1:width,1:height])
  # add a 3rd dimension to color on if b/w input image:
  if (dim3 == 1){imageInCopy<-array(imageInCopy,dim=c(width,height,3))}
    
  # JPEG Chrominance Quantization Matrix 
  T <- matrix(99,boxside,boxside) # (16-by-16) form
  T[1:4,1:4]<-c(17, 18, 24, 47, 18, 21, 26, 66, 24, 26, 56, 99, 47, 66, 99, 99)
  
  # IJG scaling:
  if (Q>=50){
    S <- 200-(2*Q)}
  if (Q<50){S <- 5000/Q}
  T <- round((((T*S)+50)/100))
  
  dctMatrix <- function(imageIn){
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
  if (par==1){
    require('partools') 
    require('dtt')
    cls <-makeCluster(2)
    clusterExport(cls,'dctMatrix', envir=environment())
    clusterExport(cls, varlist=c("T", "scale"), envir=environment())
    clusterEvalQ(cls, require('dtt'))
    distribsplit(cls, 'imageIn')
    testdctC <- clusterEvalQ(cls, testdctC <- dctMatrix(imageIn))
    # need to correct i, j locations so add height/(cls[[n]]$rank-1) to i 
    testdctC[[2]][,((boxside^2) + 1)] <- testdctC[[2]][,((boxside^2) + 1)] + (height/2) 
    # combine all testdct chunks to make new large testdct
    testdct<-rbind(testdctC[[1]],testdctC[[2]])
    # rewrite size since was divided on cls (shorter since misses rows of overlapping boxes)
    size <- dim(testdct)[1]
    }
  
  ### Serial:
  if (par==0){
    testdct <- dctMatrix(imageIn)}
  
  # sort lexographically or by all columns (accept location columns)
  # Dr. Matloff note: this is not taking any time (dctMatrix() takes all the time)
  testdct <- testdct[do.call(order, lapply(1:(boxside^2), function(i) testdct[,i])),]
  
  dctLocations <- testdct[,((boxside^2)+1):((boxside^2)+2)] # locations only
  testdct <- testdct[,1:(boxside^2)] #coefficients only
  
  numFound <- 1 # counts matching rows 
  distancePair <- matrix(0, size, 2) # just make biggest possible
  pairLoc1 <- matrix(0, size, 2) # these hold the just box locations of pairs with > Nd offset
  pairLoc2 <- matrix(0, size, 2) 
  pairFrequencies <- matrix(0, max(height, width), max(height, width))
  
    for (i in 1:(size-1)){
      if (all(testdct[i,] == testdct[(i+1),])){
        distancePair[numFound,1] <- abs(dctLocations[i,1] - dctLocations[(i+1),1]) # row offset
        distancePair[numFound,2] <- abs(dctLocations[i,2] - dctLocations[(i+1),2]) # column offset
        if (sqrt(distancePair[numFound,1]^2+distancePair[numFound,1]^2)>Nd){ # absolute distance between matching blocks
          pairLoc1[numFound,] <- dctLocations[i,] # record this location
          pairLoc2[numFound,] <- dctLocations[(i+1),] # increment matrix counting offset frequencies:
          pairFrequencies[distancePair[numFound,1], distancePair[numFound,2]] <- pairFrequencies[distancePair[numFound,1], distancePair[numFound,2]] + 1
          numFound <- numFound + 1
        }
      }
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


print(system.time(imageInCopy<-dctCP(imageIn,c,par,dim3,Nf,Nd,Q)))
# need to rerun this line to refresh image:
display(imageInCopy)

