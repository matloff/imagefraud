library(EBImage)

imageIn <- readImage("/Users/robinyancey/desktop/001_F.jpg")


#display(imageIn)

Q <- 87 #49 #47 first find the JPEG Quality factor: can be found by trial and error but might be command line 
# # arg to print this from image (researching this)
Nf <- 30 #4 #30 the program prints row/column pairs of offset frequencies greater than Nf 
# # adjust Nf up or down so that the # pairs printed  = # copied regions, larger copied regions should have higher Nf
Nd <- 128 #8 minimum offset distance of the matching block: increase as much as possible to remove any false positives


# user choices
boxside <- 8 # use 8 for the luminence JPEG Q-matrix or 16 for chrominance (16 is slower)
c <- 0 # color (0-255) of copied regions in output image
par <- 4 # if 2,4,8, or 16 then image is split in chunks for parallel dct matrix computation, if 0 it runs in serial (much slower)

dctCP<-function(imageIn,c=0,par=4,Nf=10,Nd=2,Q=50,boxside=8){  
  
  # note that images are read in differently (depending on function/package)
  width <- nrow(imageIn) 
  height<- ncol(imageIn)
  dim <- dim(imageIn)
  
  imageInCopy <-imageIn # to work with a b/w image
  
  if (dim[3] == 3){
    # standard way to convert to black and white
    red.weight<- .2989; green.weight <- .587; blue.weight <- 0.114
    imageIn <- red.weight * imageData(imageIn)[,,1] + green.weight * imageData(imageIn)[,,2] + blue.weight  * imageData(imageIn)[,,3]}
  
  # add a 3rd dimension to color on if b/w input image:
  if (is.na(dim[3])){imageInCopy<-array(imageInCopy,dim=c(width,height,3))}
  
  if (boxside == 16 || 32){  
    # JPEG Chrominance Quantization Matrix 
    T <- matrix(99,boxside,boxside) # (16-by-16) form
    T[1:4,1:4]<-c(17, 18, 24, 47, 18, 21, 26, 66, 24, 26, 56, 99, 47, 66, 99, 99)}
  
  if  (boxside == 8){
    # JPEG Luminence Quantization Matrix 
    T <- c(16, 11, 10, 16, 24, 40, 51, 61, 12, 12, 14, 19, 26, 58, 60, 55,
           14, 13, 16, 24, 40, 57, 69, 56, 14, 17, 22, 29, 51, 87, 80, 62,
           18, 22, 37, 56, 68, 109, 103, 77, 24, 35, 55, 64, 81, 104, 113, 92,
           49, 64, 78, 87, 103, 121, 120, 101, 72, 92, 95, 98, 112, 100, 103, 99)
  }
  
  # IJG scaling:
  if (Q>=50){
    S <- 200-(2*Q)}
  if (Q<50){S <- 5000/Q}
  T <- round((((T*S)+50)/100))
  
  dctMatrix2 <- function(imageIn){
    require('dtt')
    imageIn <-as.matrix(imageIn) # distribsplit changes it to dataframe (which is not acceptable by dvtt)
    width <- nrow(imageIn)
    height<- ncol(imageIn)
    # in parallel we will miss boxside - 1 blocks per worker in current form
    size <- (width-boxside+1) * (height-boxside+1) 
    testdct <- matrix(0, nrow=size, ncol=((boxside^2) + 2)) # dct with loactions
    k <- 1
    for (i in 1:(width-boxside+1)){
      for (j in 1:(height-boxside+1)){
        endw <- i+(boxside-1)
        endh <- j+(boxside-1) 
        block <- round(mvdtt(imageIn[i:endw,j:endh], type='dct')/T)
        block <- t(as.vector(block))
        testdct[k,] <- c(block, i, j)
        k <- k+1
      }
    }
    testdct
  } 
  
  ### Parallel:
  if (par>0){
    require('partools') 
    cls <-makeCluster(par)
    clusterExport(cls, varlist=c('dctMatrix2',"T", "boxside", "height"), envir=environment())
    clusterEvalQ(cls, require('dtt'))
    distribsplit(cls, 'imageIn')
    
    rowseven <- round(width/length(cls))
    imageIn2 <- imageIn[(rowseven+1):(rowseven+(boxside-1)),]
    if (par > 2){
    for (i in 2:(par-1)){
      j <- rowseven*i+1
      k <- rowseven*i+(boxside-1)
      imageIn2<-rbind(imageIn2, imageIn[j:k,])}}

    imageIn2 <- rbind(imageIn2, matrix(0, (boxside-1), dim(imageIn)[2]))
    distribsplit(cls, 'imageIn2')
    clusterEvalQ(cls, imageIn <- rbind(imageIn, imageIn2))
    clusterEvalQ(cls, imageIn <- imageIn[apply(imageIn[,-1], 1, function(x) !all(x==0)),])
    # # #
    
    testdctC <- clusterEvalQ(cls, testdctC <- dctMatrix2(imageIn))
    
    # need to correct i, j locations so add height/(cls[[n]]$rank-1) to i 
    for (i in 2:length(cls)){ 
      testdctC[[i]][,((boxside^2) + 1)] <- testdctC[[i]][,((boxside^2) + 1)] + (i-1)*(height/length(cls)) 
    }
    # combine all testdctC chunks to make new large testdct
    testdct<-do.call('rbind',testdctC) 
  }
  
  ### Serial:
  if (par==0){
    testdct <- dctMatrix2(imageIn)}
  
  # rewrite size since was divided on cls (shorter since misses rows of overlapping boxes)
  size <- dim(testdct)[1]
  
  testdct <- testdct[do.call(order, lapply(1:(boxside^2), function(i) testdct[,i])),]
  
  dctLocations <- testdct[,((boxside^2)+1):((boxside^2)+2)] # locations only
  testdct <- testdct[,1:(boxside^2)] #coefficients only
  
  numFound <- 1 # counts matching rows 
  distancePair <- matrix(0, size, 2) # just make biggest possible
  pairLoc1 <- matrix(0, size, 2) # these hold the just box locations of pairs with > Nd offset
  pairLoc2 <- matrix(0, size, 2) 
  pairFrequencies <- matrix(0, width, height)
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
      if (round(distancePair[i,]) == freqPairs[j,]){ # color matching boxes
        imageInCopy[round(pairLoc1[i,1]):(round(pairLoc1[i,1])+boxside - 1), round(pairLoc1[i,2]):(round(pairLoc1[i,2])+boxside - 1),1:3] = c
        imageInCopy[round(pairLoc2[i,1]):(round(pairLoc2[i,1])+boxside - 1), round(pairLoc2[i,2]):(round(pairLoc2[i,2])+boxside - 1),1:3] = c
      }
    }
  }
  
  imageInCopy
}


print(system.time(imageInCopy<-dctCP(imageIn,c,par,Nf,Nd,Q,boxside)))
# need to rerun this line to refresh image:
display(imageInCopy)
