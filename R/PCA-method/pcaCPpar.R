library(EBImage)
# for pca boxside 8, freq>50, and 1 column of pca features works
imageIn <- readImage("/Users/robinyancey/desktop/001_F.jpg")

#display(imageIn)

Nf <- 90 # the program prints row/column pairs of offset frequencies greater than Nf 
# adjust Nf up or down so that the # pairs printed  = # copied regions, larger copied regions should have higher Nf
Nd <- 150 # minimum offset distance of the matching block: increase as much as possible to remove any false
# positives (boxesnot in the copied region)

# user choices
dim3 <- 3 # 3 for color and 1 for b/w input image
c <- 0 # color (0-255) of copied regions in output image
par <- 2 # if 2,4,8, or 16 then image is split in chunks for parallel pca matrix computation, if 0 it runs in serial
# for 512x512 image:  seconds if par=,  seconds if par=0 
# note1: parallel version requires partools package 
# note2: higher # of parallel clusters could result in a false positive occuring in the splitting line (see test images)

pcaCP<-function(imageIn,c=0,par=8,dim3=3,Nf=10,Nd=2,Q=50){

  boxside <- 8
  
  # note that images are read in differently (depending on function/package)
  width <- nrow(imageIn) 
  height<- ncol(imageIn)
  
  imageInCopy <-imageIn #we want to work witha b/w image
  
  if (dim3 == 3){
    # standard way to convert to black and white
    red.weight<- .2989; green.weight <- .587; blue.weight <- 0.114
    imageIn <- red.weight * imageData(imageIn)[,,1] + green.weight * imageData(imageIn)[,,2] + blue.weight  * imageData(imageIn)[,,3]}
  
  imageIn <- round(imageIn[1:width,1:height]) # removed 255 here for PCA
  # add a 3rd dimension to color on if b/w input image:
  if (dim3 == 1){imageInCopy<-array(imageInCopy,dim=c(width,height,3))}
  

  pcaMatrix <- function(imageIn){
    require('scales')
    imageIn <-as.matrix(imageIn) # distribsplit changes it to dataframe (which is not acceptable by dvtt)
    boxside <- 8
    width <- nrow(imageIn)
    height<- ncol(imageIn)
    # in parallel we will miss boxside - 1 blocks per worker in current form
    size <- (width-boxside+1) * (height-boxside+1) 
    testpca <- matrix(0, nrow=size, ncol=((boxside^2) + 2)) # dct with loactions
    k <- 1
    for (i in 1:(width-boxside+1)){
      for (j in 1:(height-boxside+1)){
        endw <- i+(boxside-1)
        endh <- j+(boxside-1) 

        pca <- prcomp(imageIn[i:endw,j:endh])
        features <- pca$rotation[,1] # only 1 is best
        compact <- t(features) %*% t(imageIn[i:endw,j:endh])
        block<- round(rescale(t(features %*% compact), to = c(0, 255))) #255 (increasing doesnt do anything)
        
        block <- t(as.vector(block))
        testpca[k,] <- c(block, i, j)
        k <- k+1
      }
    }
    testpca
  } 
  
  ### Parallel:
  if (par>0){
    require('partools') 
    cls <-makeCluster(par)
    
    # this will make the parallel version not miss any boxes (when finished)
    # j <- 1 # start index in image chunk
    # rowsevn <- round(width/length(cls)) # approx chunk size
    # m <- matrix(0,nrow=(rowsevn+boxside-1),ncol=height) 
    # for (i in 1:(length(cls)-1)){
    #   k <- j + (rowsevn+(boxside-1))
    #   m[,1:height]<-image[j:k,]
    #   clusterExport(cls, 'm', envir=environment())# should only be specific node
    #   j <- k - (boxside-1)
    # }
    # m[,1:height]<-image[j:width,]
    # clusterExport(cls, 'm', envir=environment()) # highest node
    
    
    clusterExport(cls, 'pcaMatrix', envir=environment())
    distribsplit(cls, 'imageIn')
    testpcaC <- clusterEvalQ(cls, testpcaC <- pcaMatrix(imageIn))
    # need to correct i, j locations so add height/(cls[[n]]$rank-1) to i 
    for (i in 2:length(cls)){ 
      testpcaC[[i]][,((boxside^2) + 1)] <- testpcaC[[i]][,((boxside^2) + 1)] + (i-1)*(height/length(cls)) 
    }
    # combine all testpcaC chunks to make new large testpca
    testpca<-do.call('rbind',testpcaC) 
  }
  
  ### Serial:
  if (par==0){
    testpca <- pcaMatrix(imageIn)}
  
  # rewrite size since was divided on cls (shorter since misses rows of overlapping boxes)
  size <- dim(testpca)[1]
  
  # sort lexographically by all columns (accept location columns)
  # Dr. Matloff note: this is not taking any time (dctMatrix() takes all the time)
  testpca <- testpca[do.call(order, lapply(1:(boxside^2), function(i) testpca[,i])),]
  
  pcaLocations <- testpca[,((boxside^2)+1):((boxside^2)+2)] # locations only
  testpca <- testpca[,1:(boxside^2)] #coefficients only
  
  numFound <- 1 # counts matching rows 
  distancePair <- matrix(0, size, 2) # just make biggest possible
  pairLoc1 <- matrix(0, size, 2) # these hold the just box locations of pairs with > Nd offset
  pairLoc2 <- matrix(0, size, 2) 
  pairFrequencies <- matrix(0, width, height)
  for (i in 1:(size-1)){
    if (all(testpca[i,] == testpca[(i+1),])){
      distancePair[numFound,1] <- abs(pcaLocations[i,1] - pcaLocations[(i+1),1]) # row offset
      distancePair[numFound,2] <- abs(pcaLocations[i,2] - pcaLocations[(i+1),2]) # column offset
      if (sqrt(distancePair[numFound,1]^2+distancePair[numFound,1]^2)>Nd){ # absolute distance between matching blocks
        pairLoc1[numFound,] <- pcaLocations[i,] # record this location
        pairLoc2[numFound,] <- pcaLocations[(i+1),] # increment matrix counting offset frequencies:
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


print(system.time(imageInCopy<-pcaCP(imageIn,c,par,dim3,Nf,Nd,Q)))
# need to rerun this line to refresh image:
display(imageInCopy)
