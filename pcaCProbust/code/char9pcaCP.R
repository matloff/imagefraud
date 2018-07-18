library(EBImage)

imageIn <- readImage("/Users/robinyancey/desktop/copied.jpg")

display(imageIn)

Nf <- 35 #20 #87 the program prints row/column pairs of offset frequencies greater than Nf 
# adjust Nf up or down so that the # pairs printed  = # copied regions, larger copied regions should have higher Nf
Nd <- 150  #280 # minimum offset distance of the matching block: increase as much as possible to remove any false
# positives (boxesnot in the copied region)

# user choices

c <- 0 # color (0-255) of copied regions in output image
boxside <- 32
pfeatures <- 8
# (will implement in parallel for this too very soon)
par <- 4 # if 2,4,8, or 16 then image is split in chunks for parallel pca matrix computation, if 0 it runs in serial
# for 512x512 image:  seconds if par=,  seconds if par=0 
# note1: parallel version requires partools package 
# note2: higher # of parallel clusters could result in a false positive occuring in the splitting line (see test images)
pcaCProbust<-function(imageIn,c=0,Nf=10,Nd=2,par=4,boxside=16,pfeatures=0){
  
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
  
  charMatrix <- function(imageIn){
    require('scales')
    imageIn <-as.matrix(imageIn) # distribsplit changes it to dataframe (which is not acceptable by dvtt)
    width <- nrow(imageIn)
    height<- ncol(imageIn)
    size <- (width-boxside+1) * (height-boxside+1) 
    
    testpca <- matrix(0, nrow=size, ncol=((boxside^2)+11)) 
    k <- 1
    for (i in 1:(width-boxside+1)){
      for (j in 1:(height-boxside+1)){
        endw <- i+(boxside-1)
        endh <- j+(boxside-1)
        
        pca <- prcomp(imageIn[i:endw,j:endh])
        features <- pca$rotation[,1]
        compact <- t(features) %*% t(imageIn[i:endw,j:endh])
        
        sumBlock <- boxside * boxside
        f1 <- sum(imageIn[i:endw,j:endh]) / (sumBlock)
        av2 <- sum(imageIn[i:((i+boxside / 2)-1), j:((j+boxside / 2)-1)])/ (sumBlock)
        av3 <- sum(imageIn[i:((i+boxside / 2)-1), (j+boxside / 2):endh])/ (sumBlock)
        av4 <- sum(imageIn[(i+boxside / 2):endw, (j+boxside / 2):endh])/ (sumBlock)
        av5 <- sum(imageIn[(i+boxside / 2):endw, j:((j+boxside / 2)-1)])/ (sumBlock)
        f2 <- av2/(4*f1)
        f3 <- av3/(4*f1)
        f4 <- av4/(4*f1)
        f5 <- av5/(4*f1)
        
        f6 <- av2-f1
        f7 <- av3-f1
        f8 <- av4-f1 
        f9 <- av5-f1 
        
        if (pfeatures == 0){
        
        block <- round(rescale(t(features %*% compact), to = c(0, 255)))
        characteristics <- round(rescale(c(f1, f2, f3, f4, f5, f6, f7, f8, f9), to = c(0, 255))) 
        block <- t(as.vector(block))
        testpca[k,] <- c(characteristics, block, i, j)}
        
        else{
        
        block <- t(features %*% compact)
        block <- t(as.vector(block))
        characteristics <- round(rescale(c(f1, f2, f3, f4, f5, f6, f7, f8, f9, block), to = c(0, 255))) 
        testpca[k,] <- c(characteristics, i, j)
        
        }
        k <- k+1
      }
    }
    testpca
  } 
  
  
  ### Parallel:
  if (par>0){
    require('partools') 
    cls <-makeCluster(par)
    clusterExport(cls, varlist=c('charMatrix',"pfeatures", "boxside","height"), envir=environment())
    distribsplit(cls, 'imageIn')
    #new :)
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

    testpcaC <- clusterEvalQ(cls, testpcaC <- charMatrix(imageIn))
    
    # need to correct i, j locations so add height/(cls[[n]]$rank-1) to i 
    for (i in 2:length(cls)){ 
      testpcaC[[i]][,((boxside^2) + 10)] <- testpcaC[[i]][,((boxside^2) + 10)] + (i-1)*(height/length(cls)) 
    }
    # combine all testdctC chunks to make new large testdct
    testpca<-do.call('rbind',testpcaC) 
  }
  
  
  ### Serial:
  if (par==0){
    testpca <- charMatrix(imageIn)}
  
  # rewrite size since was divided on cls (shorter since misses rows of overlapping boxes)
  size <- dim(testpca)[1]
  
  # sort lexographically by all columns (accept location columns)
  
  # sort by PCA features
  testpca <- testpca[do.call(order, lapply(10:((boxside^2)+9), function(i) testpca[,i])),]
  pcaLocations <- testpca[,((boxside^2)+10):((boxside^2)+11)] # locations only
  testpcaP <- testpca[,10:(9+(boxside^2))] # pca coefficients only
  testpca <- testpca[,1:9] # coefficients only
  
  
  
  numFound <- 1 # counts matching rows 
  distancePair <- matrix(0, size, 2) # just make biggest possible
  pairLoc1 <- matrix(0, size, 2) # these hold the just box locations of pairs with > Nd offset
  pairLoc2 <- matrix(0, size, 2) 
  pairFrequencies <- matrix(0, width, height)
  for (i in 1:(size-1)){
    
    # only check first ten features of PCA (found by trial and error)
    if (all(testpca[i,1:9] == testpca[(i+1),1:9])){
     if (pfeatures == 0 || all(testpcaP[i,1:pfeatures] == testpcaP[(i+1),1:pfeatures])){
        
        distancePair[numFound,1] <- abs(pcaLocations[i,1] - pcaLocations[(i+1),1]) # row offset
        distancePair[numFound,2] <- abs(pcaLocations[i,2] - pcaLocations[(i+1),2]) # column offset
        if (sqrt(distancePair[numFound,1]^2+distancePair[numFound,1]^2) > Nd){ # absolute distance between matching blocks
          pairLoc1[numFound,] <- pcaLocations[i,] # record this location
          pairLoc2[numFound,] <- pcaLocations[(i+1),] # increment matrix counting offset frequencies:
          pairFrequencies[distancePair[numFound,1], distancePair[numFound,2]] <- pairFrequencies[distancePair[numFound,1], distancePair[numFound,2]] + 1
          numFound <- numFound + 1
        }
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


print(system.time(imageInCopy<-pcaCProbust(imageIn,c,Nf,Nd,par,boxside,pfeatures)))
# need to rerun this line to refresh image:
display(imageInCopy)
