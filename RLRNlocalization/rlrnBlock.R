blockMatrix <- function(imageIn_t, imageIn_a){
  
  boxside <- 512
  
  width <- dim(imageIn_t)[1]
  height <- dim(imageIn_t)[2]
  
  #imageIn_t_copy <- imageIn_t
  
  blocksArray <- matrix(0, nrow=1, ncol=((pfeatures*4)+3)) 
  
  red.weight<- .2989; green.weight <- .587; blue.weight <- 0.114
  imageIn_a2 <- red.weight * imageData(imageIn_a)[,,1] + green.weight * imageData(imageIn_a)[,,2] + blue.weight  * imageData(imageIn_a)[,,3]
  imageIn_t2 <- red.weight * imageData(imageIn_t)[,,1] + green.weight * imageData(imageIn_t)[,,2] + blue.weight  * imageData(imageIn_t)[,,3]

  for (i in seq(1, (width-boxside+1), by=(boxside))){
    for (j in seq(1, (height-boxside+1), by=(boxside))){
      endw <- i+(boxside-1)
      endh <- j+(boxside-1)

      block_a <- imageIn_a[i:endw,j:endh,1:3]
      block_t <- imageIn_t[i:endw,j:endh,1:3]

      # test_a <- imageIn_a2[i,j]
      # test_t <- imageIn_t2[i,j]
      test_a <- mean(imageIn_a2[i:endw,j:endh])
      test_t <- mean(imageIn_t2[i:endw,j:endh])
      #
      truth <- 1
      if (abs(test_a - test_t) > 0.001) {
        truth <- 0
        print("fraud")
        #imageIn_t_copy[i:endw,j:endh,1:3] <- 0
      }

      truthlocs <- rbind(c(truth,i,j),c(truth,i,j))

      newblock_a <- imageFeatureVectors(block_a, 1, pfeatures)
      newblock_t <- imageFeatureVectors(block_t, 1, pfeatures)

      newblock_a <- cbind(newblock_a, truthlocs)
      newblock_t <- cbind(newblock_t, truthlocs)

      newblocks <-rbind(newblock_a, newblock_t)
      blocksArray <- rbind(blocksArray, newblock_a)

    }
  }
  #display(imageIn_t_copy)

  blocksArray<-blocksArray[2:nrow(blocksArray),]
  blocksArray
  
  #imageIn_t_copy
} 

# this needs to be applied to each color space
rlrnChannelVector <- function(imageIn,pfeatures){
  
  require('scales')
  imageIn <- as.matrix(imageIn) 
  
  imageInCopy <- imageIn
  
  width <- nrow(imageIn)
  height<- ncol(imageIn)
  
  # decorrelation on each channel
  # self note: dont forget to try changing the quantization level 0-255 (scale/m)
  imageInH <- abs(imageIn[1:(width-1),] - imageIn[2:width,])
  imageInH <- round(rescale(imageInH, to = c(0, 255)))
  
  imageInV <- abs(imageIn[,1:(height-1)] - imageIn[,2:height])
  imageInV <- round(rescale(imageInV, to = c(0, 255)))
  
  imageInD <- abs(imageIn[1:(width-1),1:(height-1)] - imageIn[2:width,2:height])
  imageInD <- round(rescale(imageInD, to = c(0, 255)))
  
  imageInD2 <- abs(imageIn[1:(width-1),2:height] - imageIn[2:width,1:(height-1)])
  imageInD2 <- round(rescale(imageInD2, to = c(0, 255)))
  
  
  testrlrn <- matrix(0, nrow=1, ncol=(pfeatures*4))
  
  # calculate run lengths
  
  # (actual) horizontal direction
  end <- 0
  for (i in 1:(width-2)){
    for (j in 1:height){
      end <- 0
      l <- 1
      while (end != 1 && l <= pfeatures){
        if (i+l >= (width-2)){
          testrlrn[1,l] <- testrlrn[1,l] + 1
          end <- T
          break}
        if (imageInH[(i+l),j] == imageInH[(i+l-1),j]){
          l <- l+1
        }
        else{
          testrlrn[1,l] <- testrlrn[1,l] + 1
          end <- 1
        }
      }
      
    }
  }
  
  # (actual) vertical direction
  end <- 0
  for (i in 1:width){
    for (j in 1:(height-2)){
      end <- 0
      l <- 1
      while (end != T && l <= pfeatures){
        if (j+l >= (height-2)){
          testrlrn[1,(pfeatures+l)] <- testrlrn[1,(pfeatures+l)] + 1
          end <- T
          break}
        if (imageInV[i,(j+l)] == imageInV[i,(j+l-1)]){
          l <- l+1
        }
        else{
          testrlrn[1,(pfeatures+l)] <- testrlrn[1,(pfeatures+l)] + 1
          end <- T
        }
      }
      
    }
  }
  
  # down diagonal direction 
  end <- 0
  for (i in 1:(width-2)){
    for (j in 1:(height-2)){
      end <- 0
      l <- 1 
      while (end != T && l <= pfeatures){
        if ((j+l >= (height-2)) | (i+l >= (width-2))){
          testrlrn[1,((pfeatures*2)+l)] <- testrlrn[1,((pfeatures*2)+l)] + 1
          end <- T
          break}
        if (imageInD[(i+l),(j+l)] == imageInD[(i+l-1),(j+l-1)]){
          l <- l+1
        }
        else{
          testrlrn[1,((pfeatures*2)+l)] <- testrlrn[1,((pfeatures*2)+l)] + 1
          end <- 1
        }
      }
      
    }
  }
  
  # counter diagonal direction (minus j, increment i of next element starting from from width)
  # (this is different in paper 2 which is probably wrong since it goes along the same direction)
  # ps: thoroughly checked both diagonals :)
  end <- 0
  for (i in (width-2):1){
    for (j in 1:(height-2)){
      end <- 0
      l <- 1
      while (end != T && l <= pfeatures){
        if (j+l >= (height-2) || i-l <= 1){
          testrlrn[1,((pfeatures*3)+l)] <- testrlrn[1,((pfeatures*3)+l)] + 1
          end <- T
          break}
        if (imageInD2[(i-l),(j+l)] == imageInD2[(i-l+1),(j+l-1)]){
          l <- l+1
        }
        else{
          testrlrn[1,((pfeatures*3)+l)] <- testrlrn[1,((pfeatures*3)+l)] + 1
          end <- 1
        }
      }
      
    }
  }
  
  
  # multiply each column j by length j (each increment of the element represented 1 of that rl)
  testrlrn  <- testrlrn * t(c(1:pfeatures,1:pfeatures,1:pfeatures,1:pfeatures))
  
}


imageFeatureVectors <-function(images, numImages, pfeatures){
  
  imageFeatureArray <- 1:(pfeatures*4)
  
#  for (i in 1:numImages){
    imageIn <- images
    
    #print(dim(imageIn))
    # Cb color space
    red.weight<- -0.299; green.weight <- -0.587; blue.weight <-  0.886
    imageInCb <- red.weight * imageData(imageIn)[,,1] + green.weight * imageData(imageIn)[,,2] + blue.weight  * imageData(imageIn)[,,3]
    
    # Cr color space
    red.weight<- 0.701; green.weight <-  -0.587; blue.weight <-  -0.114
    imageInCr <- red.weight * imageData(imageIn)[,,1] + green.weight * imageData(imageIn)[,,2] + blue.weight  * imageData(imageIn)[,,3]
    
    testrlrnCb <- rlrnChannelVector(imageInCb,pfeatures)
    imageFeatureArray <- rbind(imageFeatureArray,testrlrnCb)
    
    testrlrnCr <- rlrnChannelVector(imageInCr,pfeatures)
    imageFeatureArray <- rbind(imageFeatureArray,testrlrnCr)
    
#  }
  
  imageFeatureArray <- imageFeatureArray[2:nrow(imageFeatureArray),]
  imageFeatureArray
  
  
}


testRLRNblocks <- function(authenticDir, tamperDir, outputDir, pfeatures=15, numTest=1, thresh=0){  
  
  require(EBImage)
  require('partools')
  
  setwd(authenticDir)
  list.filenames<-list.files(pattern=".png$")
  list.data<-list()
  
  for (i in 1:(length(list.filenames)))
  {
    list.data[[i]]<-readImage(list.filenames[i])
  }
  

  numTrues <- length(list.data)
  print('Number of UNtampered images in data set:')
  print(numTrues)
  images_a <- list.data
  
  
  setwd(tamperDir)
  list.filenames<-list.files(pattern=".png$")
  list.data<-list()
  
  for (i in 1:(length(list.filenames)))
  {
    list.data[[i]]<-readImage(list.filenames[i])
  }
  

  numFalses <- length(list.data)
  print('Number of tampered images in data set:')
  print(numFalses)
  images_t <- list.data
  print('Number of images in total:')
  numImages <- length(images_t) + length(images_a)
  print(numImages)
  
  # cls <- makeCluster(par)
  # clusterEvalQ(cls, require(EBImage))
  # imagesPerNode <- round(numFalses/par)
  # clusterExport(cls, varlist=c('imageFeatureVectors', 'rlrnChannelVector', "pfeatures", "imagesPerNode", "blockMatrixC","blockMatrix"), envir=environment())
  # 
  # listimages1 <- list()
  # listimages2 <- list()
  # for (i in 1:par){
  #   listimages1[i]<-list(images_t[(imagesPerNode*(i-1)+1):(imagesPerNode*i)])
  #   listimages2[i]<-list(images_a[(imagesPerNode*(i-1)+1):(imagesPerNode*i)])
  # }
  # 
  # clusterApply(cls, listimages1, function(m) {nodeImages1 <<- m; NULL})
  # clusterApply(cls, listimages2, function(m) {nodeImages2 <<- m; NULL})
  # 
  # #print(system.time(allImagesArrayC <- clusterEvalQ(cls, allImagesArrayC <- imageFeatureVectors(nodeImages, imagesPerNode, pfeatures))))
  #blockMatrixC <-function(nodeImages1[i], nodeImages2[i])){
  #nodeBlocks <- blockMatrix(imageIn_t, imageIn_a)
  
  #list.data[[i]]<-nodeBlocks
  
  #allNodeBlocks <- rbind(allNodeBlocks, nodeBlocks)
  
  
 # }

  allNodeBlocks <- matrix(0, nrow=1, ncol=((pfeatures*4)+3))  
  list.data<-list()
  for (i in 1:numFalses){
    
    width <- nrow(images_t[[i]])
    height<- ncol(images_t[[i]])
    
    if (width > 1024 && height > 1024 ) {      
    
    imageIn_t <- images_t[[i]]
    imageIn_a <- images_a[[i]]
    

    nodeBlocks <- blockMatrix(imageIn_t, imageIn_a)
    
    list.data[[i]]<-nodeBlocks
    
    allNodeBlocks <- rbind(allNodeBlocks, nodeBlocks)
    }
  }
  
  # cls <- makeCluster(par)
  # clusterEvalQ(cls, require(EBImage))
  # imagesPerNode <- round(numFalses/par)
  # clusterExport(cls, varlist=c('imageFeatureVectors', 'rlrnChannelVector', "pfeatures", "imagesPerNode", "blockkMatrixC"), envir=environment())
  # 
  # listimages1 <- list()
  # listimages2 <- list()
  # for (i in 1:par){
  #   listimages1[i]<-list(images_t[(imagesPerNode*(i-1)+1):(imagesPerNode*i)])
  #   listimages2[i]<-list(images_a[(imagesPerNode*(i-1)+1):(imagesPerNode*i)])
  # }
  # 
  # clusterApply(cls, listimages1, function(m) {nodeImages1 <<- m; NULL})
  # clusterApply(cls, listimages2, function(m) {nodeImages2 <<- m; NULL})
  # 
  # #print(system.time(allImagesArrayC <- clusterEvalQ(cls, allImagesArrayC <- imageFeatureVectors(nodeImages, imagesPerNode, pfeatures))))
  # #allImagesArray <- do.call('rbind',allImagesArrayC)
  
  # for (i in 1:imagesPerNode){
  #     
  #     nodeBlocks <- blockMatrix(nodeImages1[i], nodeImages2[i])
  #     
  #     list.data[[i]] <-nodeBlocks)
  #     
  #     allNodeBlocks <- rbind(allNodeBlocks, nodeBlocks)
  #   }
  
  
  locs <- allNodeBlocks[2:nrow(allNodeBlocks),62:63]
  allImagesArray <- allNodeBlocks[2:nrow(allNodeBlocks),1:61]
  
  allImagesArray <-as.data.frame(allImagesArray)
  names(allImagesArray) <- c(1:(pfeatures*4),'truths')
  allImagesArray$truths <- as.factor(allImagesArray$truths)
  
  train <- allImagesArray

  print("The 2 times below are for GLM training and prediction:")

  print(system.time(fit <- glm(truths ~., data=train, family=binomial())))
  # summary(fit) see warning on sticky note
  
  
  
  for (j in 1:numTest){
    
    setwd(outputDir)
    
    oneImageArray <- list.data[[j]][,1:61]
    locs <- list.data[[j]][,62:63]
    imageIn_t <- images_t[[j]]
    
    width <- nrow(images_t[[j]])
    print(width)
    height<- ncol(images_t[[j]])
    
  if (width > 1024 && height > 1024 ) {    
     
    oneImageArray <- as.data.frame(oneImageArray)
    names(oneImageArray) <- c(1:(pfeatures*4),'truths')
    oneImageArray$truths <- as.factor(oneImageArray$truths)
    
    test <- oneImageArray
    testTrue <- test$truths
    test$truths <- 0
    print(j)
    print(system.time(glmpredict <-predict(fit, newdata=test, type="response")))
    
    boxside <- 512
    tamperedinds <- which(round(glmpredict-thresh) == 0, arr.ind=TRUE)
    
    if (length(tamperedinds) > 1){ 

      pairs<-locs[tamperedinds,]

      for (i in 1:length(tamperedinds)){
        
       rEnd<- (pairs[i,1]+boxside - 1)
       cEnd<- (pairs[i,2]+boxside - 1)
  
        if ((pairs[i,2]+boxside - 1)> ncol(imageIn_t)){cEnd<- pairs[i,2] - ((pairs[i,2]+boxside - 1)-ncol(imageIn_t))}
        if ((pairs[i,1]+boxside - 1)> nrow(imageIn_t)){rEnd<- pairs[i,1] - ((pairs[i,1]+boxside - 1)-nrow(imageIn_t))}
        
        imageIn_t[pairs[i,1]:rEnd, pairs[i,2]:cEnd,1:3] = c
      }
    }
    
    if (length(tamperedinds) == 0){ sorted <- sort(glmpredict)
    min <- sorted[1]
    tamperedinds <- which(glmpredict == min, arr.ind=TRUE)
    }
    if (length(tamperedinds) == 1){
      
        pairs<-locs[tamperedinds,]
        
        rEnd<- (pairs[1]+boxside - 1)
        cEnd<- (pairs[2]+boxside - 1)

        if ((pairs[2]+boxside - 1)> ncol(imageIn_t)){cEnd<- pairs[2] - ((pairs[2]+boxside - 1)-ncol(imageIn_t))}
        if ((pairs[1]+boxside - 1)> nrow(imageIn_t)){rEnd<- pairs[1] - ((pairs[1]+boxside - 1)-nrow(imageIn_t))}
        
        imageIn_t[pairs[1]:rEnd, pairs[2]:cEnd,1:3] = c
      
    }
    
    
    name <- paste(j, "predicted", sep="_")
    png(filename=name)
    plot(imageIn_t)
    dev.off ()  
    
    imageIn_t <- images_t[[j]]
    tamperedinds <- which(testTrue == 0, arr.ind=TRUE)
    
    
    if (length(tamperedinds) > 1){ 
      
      pairs<-locs[tamperedinds,]
      
      for (i in 1:length(tamperedinds)){
        
        rEnd<- (pairs[i,1]+boxside - 1)
        cEnd<- (pairs[i,2]+boxside - 1)
         
        if ((pairs[i,2]+boxside - 1)> ncol(imageIn_t)){cEnd<- pairs[i,2] - ((pairs[i,2]+boxside - 1)-ncol(imageIn_t))}
        if ((pairs[i,1]+boxside - 1)> nrow(imageIn_t)){rEnd<- pairs[i,1] - ((pairs[i,1]+boxside - 1)-nrow(imageIn_t))}
        
        imageIn_t[pairs[i,1]:rEnd, pairs[i,2]:cEnd,1:3] = c
      }
    }
    if (length(tamperedinds) == 1){
        
        pairs<-locs[tamperedinds,]
        
        rEnd<- (pairs[1]+boxside - 1)
        cEnd<- (pairs[2]+boxside - 1)

        if ((pairs[2]+boxside - 1)> ncol(imageIn_t)){cEnd<- pairs[2] - ((pairs[2]+boxside - 1)-ncol(imageIn_t))}
        if ((pairs[1]+boxside - 1)> nrow(imageIn_t)){rEnd<- pairs[1] - ((pairs[1]+boxside - 1)-nrow(imageIn_t))}
        
        imageIn_t[pairs[1]:rEnd, pairs[2]:cEnd,1:3] = c
        
    }
    name <- paste(j, "actual", sep="_")
    png(filename=name)
    plot(imageIn_t)
    dev.off ()  

  }
  }
  
}
authenticDir <- "/Users/robinyancey/desktop/originals"
tamperDir <- "/Users/robinyancey/desktop/spliced"

outputDir <- "/Users/robinyancey/desktop/outputImages"


pfeatures <- 15
numTest <- 10
thresh <- 0.1

print(system.time(testRLRNblocks(authenticDir, tamperDir, outputDir, pfeatures, numTest, thresh)))

display(imageInCopy)
