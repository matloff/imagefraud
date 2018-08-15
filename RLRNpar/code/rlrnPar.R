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
  
  for (i in 1:numImages){
    
    imageIn <- images[[i]]
    
    # First paper: used only the 3 most accurate channels YCbCr (Y had 50% accuracy so dropped)
    # Second paper: very vow ~60% accuracy with RGB and high 90's with CrCb
    # (self note: may not use Y channel since also like RGB)
    # # R color space
    # imageInR <- imageData(imageIn)[,,1]
    # # G color space
    # imageInG <- imageData(imageIn)[,,2]
    # # B color space
    # imageInB <- imageData(imageIn)[,,3]
    
    # # Y color space
    # red.weight<- .2989; green.weight <- .587; blue.weight <- 0.114
    # imageInY <- red.weight * imageData(imageIn)[,,1] + green.weight * imageData(imageIn)[,,2] + blue.weight  * imageData(imageIn)[,,3]
    
    # Cb color space
    red.weight<- -0.299; green.weight <- -0.587; blue.weight <-  0.886
    imageInCb <- red.weight * imageData(imageIn)[,,1] + green.weight * imageData(imageIn)[,,2] + blue.weight  * imageData(imageIn)[,,3]
    
    # Cr color space
    red.weight<- 0.701; green.weight <-  -0.587; blue.weight <-  -0.114
    imageInCr <- red.weight * imageData(imageIn)[,,1] + green.weight * imageData(imageIn)[,,2] + blue.weight  * imageData(imageIn)[,,3]
    
    # testrlrnR <- rlrnChannelMatrix(imageInR)
    # imageFeatureArray <- rbind(imageFeatureArray,testrlrnR)
    
    # testrlrnG <- rlrnChannelMatrix(imageInG)
    # imageFeatureArray <- rbind(imageFeatureArray,testrlrnG)
    
    # testrlrnB <- rlrnChannelMatrix(imageInB)
    # imageFeatureArray <- rbind(imageFeatureArray,testrlrnB)
    
    # testrlrnY <- rlrnChannelVector(imageInY,pfeatures)
    # imageFeatureArray <- rbind(imageFeatureArray,testrlrnY)
    
    testrlrnCb <- rlrnChannelVector(imageInCb,pfeatures)
    imageFeatureArray <- rbind(imageFeatureArray,testrlrnCb)
    
    testrlrnCr <- rlrnChannelVector(imageInCr,pfeatures)
    imageFeatureArray <- rbind(imageFeatureArray,testrlrnCr)
    
  }
  
  imageFeatureArray <- imageFeatureArray[2:nrow(imageFeatureArray),]
  imageFeatureArray
  
  
}


testRLRNpar <- function(authenticDir, tamperDir, pfeatures, par=4, numTest){  
  require(EBImage)
  require("e1071")
  require('partools')
  require(tiff)
  
  setwd(authenticDir)
  
  list.filenames<-list.files(pattern=".tif$")
  list.data1<-list()
  
  if (length(list.filenames) > 0){
    for (i in 1:length(list.filenames))
    {
      list.data1[[i]] <- readTIFF(list.filenames[i])
    }
  }
  images <- list.data1
  
  
  list.filenames <-list.files(pattern=".jpg$")
  list.data2<-list()
  
  if (length(list.filenames) > 0){
    for (i in 1:length(list.filenames))
    {
      list.data2[[i]]<-readImage(list.filenames[i])
    }
  }
  images <- c(images, list.data2)
  
  list.filenames <-list.files(pattern=".png$")
  list.data3<-list()
  
  if (length(list.filenames) > 0){
    for (i in 1:length(list.filenames))
    {
      list.data3[[i]]<-readImage(list.filenames[i])
    }
  }
  images <- c(images, list.data3)
  
  numTrues <- length(list.data1) + length(list.data2) + length(list.data3)
  print('Number of UNtampered images in data set:')
  print(numTrues)
  
  
  
  
  
  
  
  setwd(tamperDir)
  
  list.filenames<-list.files(pattern=".tif$")
  list.data1<-list()
  
  if (length(list.filenames) > 0){
    for (i in 1:length(list.filenames))
    {
      list.data1[[i]] <- readTIFF(list.filenames[i])
    }
  }
  
  images <- c(images, list.data1)
  
  list.filenames <-list.files(pattern=".jpg$")
  list.data2<-list()
  
  if (length(list.filenames) > 0){
    for (i in 1:length(list.filenames))
    {
      list.data2[[i]]<-readImage(list.filenames[i])
    }
  }
  images <- c(images, list.data2)
  
  list.filenames <-list.files(pattern=".png$")
  list.data3<-list()
  
  if (length(list.filenames) > 0){
    for (i in 1:length(list.filenames))
    {
      list.data3[[i]]<-readImage(list.filenames[i])
    }
  }
  images <- c(images, list.data3)
  
  numFalses <- length(list.data1) + length(list.data2) + length(list.data3)
  print('Number of tampered images in data set:')
  print(numFalses)
  
  
  print('Number of images in total:')
  numImages <- length(images)
  print(numImages)
  
  
  # # 1 is copied 0 is not copied
  truthVector <- t(c(rep(1,numTrues*2), rep(0,numFalses*2)))
  truthVector <- t(truthVector)

  print("Time it takes to compute RLRN feature vectors:")
  if (par == 0) {
    print(system.time(allImagesArray <- imageFeatureVectors(images, numImages, pfeatures)))}
  
  if (par > 1){
    cls <- makeCluster(par)
    clusterEvalQ(cls, require(EBImage))
    imagesPerNode <- round(numImages/par)
    clusterExport(cls, varlist=c('imageFeatureVectors', 'rlrnChannelVector', "pfeatures", "imagesPerNode"), envir=environment())
    
    listimages <- list()
    for (i in 1:par){
      listimages[i]<-list(images[(imagesPerNode*(i-1)+1):(imagesPerNode*i)])
    }
    
    clusterApply(cls, listimages, function(m) {nodeImages <<- m; NULL})
    
    print(system.time(allImagesArrayC <- clusterEvalQ(cls, allImagesArrayC <- imageFeatureVectors(nodeImages, imagesPerNode, pfeatures))))
    allImagesArray <- do.call('rbind',allImagesArrayC)}
  
  # IMPORTANT:
  # applying pca and kernel pca (in paper 2) REDUCED TP rate and accuracy on Cr Cb channels
  # and these are the only channels that give high enough accuracy - so no PCA will be applied for now
  
  allImagesArray <- cbind(allImagesArray, truthVector)
  allImagesArray <-as.data.frame(allImagesArray)
  names(allImagesArray) <- c(1:(pfeatures*4),'truths')
  allImagesArray$truths <- as.factor(allImagesArray$truths)
  
  
  print("Feature vectors in test set:")
  print(numTest)
  testidxs <- sample(1:nrow(allImagesArray),numTest)
  test <- allImagesArray[testidxs,]
  train <- allImagesArray[-testidxs,] 
  testTrue <- test$truths
  test$truths <- 0 # just in case
  
  numTrain <- 2*(numImages) - numTest
  print("Feature vectors in train set:")
  print(numTrain)
  
  print("The 2 times below are for GLM training and prediction:")
  # compare to GLM
  print(system.time(fit <- glm(truths ~., data=train, family=binomial())))
  # summary(fit) 
  print(system.time(glmpredict <-predict(fit, newdata=test, type="response")))
  print("Serial GLM Output as fraction false:")
  numFPs <- sum(abs(round(glmpredict) - (as.numeric(testTrue)-1)))
  print(numFPs/length(testTrue))
  #  0.03217585 that is 97% accurate!
  print("Serial GLM Output as Confusion Matrix")
  print(table(pred=as.vector(round(glmpredict)), true=testTrue))
  
  #    true
  # pred    0    1
  #      0 1949  105
  #      1  249 7697
  
  if (par > 1 && numTrain > 700){ # any lower number of images could limit usability of SA
    print("3 times below are for parallel svm tune/train/predict")
    clusterExport(cls,'test', envir=environment()) 
    distribsplit(cls,'train',scramble=T) 
    clusterEvalQ(cls,require('e1071')) 
    
    print(system.time(clusterEvalQ(cls,tune <- tune.svm(truths ~., data = train, gamma =seq(.01, 0.1, by = .01), cost = seq(0.1,1, by = 0.1)))))
    print(system.time(clusterEvalQ(cls, mysvm <- svm(truths ~., data = train, gamma = tune$best.parameters$gamma, cost = tune$best.parameters$cost))))
    print(system.time(clusterEvalQ(cls, svmpredict <- predict(mysvm, test, type = "response"))))
    clusterEvalQ(cls, svmpredict <- as.numeric(svmpredict))
    svmpredictC <- clusterEvalQ(cls, svmpredictT <- t(svmpredict))
    predictsvmC <-Reduce('+', svmpredictC) / par # Software Alchemy
    predictsvmC <-round(predictsvmC-1)
    
    print("Parallel SVM Output as Confusion Matrix")
    #print(table(pred=as.vector(predictsvmC), true=test$truths))
    print(table(pred=as.vector(predictsvmC), true=testTrue))
    
    # 807/1000 = 0.0807 -> only 92% accurate using SVM (at least with a SA cluster of 16)
    #      true
    # pred    0    1
    #     0 1496   68
    #     1  739  7697
    
  }
  
  # grid search method applied to obtain the best value for the and  parameters (RBF kernel)
  # 10-fold cross-validation is employed in classification
  # first paper: repeated 30 times for each parameter group (C,γ)
  else {
    print("3 times below are for serial svm tune/train/predict")
    print(system.time(tune <- tune.svm(truths ~., data = train, gamma =seq(.01, 0.1, by = .01), cost = seq(0.1,1, by = 0.1))))
    # print(tune$best.parameters)
    # first paper: the performance is measured by the average classification accuracy across the 30 runs
    print(system.time(mysvm <- svm(truths ~., data = train, gamma = tune$best.parameters$gamma, cost = tune$best.parameters$cost)))
    #print(summary(mysvm))
    print(system.time(svmpredict <- predict(mysvm, test, type = "response")))
    
    print("Serial SVM Output as Confusion Matrix")
    print(table(pred=svmpredict, true=testTrue))
    
    # serial predictive power is slightly LOWER! 392/10000=0.0392 96%! and it took 7576.853 seconds (over two hours)
    #     true
    # pred    0    1
    #     0 1911   68
    #     1  324 7697
  }
  
}

# to add more images: set working directory to the two folders with names "tampered' and 'authentic'
# containing your test images and enter path to directory as a string

# First paper: used the whole database (apparently)
# please, input the directory of the authentic and tampered training images as string:
# authenticDir <- "/Users/robinyancey/desktop/authentic"
# tamperDir <- "/Users/robinyancey/desktop/tampered"
authenticDir <- "/Users/robinyancey/desktop/Au1"
tamperDir <- "/Users/robinyancey/desktop/Tp1"

# to increase the number of features per each of the 4 directions (and total features per channel)
# increase 'pfeatures' variable below (increase V61 to V(4*pfeatures))

pfeatures <- 15

# to try more channels (internally) (probably not a good idea based on both papers):
# 1. uncomment getting RGB channels from original image
# 2. increase the number of repeated truth values for each image (rep(,n) in the truthVector variable)

# to test on a cluster, set par to the number of worker nodes (par <- 0 is serial)
# NOTE: please, make sure the number of images is divisible by the cluster size
par <- 8


numTest <- 70

print(system.time(testRLRNpar(authenticDir, tamperDir, pfeatures, par, numTest)))

display(imageInCopy)
