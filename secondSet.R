library(regtools)

plotImage <- function(imageAsRow,imageHeight=imageHeight,imageWidth=imageWidth) {
    # Must be done for MNIST because apparently the
    # MNIST as a data.frame is characters, not integers.
    imageAsRow <- as.numeric(imageAsRow)

    # The image has to be given as a matrix to be plotted.
    imageToPlot <- matrix(imageAsRow,nrow=imageHeight,ncol=imageWidth)

    # The image needs to be flipped upside down because the
    # image function puts the origin at the top left.
    imageToPlot <- imageToPlot[1:imageHeight,imageWidth:1]

    image(imageToPlot)
}

# Calculates three features in the horizontal direction in the
# first half of the digit's bounding box.
# calculateFirstHorizontalFeatures <- function(imageAsRow) {

# }

# # Calculates three features in the horizontal direction in the
# # second half of the digit's bounding box.
# calculateSecondHorizontalFeatures <- function(imageAsRow) {

# }

# # Calculates three features in the vertical direction in the
# # digit's entire bounding box.
# calculateVerticalFeatures <- function(imageAsRow) {

# }

# Returns the nine features for the given image.
# Given image should be a ROW of pixels.
calculateFeatureSet <- function(imageAsRow,
    imageHeight,imageWidth,threshold) {

    imageArea = imageHeight * imageWidth
    imageAsMatrix <- matrix(imageAsRow,nrow=imageHeight,ncol=imageWidth,
                            byrow=TRUE)

    # Calculate horizontal features for top bounding box.
    # Areas defined by first, second, and further transitions.
    areaFirstTransition <- 0
    areaSecondTransition <- 0
    areaOtherTransitions <- 0
    # Go through each row, keeping track of which transition
    # we're on, and update the areas.
    for (rowIndex in 1:floor(imageHeight/2))
    {
        reachedFirstTransition <- FALSE
        reachedSecondTransition <- FALSE
        for (colIndex in 2:imageWidth)
        {
            # Check if found transition from white to black.
            prevPixel <- imageAsMatrix[rowIndex,colIndex-1]
            currentPixel <- imageAsMatrix[rowIndex,colIndex]
            if (prevPixel >= threshold &
                currentPixel < threshold)
            {
                if (!reachedFirstTransition)
                {
                    reachedFirstTransition <- TRUE
                    areaFirstTransition <-
                        areaFirstTransition + (colIndex-1)
                } else if (!reachedSecondTransition) {
                    reachedSecondTransition <- TRUE
                    areaSecondTransition <-
                        areaSecondTransition + (colIndex-1)
                } else {
                    areaOtherTransitions <-
                        areaOtherTransitions + (colIndex-1)
                }
            }
        }
    }
    horizontalTopHalfFeatures <-
        c(areaFirstTransition / (imageArea / 2),
          areaSecondTransition / (imageArea / 2),
          areaOtherTransitions / (imageArea / 2))

    # Calculate horizontal features for bottom bounding box.
    # Areas defined by first, second, and further transitions.
    areaFirstTransition <- 0
    areaSecondTransition <- 0
    areaOtherTransitions <- 0
    for (rowIndex in (floor(imageHeight/2)+1):imageHeight)
    {
        reachedFirstTransition <- FALSE
        reachedSecondTransition <- FALSE
        for (colIndex in 2:imageWidth)
        {
            # Check if found transition from white to black.
            prevPixel <- imageAsMatrix[rowIndex,colIndex-1]
            currentPixel <- imageAsMatrix[rowIndex,colIndex]
            if (prevPixel >= threshold &
                currentPixel < threshold)
            {
                if (!reachedFirstTransition)
                {
                    reachedFirstTransition <- TRUE
                    areaFirstTransition <-
                        areaFirstTransition + (colIndex-1)
                } else if (!reachedSecondTransition) {
                    reachedSecondTransition <- TRUE
                    areaSecondTransition <-
                        areaSecondTransition + (colIndex-1)
                } else {
                    areaOtherTransitions <-
                        areaOtherTransitions + (colIndex-1)
                }
            }
        }
    }
    horizontalBottomHalfFeatures <-
        c(areaFirstTransition / (imageArea / 2),
          areaSecondTransition / (imageArea / 2),
          areaOtherTransitions / (imageArea / 2))

    # Calculate vertical features.
    # Areas defined by first, second, and further transitions.
    areaFirstTransition <- 0
    areaSecondTransition <- 0
    areaOtherTransitions <- 0
    for (colIndex in 1:imageWidth)
    {
        reachedFirstTransition <- FALSE
        reachedSecondTransition <- FALSE
        for (rowIndex in 2:imageHeight)
        {
            # Check if found transition from white to black.
            prevPixel <- imageAsMatrix[rowIndex-1,colIndex]
            currentPixel <- imageAsMatrix[rowIndex,colIndex]
            if (prevPixel >= threshold &
                currentPixel < threshold)
            {
                if (!reachedFirstTransition)
                {
                    reachedFirstTransition <- TRUE
                    areaFirstTransition <-
                        areaFirstTransition + (rowIndex-1)
                } else if (!reachedSecondTransition) {
                    reachedSecondTransition <- TRUE
                    areaSecondTransition <-
                        areaSecondTransition + (rowIndex-1)
                } else {
                    areaOtherTransitions <-
                        areaOtherTransitions + (rowIndex-1)
                }
            }
        }
    }
    verticalFeatures <-
        c(areaFirstTransition / imageArea,
          areaSecondTransition / imageArea,
          areaOtherTransitions / imageArea)

    return(c(horizontalTopHalfFeatures,
             horizontalBottomHalfFeatures,
             verticalFeatures))
}

# Returns data after moving labels column from first to last column.
# firstSample says what sample to start it; for e.g., if I wanted to
# use the 101st through 200th samples, I'd pass firstSample=101,
# numSamples=100.
loadData <- function(dataPath,firstSample,numSamples) {
    rawfile<-read.csv(dataPath,header=T) #Reading the csv file

    # Move the labels column from the first to the last column
    y <- rawfile$label
    tmp <- cbind(rawfile,y)

    # Only keep the desired samples.
    return(tmp[firstSample:(firstSample+numSamples-1),2:ncol(tmp)])
}

# Returns the percentage of labels that are the same in the given vectors.
# The argument vectors are assumed to be same length.
computeAccuracy <- function(correctLabels, predictedLabels) {
    return(mean(correctLabels == predictedLabels))
}

# This function is specific to the MNIST data set and assumes 10 labels: 0-9.
# Set detailedLabels to TRUE if want detailed row/col names on the
# confusion matrix (e.g. "actual: 8").
computeConfusionMatrix <- function(correctLabels, predictedLabels,
                                   labelled=FALSE) {
    # Set up empty confusion matrix.
    numLabels <- 10
    confusionMatrix <- matrix(0,nrow=numLabels,ncol=numLabels)
    if (labelled) {
        rownames(confusionMatrix) <- paste("actual:", 0:9)
        colnames(confusionMatrix) <- paste("predicted:", 0:9)
    } else {
        rownames(confusionMatrix) <- 0:9
        colnames(confusionMatrix) <- 0:9
    }

    for (labelIndex in 1:length(correctLabels)) {
        correctLabel <- correctLabels[labelIndex]
        predictedLabel <- predictedLabels[labelIndex]

        # First label (0) corresponds to index 1.
        confusionMatrix[correctLabel+1,predictedLabel+1] <-
            1 + confusionMatrix[correctLabel+1,predictedLabel+1]
    }

    return(confusionMatrix)
}

computeSetFeatures <- function(imagesAsRows,imageHeight,imageWidth,numPixelsPerImage) {
    allImagesFeatures <- c() # features for each image in imagesAsRows

    # Go through each row (each row is an image), calculate the
    # features for the row, and store those features.
    for (rowIndex in 1:nrow(imagesAsRows))  # for each image in set
    {
        imageAsRow <- imagesAsRows[rowIndex,1:numPixelsPerImage]
        imageFeatures <- calculateFeatureSet(imageAsRow,imageHeight,
                                          imageWidth,1)
        # store this feature set somewhere
        label <- imagesAsRows[rowIndex,numPixelsPerImage+1]
        allImagesFeatures <- rbind(allImagesFeatures, c(imageFeatures,label))
    }

    # Some formatting.
    allImagesFeatures <- as.data.frame(allImagesFeatures)
    names(allImagesFeatures) <- c("fh1_top", "fh2_top", "fh3_top",
                                    "fh1_bot", "fh2_bot", "fh3_bot",
                                    "fv1", "fv2", "fv3",
                                    "truths")
    allImagesFeatures$truths <- as.factor(allImagesFeatures$truths)

    return(allImagesFeatures)
}

trainClassifier <- function(trainingSetFeatures) {
    # Based on similar code from: https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
    # library(nnet)
    # trainingSetFeatures$truths2 <- relevel(trainingSetFeatures$truths,
    #                                        ref="0")  # make "0" the baseline class
    # fit <- multinom(truths2 ~ fh1_top + fh2_top + fh3_top
    #                         + fh1_bot + fh2_bot + fh3_bot
    #                         + fv1 + fv2 + fv3,
    #                 data = trainingSetFeatures,
    #                 trace=FALSE)  # trace=FALSE suppresses output
    # return(fit)

    ologout <- ovalogtrn(10, trainingSetFeatures)
    return(ologout)
}

# Returns predicted labels.
generatePredictions <- function(classifier,testSetFeatures) {
    # predictions <- predict(classifier, newdata=testSetFeatures, "probs")
    predictedLabels <- ovalogpred(classifier,testSetFeatures[,1:9])
    # predictedLabels <- c()
    # for (rowIndex in 1:nrow(predictions))
    # {
    #     tmp <- (as.integer(
    #         which(predictions[rowIndex,] == max(predictions[rowIndex,]))) - 1)
    #     predictedLabels <- c(predictedLabels, tmp)
    # }
    return(predictedLabels)
}

secondSet <- function(trainingDataPath,testDataPath,
                      imageHeight,imageWidth,
                      numPixelsPerImage,numTrainingSamples,
                      numTestSamples) {
    # Set up training data features.
    trainMNIST <- loadData(trainingDataPath,1,numTrainingSamples)
    trainingSetFeatures <- computeSetFeatures(
        trainMNIST,imageHeight,imageWidth,numPixelsPerImage)

    # Set up test data features.
    # Since the test data is unlabelled, we use some data from the
    # the training set, which is labelled.
    testMNIST <- loadData(trainingDataPath,(numTrainingSamples+1),numTestSamples)
    testSetFeatures <- computeSetFeatures(
        testMNIST,imageHeight,imageWidth,numPixelsPerImage)
    print("True labels: ")
    print(testMNIST$y)

    fit <- trainClassifier(trainingSetFeatures)
    predictedLabels <- generatePredictions(fit,testSetFeatures)
    print("Predicted labels: ")
    print(predictedLabels)

    print("Accuracy: ")
    print(computeAccuracy(testMNIST$y, predictedLabels))

    print("Confusion matrix: ")
    print(computeConfusionMatrix(testMNIST$y, predictedLabels))
}

runWithMNIST <- function() {
    # Specific to the MNIST data set.
    imageHeight <- 28
    imageWidth <- 28
    numPixelsPerImage <- 784
    trainingDataPath <- "../kaggle_mnist/train.csv"
    testDataPath <- "../kaggle_mnist/test.csv"

    # Since we're only using the training data set (42000 samples),
    # as the test data set isn't labelled, it must be so that:
    # (numTrainingSamples + numTestSamples) <= 42000
    # numTrainingSamples <- 1000
    # numTestSamples <- 100
    numTrainingSamples <- 40000
    numTestSamples <- 1500

    secondSet(trainingDataPath,testDataPath,imageHeight,imageWidth,
        numPixelsPerImage,numTrainingSamples,numTestSamples)
}
