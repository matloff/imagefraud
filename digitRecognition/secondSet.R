library(regtools)

# This function was just for me to see the digits and is not used
# in the classification.
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

# Returns three features for a horizontal box (spanning the whole
# width of the image and the rows firstRowIndex:lastRowIndex
# in imageAsMatrix).
# The first feature is the area before
# the first horizontally encountered white-to-black transition.
# The second feature is the area before the second horizontally
# encountered white-to-black transition is encountered.
# The third feature is this area for further transitions.
# Set leftToRight to FALSE if wish to analyze right to left.
analyzeHorizontalBox <- function(imageAsMatrix, leftToRight,
                            firstRowIndex,lastRowIndex, threshold) {
    imageHeight <- nrow(imageAsMatrix)
    imageWidth <- ncol(imageAsMatrix)

    # For telling the below code to go the correct direction:
    if (leftToRight) {
        columnIndicesRange <- 2:ncol(imageAsMatrix)
    } else {
        columnIndicesRange <- (imageWidth-1):1
    }

    # Calculate horizontal features for top bounding box.
    # Areas defined by first, second, and further transitions.
    areaFirstTransition <- 0
    areaSecondTransition <- 0
    areaOtherTransitions <- 0
    # Go through each row, keeping track of which transition
    # we're on, and update the areas.
    for (rowIndex in firstRowIndex:lastRowIndex)
    {
        reachedFirstTransition <- FALSE
        reachedSecondTransition <- FALSE
        for (colIndex in columnIndicesRange)
        {
            # Check if found transition from white to black.
            if (leftToRight) {
                prevPixel <- imageAsMatrix[rowIndex,colIndex-1]
            } else {
                prevPixel <- imageAsMatrix[rowIndex,colIndex+1]
            }
            currentPixel <- imageAsMatrix[rowIndex,colIndex]
            if (prevPixel >= threshold &
                currentPixel < threshold)
            {
                # Calculate area before this transition, depending
                # on whether we were going left-to-right or right-to-left.
                if (leftToRight) {
                    areaBeforeTransition <- colIndex - 1
                } else {
                    areaBeforeTransition <- imageWidth - colIndex
                }

                if (!reachedFirstTransition)
                {
                    reachedFirstTransition <- TRUE
                    areaFirstTransition <-
                        areaFirstTransition + areaBeforeTransition
                } else if (!reachedSecondTransition) {
                    reachedSecondTransition <- TRUE
                    areaSecondTransition <-
                        areaSecondTransition + areaBeforeTransition
                } else {
                    areaOtherTransitions <-
                        areaOtherTransitions + areaBeforeTransition
                }
            }
        }
    }
    imageArea <- imageHeight * imageWidth
    return(c(areaFirstTransition / (imageArea / 2),
             areaSecondTransition / (imageArea / 2),
             areaOtherTransitions / (imageArea / 2)))
}

# Returns three features for a vertical box (spanning the whole
# height of the image and the columns firstColIndex:lastColIndex
# in imageAsMatrix).
# The first feature is the area before the first vertically
# encountered white-to-black transition.
# The second feature is the area before the second vertically
# encountered white-to-black transition is encountered.
# The third feature is this area for further transitions.
analyzeVerticalBox <- function(imageAsMatrix,
    firstColIndex, lastColIndex, threshold) {
    imageHeight <- nrow(imageAsMatrix)
    imageWidth <- ncol(imageAsMatrix)

    areaFirstTransition <- 0
    areaSecondTransition <- 0
    areaOtherTransitions <- 0
    for (colIndex in firstColIndex:lastColIndex)
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
    imageArea <- imageHeight * imageWidth
    return(c(areaFirstTransition / imageArea,
             areaSecondTransition / imageArea,
             areaOtherTransitions / imageArea))
}

# Returns the nine features for the given image.
# Given image should be a ROW of pixels.
calculateFeatureSet <- function(imageAsRow,
    imageHeight,imageWidth,threshold) {

    imageArea = imageHeight * imageWidth
    imageAsMatrix <- matrix(imageAsRow,nrow=imageHeight,ncol=imageWidth,
                            byrow=TRUE)

    # # Calculate horizontal features for top bounding box.
    # # Areas defined by first, second, and further transitions.
    # horizontalTopHalfFeatures <- analyzeHorizontalBox(imageAsMatrix,
    #     TRUE, 1, floor(imageHeight/2), threshold)

    # # Calculate LEFTWARD horizontal features for top bounding box.
    # # Areas defined by first, second, and further transitions.
    # horizontalTopHalfLeftwardFeatures <- analyzeHorizontalBox(imageAsMatrix,
    #         FALSE, 1, floor(imageHeight/2), threshold)

    # # Calculate horizontal features for bottom bounding box.
    # # Areas defined by first, second, and further transitions.
    # horizontalBottomHalfFeatures <- analyzeHorizontalBox(imageAsMatrix,
    #     TRUE, floor(imageHeight/2)+1, imageHeight, threshold)

    # # Calculate LEFTWARD horizontal features for bottom bounding box.
    # # Areas defined by first, second, and further transitions.
    # horizontalBottomHalfLeftwardFeatures <- analyzeHorizontalBox(imageAsMatrix,
    #     FALSE, floor(imageHeight/2)+1, imageHeight, threshold)

    # horizontalFeatures <- analyzeHorizontalBox(imageAsMatrix, TRUE,
    #     1, imageHeight, threshold)

    # horizontalTopThirdFeatures <- analyzeHorizontalBox(imageAsMatrix,
    #     TRUE, 1, floor(imageHeight/3), threshold)
    # horizontalMiddleThirdFeatures <- analyzeHorizontalBox(imageAsMatrix,
    #     TRUE, floor(imageHeight/3)+1, floor(2*imageHeight/3), threshold)
    # horizontalBottomThirdFeatures <- analyzeHorizontalBox(imageAsMatrix,
    #     TRUE, floor(2*imageHeight/3)+1, imageHeight, threshold)
    # horizontalTopThirdLeftwardFeatures <- analyzeHorizontalBox(imageAsMatrix,
    #     FALSE, 1, floor(imageHeight/3), threshold)
    # horizontalMiddleThirdLeftwardFeatures <- analyzeHorizontalBox(imageAsMatrix,
    #     FALSE, floor(imageHeight/3)+1, floor(2*imageHeight/3), threshold)
    # horizontalBottomThirdLeftwardFeatures <- analyzeHorizontalBox(imageAsMatrix,
    #     FALSE, floor(2*imageHeight/3)+1, imageHeight, threshold)

    horizontalFirstFourthFeatures <- analyzeHorizontalBox(imageAsMatrix,
        TRUE, 1, floor(imageHeight/4), threshold)
    horizontalSecondFourthFeatures <- analyzeHorizontalBox(imageAsMatrix,
        TRUE, floor(imageHeight/4)+1, floor(2*imageHeight/4), threshold)
    horizontalThirdFourthFeatures <- analyzeHorizontalBox(imageAsMatrix,
        TRUE, floor(2*imageHeight/4)+1, floor(3*imageHeight/4), threshold)
    horizontalBottomFourthFeatures <- analyzeHorizontalBox(imageAsMatrix,
        TRUE, floor(3*imageHeight/4)+1, imageHeight, threshold)
    horizontalFirstFourthLeftwardFeatures <- analyzeHorizontalBox(imageAsMatrix,
        FALSE, 1, floor(imageHeight/4), threshold)
    horizontalSecondFourthLeftwardFeatures <- analyzeHorizontalBox(imageAsMatrix,
        FALSE, floor(imageHeight/4)+1, floor(2*imageHeight/4), threshold)
    horizontalThirdFourthLeftwardFeatures <- analyzeHorizontalBox(imageAsMatrix,
        FALSE, floor(2*imageHeight/4)+1, floor(3*imageHeight/4), threshold)
    horizontalBottomFourthLeftwardFeatures <- analyzeHorizontalBox(imageAsMatrix,
        FALSE, floor(3*imageHeight/4)+1, imageHeight, threshold)

    # Calculate vertical features.
    # Areas defined by first, second, and further transitions.
    verticalFeatures <- analyzeVerticalBox(imageAsMatrix,
        1, imageWidth, threshold)

    return(c(horizontalFirstFourthFeatures,
             horizontalSecondFourthFeatures,
             horizontalThirdFourthFeatures,
             horizontalBottomFourthFeatures,
             horizontalFirstFourthLeftwardFeatures,
             horizontalSecondFourthLeftwardFeatures,
             horizontalThirdFourthLeftwardFeatures,
             horizontalBottomFourthLeftwardFeatures,
             verticalFeatures))

    # return(c(horizontalTopThirdFeatures,
    #          horizontalMiddleThirdFeatures,
    #          horizontalBottomThirdFeatures,
    #          horizontalTopThirdLeftwardFeatures,
    #          horizontalMiddleThirdLeftwardFeatures,
    #          horizontalBottomThirdLeftwardFeatures,
    #          verticalFeatures))

    # return(c(horizontalFeatures,verticalFeatures))

    # return(c(horizontalTopHalfFeatures,
    #          horizontalTopHalfLeftwardFeatures,
    #          horizontalBottomHalfFeatures,
    #          horizontalBottomHalfLeftwardFeatures,
    #          verticalFeatures))
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
        # store this feature set somewhere, attaching the label to the end
        # of the row
        label <- imagesAsRows[rowIndex,numPixelsPerImage+1]
        allImagesFeatures <- rbind(allImagesFeatures, c(imageFeatures,label))
    }

    return(allImagesFeatures)
}

trainClassifier <- function(trainingSetFeatures) {
    ologout <- ovalogtrn(10, trainingSetFeatures)
    return(ologout)
}

# Returns predicted labels.
generatePredictions <- function(classifier,testSetFeatures) {
    numCols <- ncol(testSetFeatures)  # for excluding last column (labels)
    predictedLabels <- ovalogpred(classifier, testSetFeatures[,1:(numCols-1)])
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

    print("Num features: ")
    print(ncol(trainingSetFeatures) - 1)

    # Set up test data features.
    # Since the test data is unlabelled, we use some data from the
    # the training set, which is labelled.
    testMNIST <- loadData(trainingDataPath,(numTrainingSamples+1),numTestSamples)
    testSetFeatures <- computeSetFeatures(
        testMNIST,imageHeight,imageWidth,numPixelsPerImage)
    # print("True labels: ")
    # print(testMNIST$y)

    fit <- trainClassifier(trainingSetFeatures)
    predictedLabels <- generatePredictions(fit,testSetFeatures)
    # print("Predicted labels: ")
    # print(predictedLabels)

    print("Accuracy: ")
    print(computeAccuracy(testMNIST$y, predictedLabels))

    print("Confusion matrix: ")
    print(computeConfusionMatrix(testMNIST$y, predictedLabels))
}

runWithMNIST <- function(trainingDataPath, testDataPath) {
    # Specific to the MNIST data set.
    imageHeight <- 28
    imageWidth <- 28
    numPixelsPerImage <- 784

    # Since we're only using the training data set (42000 samples),
    # as the test data set isn't labelled, it must be so that:
    # (numTrainingSamples + numTestSamples) <= 42000
    numTrainingSamples <- 40000
    numTestSamples <- 1500

    secondSet(trainingDataPath,testDataPath,imageHeight,imageWidth,
        numPixelsPerImage,numTrainingSamples,numTestSamples)
}
