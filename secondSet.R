# Specific to the MNIST data set.
numRows <- 28
numCols <- 28
numPixelsPerImage <- 784

plotImage <- function(imageAsRow) {
	# Must be done for MNIST because apparently the
	# MNIST as a data.frame is characters, not integers.
	imageAsRow <- as.numeric(imageAsRow)

	# The image has to be given as a matrix to be plotted.
	imageToPlot <- matrix(imageAsRow,nrow=numRows,ncol=numCols)

	# The image needs to be flipped upside down because the
	# image function puts the origin at the top left.
	imageToPlot <- imageToPlot[1:numRows,numCols:1]

	image(imageToPlot)
}

# Calculates three features in the horizontal direction in the
# first half of the digit's bounding box.
calculateFirstHorizontalFeatures <- function(imageAsRow) {

}

# Calculates three features in the horizontal direction in the
# second half of the digit's bounding box.
calculateSecondHorizontalFeatures <- function(imageAsRow) {

}

# Calculates three features in the vertical direction in the
# digit's entire bounding box.
calculateVerticalFeatures <- function(imageAsRow) {

}

# Returns the nine features for the given image.
# Given image should be a ROW of pixels.
calculateFeatureSet <- function(imageAsRow) {
	return(c(calculateFirstHorizontalFeatures(imageAsRow),
     		 calculateSecondHorizontalFeatures(imageAsRow),
			 calculateVerticalFeatures(imageAsRow)))
}

# Scales the given image (expected as a row of pixels) so that
# values are either 0 or 1.
scaleImage <- function(imageAsRow) {

}

secondSet <- function(trainingDataPath) {
	# Copy paste the (non-hard-coded) stuff from below
}

trainingDataPath <- "../kaggle_mnist/train.csv"

rawfile<-read.csv(trainingDataPath,header=T) #Reading the csv file
y <- rawfile$label
testV <- cbind(rawfile,y)
trainMNIST <- testV[,2:ncol(testV)]

for (rowIndex in nrow(trainMNIST))  # for each image in training set
{
	imageAsRow <- trainMNIST[rowIndex,1:numPixelsPerImage]
	scaledImageAsRow <- scaleImage(imageAsRow)
	featureSet <- calculateFeatureSet(scaledImageAsRow)
	# store this feature set somewhere
}

# Logistic model stuff

