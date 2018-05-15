#libraries required for pca
library(jpeg)
library(scales)

#libraries required for nmf
library(NMF)
library(pixmap)

# Call this one!
faridPca <- function(img, compressMethod)
# compressMethod is either the string pca or 
{
	img <- readJPEG(img)

	# Parameters for running the algorithm.
	parameters <- new.env()

	parameters$redChannel <- img[,,1]
	parameters$greenChannel <- img[,,2]
	parameters$blueChannel <- img[,,3]

	parameters$imgWithBlocks <- img
	parameters$imgWidth <- dim(img)[2]
	parameters$imgHeight <- dim(img)[1]

	parameters$compressMethod <- compressMethod

	parameters$imgDim <- dim(img)
	parameters$N <- parameters$imgDim[1] * parameters$imgDim[2]

	parameters$b <- 64

	parameters$N_b <- (sqrt(parameters$N) - sqrt(parameters$b) + 1)^2

	parameters$e <- 0.01
	parameters$Q <- 256
	parameters$N_n <- 100  # number of neighboring rows to search in sorted matrix
	parameters$N_f <- 128
	parameters$N_d <- 16

	# for now, set arbitrarily
	# TODO: change this
	parameters$N_t <- 8

	parameters$imgWidth <- dim(img)[2]
	parameters$imgHeight <- dim(img)[1]

	# redSlice <- channelPca(img, 1, parameters)
	changedImg <- redSlice <- channelPca(img, 1, parameters)
	# greenSlice <- channelPca(img, 2, parameters)
	# blueSlice <- channelPca(img, 3, parameters)

	plot.new()
	plot.window(xlim=c(0,parameters$imgWidth), ylim=c(0,parameters$imgHeight))
	# parameters$imgWithBlocks[,,1] <- redSlice
	# parameters$imgWithBlocks[,,2] <- greenSlice
	# parameters$imgWithBlocks[,,3] <- blueSlice


	# rasterImage(parameters$imgWithBlocks, xleft=0, xright =parameters$imgWidth,
	rasterImage(changedImg, xleft=0, xright =parameters$imgWidth,
		ybottom=0,ytop=parameters$imgHeight)


}

# Creates 
compressBlocks <- function()
{

}

# parameters is environment containing all the params for running
# the algorithm.
channelPca <- function(img, channelValue, parameters)
{
	channel <- img[,,channelValue]

	# To help the upcoming double for loop.
	topLeftX <- 1
	topLeftY <- 1
	blockWidth <- sqrt(parameters$b)
	numSlidingBlocksPerRow <- parameters$imgWidth - blockWidth + 1
	blockHeight <- sqrt(parameters$b)
	numSlidingBlocksPerCol <- parameters$imgHeight - blockHeight + 1

	# Init matrix where each row represents a block as a row vector.
	# The max size of a row is whatever the max size of N_t is (i.e. b).
	# Have two extra spots in each row to hold the x and y coordinate of the
	# top left of the corresponding block (top left of image is (0,0)).
	blocks <- matrix(0, nrow = parameters$N_b, ncol = parameters$b + 2)

	blockIndex <- 1
	for (rowIndex in 1:numSlidingBlocksPerCol)
	{
		for (colIndex in 1:numSlidingBlocksPerRow)
		{
			# PERFORMING PCA ON ONE BLOCK
			#gets specific block for one color channel
			block <- channel[rowIndex:(rowIndex+blockHeight-1),
			                       colIndex:(colIndex+blockWidth-1)]
			#performs PCA on specified block for one color channel
			if (parameters$compressMethod == "pca")
			{
				# blockPca <- prcomp(block, center = FALSE)
				#compresses PCA results into a vector for specified red block
				# blockCompressed <- as.vector(blockPca$x[,1:parameters$N_t])
			}
			if (parameters$compressMethod == "nmf")
			{
				blockPca <- nmf(block,2)
				blockCompressed <- blockPca@fit@W %*% blockPca@fit@H
			}
			# each matrix with each row containing a compressed block of the one color channel block
			# blocks[blockIndex,(1:parameters$b)] <- as.vector(blockCompressed)
			blocks[blockIndex,(1:parameters$b)] <- as.vector(block)
			# storing the block's coordinates for easier lookup for the one color channel block
			blocks[blockIndex,(parameters$b+1):(parameters$b+2)] <- c(rowIndex,colIndex)

			# increasing blockIndex for the matrix 
			blockIndex <- blockIndex + 1

			# TODO: 
			# delete later
			# old way we quantized vectors
			# blockQuantized <- as.vector(round(rescale(blockCompressed,
			# 	 to = c(0, q))))
			# blockQuantized <- floor(blockCompressed / Q)
			# blocks[blockIndex,1:b] <- blockQuantized
		}
	}

	blocks <- prcomp(blocks)$x
	# browser()

	# sorts the rows in lexographic order for redChannel
	S <<- blocks[do.call(order, lapply(1:(parameters$b+2), function(i) blocks[,i])),]
	parameters <<- parameters

	# Note that the last two coordinates of each row in S are the
	# coordinates of the block corresponding to that row.
	# offsets stores every pair of coordinates of rows that are within
	# N_n of each other.
	coordinatePairs <- list()

	numCoordinatePairs <- 0
	for (rowIndex in 1:(nrow(S)-parameters$N_n))
	{
		for (rowAppend in 1:(parameters$N_n-1))
		{
			numCoordinatePairs <- numCoordinatePairs + 1

			#first is coordinates of block1
			#second is coordinates of block2
			# message("rowIndex=", rowIndex, "rowAppend=", rowAppend)
			coordinatePairs[[numCoordinatePairs]] <- list(first=S[rowIndex,(parameters$b+1):(parameters$b+2)],
				second=S[rowIndex+rowAppend,(parameters$b+1):(parameters$b+2)])
		}
	}

	message("Survived coordinate pairs")
	# browser()

	offsets <- list()
	numOffsets <- 0
	for (coordinatePair in coordinatePairs)
	{
		coordinates1 <- coordinatePair$first
		xi <- coordinates1[1]
		yi <- coordinates1[2]
		coordinates2 <- coordinatePair$second
		xj <- coordinates2[1]
		yj <- coordinates2[2]
		if (xi - xj > 0) {
			offset <- list(xi - xj, yi - yj, xi, yi, xj, yj)
		} else if (xi - xj < 0) {
			offset <- list(xj - xi, yi - yj, xi, yi, xj, yj)
		} else {
			offset <- list(0, abs(yi - yj), xi, yi, xj, yj)
		}

		if (sqrt((offset[[1]])^2 + (offset[[2]])^2) >= parameters$N_d) {
			# Add the offset.
			numOffsets <- numOffsets + 1
			offsets[[numOffsets]] <- offset
		}
	}

	# print(offsets)

	message("Survived coordinate pairs again")
	# browser()

	frequentOffsets <- list()
	numFrequentOffsets <- 0
	# Source: https://stackoverflow.com/questions/28100593/how-to-sort-a-list-of-lists-in-r?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
	tmp <- offsets[order(sapply(offsets,'[[',2))]
	sortedOffsets <- tmp[order(sapply(tmp,'[[',1))]
	# print(sortedOffsets)
	offsetFrequency <- 0
	previousOffset <- list(-1,-1)
	for (offset in sortedOffsets)
	{
		if (offset[[1]] == previousOffset[[1]] &&
			offset[[2]] == previousOffset[[2]])
		{
			offsetFrequency <- offsetFrequency + 1
		} else {
			# Found new offset.
			if (offsetFrequency >= parameters$N_f) {
				numFrequentOffsets <- numFrequentOffsets + 1
				frequentOffsets[[numFrequentOffsets]] <- previousOffset
			}

			# Reset for this new offset.
			offsetFrequency <- 0
		}
		previousOffset <- offset
	}

	# print("frequentOffsets")
	# print(frequentOffsets)

	# TODO:
	# 1) figure out the coordinates that correspond to that block
	# 2) set entire corresponding block to black, not just top-left coordinate

	imgWithBlocks <- img  # make copy of the image

	for (frequentOffset in frequentOffsets) {
		coordinate1 <- frequentOffset[3:4]
		coordinate2 <- frequentOffset[5:6]

		# browser()

		imgWithBlocks[coordinate1[[1]]:(coordinate1[[1]]+blockWidth-1),
					  coordinate1[[2]]:(coordinate1[[2]]+blockHeight-1),] <- 0  # c(1,1,0)
		imgWithBlocks[coordinate2[[1]]:(coordinate2[[1]]+blockWidth-1),
					  coordinate2[[2]]:(coordinate2[[2]]+blockHeight-1),] <- 0  # c(1,1,0)
	}

	return(imgWithBlocks[,,channelValue])
	# TODO: Change it to blocks
	# not just top left
} #end of channelPca