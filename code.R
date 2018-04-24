library(jpeg)
library(scales)

faridPca <- function(img)
{
	# convert image to its matrix representation 
	img <- readJPEG(img)

	imgDim <- dim(img)
	N <- imgDim[1] * imgDim[2]

	b <- 64

	N_b <- (sqrt(N) - sqrt(b) + 1)^2

	e <- 0.01
	Q <- 256
	N_n <- 100
	N_f <- 128
	N_d <- 16

	# for now, set arbitrarily
	# TODO: change this
	N_t <- 8

	# extract the individual color value matrices
	# to perform PCA on each 
	redChannel <- img[,,1]
	greenChannel <- img[,,2]
	blueChannel <- img[,,3]

	# To help the upcoming double for loop.
	topLeftX <- 1
	topLeftY <- 1
	imgWidth <- dim(img)[2]
	blockWidth <- sqrt(b)
	numSlidingBlocksPerRow <- imgWidth - blockWidth + 1
	imgHeight <- dim(img)[1]
	blockHeight <- sqrt(b)
	numSlidingBlocksPerCol <- imgHeight - blockHeight + 1

	# Init matrix where each row represents a block as a row vector.
	# The max size of a row is whatever the max size of N_t is (i.e. b).
	# Have two extra spots in each row to hold the x and y coordinate of the
	# top left of the corresponding block (top left of image is (0,0)).
	blocks <- matrix(0, nrow = N_b, ncol = b + 2)

	blockIndex <- 1
	for (rowIndex in 1:numSlidingBlocksPerCol)
	{
		for (colIndex in 1:numSlidingBlocksPerRow)
		{
			redBlock <- redChannel[rowIndex:(rowIndex+blockHeight-1),
			                       colIndex:(colIndex+blockWidth-1)]
			redBlockPca <- prcomp(redBlock, center = FALSE)
			redBlockCompressed <- as.vector(
				redBlockPca$x[,1:N_t] %*% t(redBlockPca$rotation[,1:N_t]))
			# print(redBlockCompressed[1])
			# print(redBlockCompressed[2])
			# print(redBlockCompressed[1] == redBlockCompressed[2])
			# redBlockQuantized <- as.vector(round(rescale(redBlockCompressed,
			# 	 to = c(0, q))))
			# redBlockQuantized <- floor(redBlockCompressed / Q)
			# blocks[blockIndex,1:b] <- redBlockQuantized
			blocks[blockIndex,(1:b)] <- as.vector(redBlockCompressed)

			# Store this block's coordinate.
			blocks[blockIndex,(b+1):(b+2)] <- c(rowIndex,colIndex)

			blockIndex <- blockIndex + 1
		}
	}

	S <- blocks[do.call(order, lapply(1:(b+2), function(i) blocks[,i])),]

	# Note that the last two coordinates of each row in S are the
	# coordinates of the block corresponding to that row.
	# offsets stores every pair of coordinates of rows that are within
	# N_n of each other.
	coordinatePairs <- list()

	numCoordinatePairs <- 0
	for (rowIndex in 1:(nrow(S)-N_n))
	{
		for (rowAppend in 1:(N_n-1))
		{
			numCoordinatePairs <- numCoordinatePairs + 1

			#first is coordinates of block1
			#second is coordinates of block2
			coordinatePairs[[numCoordinatePairs]] <- list(first=S[rowIndex,(b+1):(b+2)],
				second=S[rowIndex+rowAppend,(b+1):(b+2)])
		}
	}

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

		if (sqrt((offset[[1]])^2 + (offset[[2]])^2) >= N_d) {
			# Add the offset.
			numOffsets <- numOffsets + 1
			offsets[[numOffsets]] <- offset
		}
	}

	# print(offsets)

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
			# print("numFrequentOffsets")
			# print(numFrequentOffsets)
			# print("previousOffset")
			# print(previousOffset)
			# print("frequentOffsets")
			# print(frequentOffsets)
			if (offsetFrequency >= N_f) {
				numFrequentOffsets <- numFrequentOffsets + 1
				frequentOffsets[[numFrequentOffsets]] <- previousOffset
			}

			# Reset for this new offset.
			offsetFrequency <- 0
		}
		previousOffset <- offset
	}

	print("frequentOffsets")
	print(frequentOffsets)

	# TODO:
	# 1) figure out the coordinates that correspond to that block
	# 2) set entire corresponding block to black, not just top-left coordinate

	imgWithBlocks <- img  # make copy of the image

	for (frequentOffset in frequentOffsets) {
		coordinate1 <- frequentOffset[3:4]
		coordinate2 <- frequentOffset[5:6]

		# browser()

		imgWithBlocks[coordinate1[[1]],coordinate1[[2]],] <- 0
		imgWithBlocks[coordinate2[[1]],coordinate2[[2]],] <- 0
	}

	# print(imgWithBlocks)
	# plot(imgWithBlocks)
	plot.new()
	plot.window(xlim=c(0,imgWidth), ylim=c(0,imgHeight))
	rasterImage(imgWithBlocks, xleft=0, xright =imgWidth, ybottom=0,ytop=imgHeight)
	# points(imgWithBlocks)
}