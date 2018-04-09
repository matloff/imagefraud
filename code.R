library(jpeg)

popSkewFreed <- function(img)
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
	N_t <- 3

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
	blocks <- matrix(0, N_b, b)

	blockIndex <- 1
	for (rowIndex in 1:numSlidingBlocksPerCol)
	{
		for (colIndex in 1:numSlidingBlocksPerRow)
		{
			redBlock <- redChannel[rowIndex:(rowIndex+blockHeight-1),
			                       colIndex:(colIndex+blockWidth-1)]
			redBlockPca <- prcomp(redBlock, center = FALSE)
			# print("dim(redBlockPca):")
			# print(dim(redBlockPca$x))
			# print(dim(redBlockPca$rotation))
			redBlockCompressed <- as.vector(redBlockPca$x[,1:N_t] %*%
				t(redBlockPca$rotation[,1:N_t]))
			redBlockQuantized <- floor(redBlockCompressed / Q)
			blocks[blockIndex,] = redBlockQuantized

			blockIndex <- blockIndex + 1
		}
	}

	print(blocks[1:5,1:5])
}