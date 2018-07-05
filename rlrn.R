library(jpeg)

# Plots given image. Can be given a single slice (Cr channel),
# but will only do color if given three (RGB) slices.
plotImage <- function(img,imgWidth,imgHeight)
{
	plot.new()
	plot.window(xlim=c(0,imgWidth), ylim=c(0,imgHeight))
	rasterImage(img, xleft=0, xright=imgWidth,
		ybottom=0,ytop=imgHeight)
}

# Expects 3d matrix. Returns 2d matrix of the luma component
# of given image.
generateLumaSlice <- function(img,imgWidth,imgHeight)
{
	slice <- matrix(0,nrow=imgHeight,ncol=imgWidth)
	for (rowIndex in 1:imgHeight)
	{
		for (colIndex in 1:imgWidth)
		{
			redVal <- img[rowIndex,colIndex,1]
			greenVal <- img[rowIndex,colIndex,2]
			blueVal <- img[rowIndex,colIndex,3]
			slice[rowIndex,colIndex] <-
				0.299 * redVal + 0.587 * greenVal
				+ 0.114 * blueVal
		}
	}
	return(slice)
}

# Expects 3d matrix. Returns 2d matrix of the blue-difference
# chroma components of given image.
generateCbSlice <- function(img,imgWidth,imgHeight)
{

}

# Expects 3d matrix. Returns 2d matrix of the red-difference
# chroma components of given image.
generateCrSlice <- function(img,imgWidth,imgHeight)
{

}

rlrn <- function(imgName)
{
	img <<- readJPEG(imgName)
	imgDim <- dim(img)
	imgWidth <- imgDim[2]
	imgHeight <- imgDim[1]
	# plotImage(img,imgWidth,imgHeight)
	lumaSlice <<- generateLumaSlice(img,imgWidth,imgHeight)
	plotImage(lumaSlice,imgWidth,imgHeight)
}