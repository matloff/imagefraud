
heights <- c(60, 67, 68, 72)  # in inches
weights <- c(100, 155, 150, 200)  # in pounds
sampleSize <- length(heights)

sample <- matrix(c(height=heights,weight=weights),nrow=sampleSize,ncol=2)
p <- prcomp(sample)
