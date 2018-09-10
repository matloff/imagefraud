# maybe will make filter length input

BAGlocalization <- function(image){
  require(EBImage) # load image
  require(mrbsizeR) # DCT
  require(pracma) # histogram 
  require(smoother) # gaussian filter
  require(Rlibeemd) # extrema
  require(matlab) # display heat map image
  
  im1<- readImage(image) 
  
  # reduce to one dimension with one byte values
  red.weight<- .2989; green.weight <- .587; blue.weight <- 0.114
  im <- red.weight * imageData(im1)[,,1] + green.weight * imageData(im1)[,,2] + blue.weight  * imageData(im1)[,,3]
  # make sure image dimensions are a multiple of 8
  im <- im[1:(floor(nrow(im)/8)*8), 1:(floor(ncol(im)/8)*8)]*255
  # scale to around 0
  im <- im-128
  
  # DCT block multiply by 8x8 image blocks(as done in JPEG compression):
  # (make sure the library function dctMatrix is not overwritten by personal function!)
  T <- dctMatrix(8)
  boxside <- 8
  width <- nrow(im)
  height <- ncol(im)
  for (i in seq(1, (width-boxside+1), by=(boxside))){
    for (j in seq(1, (height-boxside+1), by=(boxside))){
      endw <- i+(boxside-1)
      endh <- j+(boxside-1)
      im_block <- im[i:endw,j:endh]
      im[i:endw,j:endh] <- round(T %*% im_block %*% t(T))
    }
  }
  
  #stack DC-Transformed blocks in order of image (to put image back together at the end)
  blockdct <- array(im, c(8, round(dim(im)[1]/8), 8, round(dim(im)[2]/8)) )
  blockdct <- aperm(blockdct, c(1, 3, 2, 4))
  blockdct <- array(blockdct, c(8, 8, round(dim(im)[1]*dim(im)[2]/64)))
  
  # create the scaled histogram of each of the 64 coefficients in the DCT block 
  someData <- rep(NaN, 8*8*515) 
  hists <- array(someData, c(8, 8, 515))
  for (i in 1:8){
    for (j in 1:8){
      hists[i,j,] <- histc(blockdct[i,j,],-257:257)$cnt
    }}
  
  # estimate Q values based on periodicity of histogram:
  Q <- matrix(0,nrow=8, ncol=8)
  # these are the indices to make the zig zag path through each block used in 
  # JPEG compression (may want to double check this or add more coefficients)
  rows <- c(1,2,1,1,2,3,4,3,2,1,1,2,3,4,5,6,5,4,3,2,1,1,2,3,4,5,6,7,8,7,6,5)
  cols <- c(1,1,2,3,2,1,1,2,3,4,5,4,3,2,1,1,2,3,4,5,6,7,6,5,4,3,2,1,1,2,3,4)
  
  for (coeff in 2:32){  
    none <- F # in case there are no peaks
    
    row <- rows[coeff]
    col<- cols[coeff]
    
    onehist <- array(hists[row,col,],c(1,dim(hists)[3]))
    
    # magnitude of the frequencies of the histogram ( or power spectrum )
    # so we can take the derivative to get the values at the 
    # peaks which can be used to estimate Q
    ffthists <- abs(fft(onehist))
    
    ffthists <- ffthists[12:503]
    # this takes the difference in the value from the previous value twice (2nd deriv)    
    secondDiff <- diff(diff(ffthists))
    
    if (length(secondDiff) == 0){
      secondDiff <- 0
    }
    # low pass filter and remove positive values    
    options('smoother.gaussianwindow.alpha'=6,'smoother.window'=45, 'smoother.tails'=T)
    filter  <- smth.gaussian(secondDiff, window = getOption("smoother.window"), alpha = getOption("smoother.gaussianwindow.alpha"),tails = getOption("smoother.tails"))
    filter[ filter > min(filter)/10 ] <- 0
    
    ext <- extrema(filter)
    
    if (!none){
    # the number of extreme values in the filtered histogram of DCT can be used to find the 
    # estimated Q table coefficient
    Q[row,col] <- length(ext$minima[,1])-2
    }
    
  }
  # make a Q table for each of the stacked blocks to use in artifact eqn.
  Q2<- array(Q,c(8,8,dim(blockdct)[3]))
  
  # formula to get block artifact
  imblocks <- abs( blockdct - (Q2*round(blockdct/Q2)) )
  imblocks[is.nan(imblocks)] <- 0 # in case 0/0
  # sum the total amount of artifacts for each block
  imblocks2 <- NULL
  for (s in 1:(dim(blockdct)[3])){
    imblocks2[s] <- sum(sum(imblocks[1:8,1:8,s]))}
  # reformulate blocks (with artifacts) into compressed image 
  imblocks <- t(matrix(imblocks2, c(dim(im)[1]/8, dim(im)[2]/8)))
  # display image as heat map
  imagesc(x=seq(ncol(imblocks)), y=seq(nrow(imblocks)), imblocks, col=jet.colors(128))
}

#image<- '/Users/robinyancey/desktop/Tp_D_NNN_M_N_ani10132_ani10123_12477.jpg'
#image<- '/Users/robinyancey/desktop/Tp_D_CND_S_N_ani00073_ani00068_00193.tif'
image<- '/Users/robinyancey/desktop/Tp_D_NRN_S_N_ani10210_ani10209_12373.jpg'

BAGlocalization(image)
