zeroOne <- function(im_out, thresh){
  
size <-round(ncol(im_out)*nrow(im_out)*thresh)
elements <- tail(sort(im_out), size)
minVal <- elements[1]
im_out[(im_out < minVal)] <- 0
im_out[(im_out > minVal)] <- 1

im_out

}

