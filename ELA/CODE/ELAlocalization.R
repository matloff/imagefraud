ELAlocalization <- function(image, Q=0.95, scale=25, ratio=0.3){

require(jpeg)
  
im1<- readJPEG(image)

r<-writeJPEG(im1, raw(), quality=Q)

im2 <- readJPEG(r)

im_out<-(abs(im1-im2)*scale)

im_out <-apply(im_out, c(1,2), mean)

im_out[(im_out < ratio)] <- 0
im_out[(im_out > ratio)] <- 1
display(t(im_out))

  im_out
}
image<- '/Users/robinyancey/desktop/Tp_D_NNN_M_N_ani10132_ani10123_12477.jpg'
#image<- '/Users/robinyancey/desktop/Tp_D_NRN_S_N_ani10210_ani10209_12373.jpg'
Q <- 0.95
scale <- 25
r <- 0.3
ELAlocalization(image, Q, scale, r)
