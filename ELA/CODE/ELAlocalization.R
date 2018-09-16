ELAlocalization <- function(image, Q, scale){
require(jpeg)
require(matlab)
  
im1<- readJPEG(image)


r<-writeJPEG(im1, raw(), quality=Q)
im2 <- readJPEG(r)

im_out<-(abs(im1-im2)*scale)

im_out <-apply(im_out, c(1,2), mean)

imagesc(im_out)
}

image<- '/Users/robinyancey/desktop/Tp_D_NNN_M_N_ani10132_ani10123_12477.jpg'
#image<- '/Users/robinyancey/desktop/Tp_D_NRN_S_N_ani10210_ani10209_12373.jpg'
ELAlocalization(image, Q=0.95, scale=25)
