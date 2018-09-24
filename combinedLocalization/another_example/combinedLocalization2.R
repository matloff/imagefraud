image<- '/Users/robinyancey/desktop/Tp_D_NNN_M_N_ani10132_ani10123_12477.jpg'
thresh <- 0.25

combinedLocalization <- function(image,thresh){
im_out <- BAGlocalization(image)
display(im_out)
im_out2 <- ELAlocalization(image, Q=0.95, scale=25)
display(im_out2)

im_out2 <- im_out2[1:nrow(im_out),1:ncol(im_out)]
im_out3 <- im_out + im_out2
display(im_out3)

# this is just example input
boxside <- 8
#thresh <- 0.25
im_out4 <- rmBoxAvErrors(im_out3, boxside, thresh)
display(im_out4)

# this is just example input
boxside <- 16
#thresh <- 0.25
im_out5 <- rmBoxAvErrors(im_out4, boxside, thresh)
display(im_out5)

thresh <- 0.065
im_out6 <- zeroOne(im_out5, thresh)

display(im_out6)
im_out6
}

im_out <- combinedLocalization(image,thresh)
display(im_out)
