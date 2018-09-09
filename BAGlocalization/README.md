# Block Artifact Grid Localization

- Block Artifact Grid localization uses the difference in the JPEG compression rates in image blocks to estimate the locations with a high amount of artifacts which are located in the specific blocks in point of forgery 
 
- This method is based on the JPEG compression technique (see https://en.wikipedia.org/wiki/JPEG)

# How to use:
To use this code simply imput the file path in quotes into the function on the file in the "CODE" folder in this directory.

There are lso a number of test images and their output for examples in this directory.

# Method Explaination

The following steps can be used to localize forgery:
1. divide the image into 8x8 blocks (as used in JPEG compression)

2. take the DCT of the blocks (using an 8x8 DCT matrix and matrix multiply as done in JPEG compression) 

3. make a histogram of the (quantized between -257 to 257) DCT values for each of the 64 locations of 8x8 blocks (the number of blocks is equal to the number that can fit into the image (eg. round(nrow/8)*round(ncol/8)/ (8x8)) so the number of values in each histogram is equal to the number of blocks


4. take the FFT of the histogram of each of the 64 frequencies to get the periodicity and then power spectrum (absolute value) to get peaks 

5. calculate the number of local minimums of the filtered second derivative (extrema). This is the estimated Q value of the Q matrix

6. Once we get a Q estimate for at least 32 Q values in the matrix, we use the estimated Q to calculate the block artifact for each image block using the equation for the block artifact (see paper)

# Sources

(see https://en.wikipedia.org/wiki/JPEG)

The code was tested on images from the CASIA database:

Credits for the use of the CASIA Image Tempering Detection Evaluation Database (CAISA TIDE) V2.0 are given to the National Laboratory of Pattern Recognition, Institute of Automation, Chinese Academy of Science, Corel Image Database and the photographers. http://forensics.idealtest.org"

The algorithm was found in many sources but mainly written mainly using the following paper.

Ye, Shuiming & Sun, Qibin & Chang, Ee-Chien. (2007). Detecting Digital Image Forgeries by Measuring Inconsistencies of Blocking Artifact. Proceedings of the 2007 IEEE International Conference on Multimedia and Expo, ICME 2007. 12 - 15. 10.1109/ICME.2007.4284574. 
