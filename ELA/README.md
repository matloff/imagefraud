# Error Level Analysis

When lossy compression is applied uniformly to an image we can see the uniform levels of compression artifacts. 
This is due to the different parts of the image having been subjected to the lossy compression multiple times or
different levels of compression (see https://en.wikipedia.org/wiki/Error_level_analysis).

# Method
First the image is divided into 8×8 blocks and each is recompressed independently at a known quality factor (such as 95%). 
The error level of an image is found from the amount of the loss in information when it is saved in the JPEG format 
(compressed). When measuring the error in the difference between the original image and the compressed image, 
each block should have approximately the same amount if the image is completely unmodified. 

# Sources
The code was tested on images from the CASIA database:

Credits for the use of the CASIA Image Tempering Detection Evaluation Database (CAISA TIDE) V2.0 are given to the National Laboratory of Pattern Recognition, Institute of Automation, Chinese Academy of Science, Corel Image Database and the photographers. http://forensics.idealtest.org"

N. B. A. Warif, M. Y. I. Idris, A. W. A. Wahab and R. Salleh, "An evaluation of Error Level Analysis in image forensics," 2015 5th IEEE International Conference on System Engineering and Technology (ICSET), Shah Alam, 2015, pp. 23-28.

https://fotoforensics.com/tutorial-ela.php
