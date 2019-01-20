# imagefraud Directories:

ela_2ndstrm_FasterRCNN: This code uses a library for Faster-RCNN (Faster Region Proposal - Convolutional Neural Network: one of the most popular object detection methods in machine learning) using the GPU version of Tensorflow. It was found that object detection networks can be trained to detect tampered regions (instead of objects), and furthermore that using two streams (bi-linearly pooled, and one with pre-filtered input images) (will cite paper) improves accuracy.  In my experiments I tested using ELA on the images input to the second stream (instead of noise filtering like in the paper), and I have recieved improved detection accuracy over the single stream RGB network.

 RLRNpar folder: This is code uses a chroma-based method of feature analysis using Run Length Run Number (RLRN) endcoding. It trains a GLM model  based on this feature array from a user input of an entire database of images. (For example, when trained with only 220 images it will then be able to detect whether an input image is has any type of image fraud or not with 97% accuracy.) (see README with subdirectory for more information)

 - NOTE: please, use this code for detecting just whether an image is fraudulent or not as it is the fastest, easiest to use and most accurate
 
BAGlocalization folder: This code uses the Block Artifact Grid (BAG) to measure differences in the compression rate and quality factor of image blocks used in JPEG compression. Those blocks with a different factor are highlighted in red/yellow in the output heat map.

ELA folder: contains a code to run an "Error Level Analysis" on JPEG images to determine the difference in compression levels throughout the image and localize forgery.

combinedLocalization folder: contains a code to combine BAG extraction and ELA with calls to rmBoxAvErrors.R and zeroOne.R to produce a highly localized output image. rmBoxAvErrors.R removes boxes with average errors less than a threshold to remove flas positives, while zeroOne.R writes the leftover pixels to 1 or 0 using another threshold.

- NOTE: please use this code to localize image forgery after determining the image was forged (and converting to JPEG) to help get the best localization approximation. First, be sure the image is determined by RLRNpar.R to be spliced by inserting a piece of another image (not by copy-paste from the same image).

 pcaCProbust folder: contains a working combined intensity based and PCA algorithm (under the colder folder) for image fraud,
as well as multiple test images and output (see README with subdirectory for more information)
 
 - NOTE: please, use this code for localizing the actual copied region (of images with copy-paste forgery) as it is the fastest, easiest to use and most accurate
 
 RLRNlocalization folder: contains an experimental working code for localizing tampered regions based on RLRN (see README with subdirectory for more information)

 - NOTE: please, use this code for localizing tampered regions that are NOT copy-paste image fraud (tampered regions inserted from another image)
 
 dctCPtested folder: contains a working DCT quantization algorithm (under the code folder) for copy-paste image fraud,
as well as multiple test images and output (see README with subdirectory for more information)



