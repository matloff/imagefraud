# imagefraud Directories:

RLRNpar folder: This is code uses a chroma-based method of feature analysis using Run Length Run Number (RLRN) endcoding. It trains a GLM model  based on this feature array from a user input of an entire database of images. (For example, when trained with only 220 images it will then be able to detect whether an input image is has any type of image fraud or not with 97% accuracy.) (see README with subdirectory for more information)

 - NOTE: please, use this code for detecting just whether an image is fraudulent or not as it is the fastest, easiest to use and most accurate
 
BAGlocalization folder: This code uses the Block Artifact Grid to measure differences in the compression rate and quality factor of image blocks used in JPEG compression. Those blocks with a different factor are highlighted in red/yellow in the output heat map.

-NOTE: please, use this code to localize tampered regions of an imaged that is determined by RLRNpar.R to be spliced by inserting a piece of another image (not by copy-paste from the same image).

pcaCProbust folder: contains a working combined intensity based and PCA algorithm (under the colder folder) for image fraud,
as well as multiple test images and output (see README with subdirectory for more information)
 
 - NOTE: please, use this code for localizing the actual copied region (of images with copy-paste forgery) as it is the fastest, easiest to use and most accurate
 
RLRNlocalization folder: contains an experimental working code for localizing tampered regions based on RLRN (see README with subdirectory for more information)

 - NOTE: please, use this code for localizing tampered regions that are NOT copy-paste image fraud (tampered regions inserted from another image)
 
dctCPtested folder: contains a working DCT quantization algorithm (under the colder folder) for copy-paste image fraud,
as well as multiple test images and output (see README with subdirectory for more information)

# NEXT TASK:

(Finish) implementing the paper here: https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=7084319
(which also uses blocking and database model training, but a more sophisiticated method of feature generation) and compare this to RLRN (and/or use it for improving it)

