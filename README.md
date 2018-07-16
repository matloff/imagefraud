# imagefraud

RLRNpar folder: This is code uses a chroma-based method of feature analysis using Run Length Run Number (RLRN) endcoding. It trains an SVM (Support Vector Machine) based on this feature array from a user input of an entire database of images. (For example, the SVM trained with only 220 images will then be able to detect whether an input image is has any type of image fraud or not with 97% accuracy.) (see README with subdirectory for more information)

 - NOTE: please, use this code for detecting just whether an image is fraudulent or not as it is the fastest, easiest to use and most accurate

pcaCProbust folder: contains a working combined intensity based and PCA algorithm (under the colder folder) for image fraud,
as well as multiple test images and output (see README with subdirectory for more information)
 
 - NOTE: please, use this code for localizing the actual copied region as it is the fastest, easiest to use and most accurate
 
 
dctCPtested folder: contains a working DCT quantization algorithm (under the colder folder) for copy-paste image fraud,
as well as multiple test images and output (see README with subdirectory for more information)

pcaCPtested folder: contains a test code for comparison of the use of just PCA features for copy-paste forgery detection 
(which has shown to not be as effective as other methods in our work and other research papers).

