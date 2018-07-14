# RLRN Detection
This is code uses a chroma-based method of feature analysis using Run Length Run Number (RLRN) endcoding.
It trains an SVM (Support Vector Machine) based on this feature array from a user input 
of an entire database of images. (For example, the SVM trained with only 220 images will then be able to detect whether an input image is has any type of image fraud or not with 97% accuracy.)

Example input:
please, input the directory of the authentic and tampered training images as string:

authenticDir <- "/Users/robinyancey/desktop/authentic"
tamperDir <- "/Users/robinyancey/desktop/tampered"

- to increase the number of features per each of the 4 directions (and total features per channel)
- increase 'pfeatures' variable below (increase V61 to V(4*pfeatures))

pfeatures <- 15


- to test on a cluster, set par to the number of worker nodes (par <- 0 is serial)
- NOTE: please, make sure the number of images is divisible by the cluster size
par <- 4

print(system.time(testRLRNpar(authenticDir, tamperDir, pfeatures, par)))

A feature is currently being added to use the (already created) feature vectors from the forged images found by the SVM, to
find where in the image the fraud is detected (as the other algorithms do). 

Why RLRN works:
Since these features combine the color information as well as the shape of objects in an image, it is able to
characterize the direction, area and geometrical shape of an object. 

Parallelization feature: 
The extracting the run-length feature is time-consuming (since it must process every pixel in an entire image for 
each feature vector,) an input par of 2 or greater will run the feature a cluster. A super linear speedup was obtained witha. cluster of 4 lowering computational time of feature vectors from over and hour (in serial) to only a minute and a half 
(so running in parallel is reccomended.)


The code was tested on a sample of 220 images from the CASIA database:

Credits for the use of the CASIA Image Tempering Detection Evaluation Database (CAISA TIDE) V2.0 are given to the National Laboratory of Pattern Recognition, Institute of Automation, Chinese Academy of Science, Corel Image Database and the photographers. http://forensics.idealtest.org"

The initial code is based on the paper cited below:

X. Zhao, J. Li, S. Li, and S. Wang, “Detecting digital image splicing in chroma spaces,” in Digital Watermarking, vol. 6526 of Lecture Notes in Computer Science, pp. 12–22, Springer, Berlin, Germany, 2011
