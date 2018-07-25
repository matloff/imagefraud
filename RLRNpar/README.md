# RLRN Detection
This is code uses a chroma-based method of feature analysis using Run Length Run Number (RLRN) endcoding.
It trains an SVM (Support Vector Machine) based on this feature array from a user input 
of an entire database of images. (For example, the SVM trained with only 220 images will then be able to detect whether an input image is has any type of image fraud or not with 97% accuracy.)

*UPDATE: GLM takes only a few seconds to compute and was found to have better accuracy then SVM which takes over 2 hours to compute (using 10k test image feature arrays, trained on 18832 image feature vectors or 9416 different images), so GLM results are output first) 
-note: to use SVM on the entire CASIA database as above use a parallel cluster of 16 to compute in minutes with only a slightly lower accuracy


EXAMPLE INPUT:

Please, input the directory of the authentic and tampered training images as string:

authenticDir <- "/Users/robinyancey/desktop/authentic"
tamperDir <- "/Users/robinyancey/desktop/tampered"

- to increase the number of features per each of the 4 directions (and total features per channel)
- increase 'pfeatures' variable below 

pfeatures <- 15

- (optional) test whether images (which are unknown) are predicted fraudulent or not based on trained SVM
- note: the SVM has not been tuned/trained for the new images so they accuracy of predicition may be lower

newImagesDir <- "/Users/robinyancey/desktop/newImages"

- to test on a cluster, set par to the number of worker nodes (par <- 0 is serial)
- NOTE: please, make sure the number of images is divisible by the cluster size

par <- 4

Parallelization feature: 
The extracting the run-length feature is time-consuming (since it must process every pixel in an entire image for 
each feature vector,) an input par of 2 or greater will run the feature a cluster. A super linear speedup was obtained with a cluster of 4 lowering computational time of feature vectors from over and hour (in serial) to only a minute and a half 
(so running in parallel is reccomended.) SVM is also parallel when images > 700 (so that Software Alchemy is usable).



CITATIONS:

The code was tested on a sample of 220 images from the CASIA database:

Credits for the use of the CASIA Image Tempering Detection Evaluation Database (CAISA TIDE) V2.0 are given to the National Laboratory of Pattern Recognition, Institute of Automation, Chinese Academy of Science, Corel Image Database and the photographers. http://forensics.idealtest.org"

The initial code is based on the paper cited below:

X. Zhao, J. Li, S. Li, and S. Wang, “Detecting digital image splicing in chroma spaces,” in Digital Watermarking, vol. 6526 of Lecture Notes in Computer Science, pp. 12–22, Springer, Berlin, Germany, 2011

WHY IT WORKS:

Image splicing usually creates abrupt changes around certain objects or regions such as lines, edges and corners (which are sharper and rougher than regular lines, edges and corners) due to the copy-and-pasting. 

The technique was first used for texture analysis and stegenalysis to detect hidden messages in images.

For example, a constant texture is produced if local statistics are constant, slowly varying or approximately periodic (see below).

Mir AH.; Hanmandlu, M.; Tandon, S.N., "Texture analysis of CT images," Engineering in Medicine and Biology Magazine, IEEE , vol.14, no.6, pp.781,786, Nov/Dec 1995.


