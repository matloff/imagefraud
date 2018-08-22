Use this code to localize tampered regions of the image using RLRN.

PLEASE NOTE:
The box size of the is 512 for these large 1024 pixels (or greater) for now so use these large size images for testing:

Link to databases: https://www5.cs.fau.de/research/data/image-manipulation/

V. Christlein, C. Riess, J. Jordan, C. Riess, E. Angelopoulou: "An Evaluation of Popular Copy-Move Forgery Detection Approaches", 
IEEE Transactions on Information Forensics and Security, vol. 7, no. 6, pp. 1841-1854, 2012.

Features that will be added VERY SOON:
More features will be added to test this on smaller images (with smaller blocks) and to use smaller blocks within
the large blocks to further identify the exact shape of the tampering (since tests show this works as well).

How to use: input the tampered and authentic directory folder file paths, and number of features (as in the original RLRNpar.R) BUT make sure each image corresponds exactly in order with its tampered version and authentic version in each folder. For example, number each authetic image as the same name as the tampered version. (I will try to upload folders of test images for each RLRN code). Use these to train and then a value for input numTest as a smaller number than the train set (# images in each folder). Additionally, add another file folder path which will be used by the program to store your testbimages with both the predicted localized region and the actual localized region (for comparison).

Results:
Using 10 test images , copied regions are located by exactly covering the tampered region with black boxes or mostly in each image.

An example output is located in the outputImages folder.

How it works:

This is an experimental method I made up (based on the RLRN 0/1 tampered/not code) and fact that theoretically tampered
regions themselves should be what will produce different (tampered) image features if the detection accuracy on the whole image is 98% on the whole image as shown in RLRNpar.R.


