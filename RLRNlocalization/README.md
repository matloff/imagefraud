Use this code to localize tampered regions of the image using RLRN.


How to use: input the tampered and authentic directory folder file paths, and number of features (as in the original RLRNpar.R) BUT make sure each image corresponds exactly in order with its tampered version and authentic version in each folder. For example, number each authetic image as the same name as the tampered version. Use these to train and then a value for input numTest as a smaller number than the train set (# images in each folder). Additionally, add another file folder path which will be used by the program to store your testbimages with both the predicted localized region and the actual localized region (for comparison).

Use the "scaled down version" of the original and spliced image databases from: https://www5.cs.fau.de/research/data/image-manipulation/

V. Christlein, C. Riess, J. Jordan, C. Riess, E. Angelopoulou: "An Evaluation of Popular Copy-Move Forgery Detection Approaches", 
IEEE Transactions on Information Forensics and Security, vol. 7, no. 6, pp. 1841-1854, 2012.

I will upload the folders with the images in proper format, into this directory.

The code file also includes example optional inputs for pfeatures and thresh (increasing thresh will increase the number of boxes covering the tampered region if 1 or none were found in a large tampered region).


Features that will be added SOON:
More features will be added to test this on smaller images (with smaller blocks) and to use smaller blocks within
the large blocks to further identify the exact shape of the tampering (since tests show this works as well). The parallel version is also in progress.


An example output is located in the example_output_images folder.

How it works:

This is an experimental method I made up (based on the RLRN 0/1 tampered/not code) and fact that theoretically tampered
regions themselves should be what will produce different (tampered) image features (if the detection accuracy on the whole image is 98% on the whole image as shown in RLRNpar.R.)


