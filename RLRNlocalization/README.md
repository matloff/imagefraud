Use this code to localize tampered regions of the image using RLRN.

PLEASE NOTE:
The box size of the is 512 for these large 1024 pixels (or greater) for now so use these large size images for testing:

Link to databases: https://www5.cs.fau.de/research/data/image-manipulation/

V. Christlein, C. Riess, J. Jordan, C. Riess, E. Angelopoulou: "An Evaluation of Popular Copy-Move Forgery Detection Approaches", 
IEEE Transactions on Information Forensics and Security, vol. 7, no. 6, pp. 1841-1854, 2012.

images but more features will be added to test this on smaller images (with smaller blocks) and to use smaller blocks within
the large blocks to further identify the exact shape of the tampering.

Results:
Using 10 test images , copied regions are located in each image.

These (Initial) test image outputs are located in the folder.

How it works:

This is an experimental method I made up (based on the RLRN 0/1 tampered/not code) and fact that theoretically tampered
regions themselves should be what will produce different image features if the detection accuracy on the whole image is so high.
