# Noise Analysis

The addition of noise is a common method used by image forgers to conver up the tampered region.
This is actually one of the main reasons the image detection software fails.
The noise level will be uniform across an untampered image while, forged images will have local inconsistencies
This noise analysis method divides an image into blocks, 
and measures changes in noise level versus the uniform level (noise variance).
