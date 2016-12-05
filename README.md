#bioimagetools

## Synopsis

bioimagetools is an R package for working with images in 3D and 4D, mostly for biology/microscopy. 

* Read and write TIFF stacks. 
* Functions for filtering and segmentation.
* Analysis of 3D point patterns.

## Installation

    setRepositories(ind=c(1,2))
    install.packages(c("devtools","spatstat","tiff","EBImage"))
    devtools::install_github("volkerschmid/bioimagetools")

## Contributors

This package is developed at the BioImaging group at the Department of Statistics, LMU Munich.