# bioimagetools

<!-- badges: start -->
  [![CRAN status](https://www.r-pkg.org/badges/version/bioimagetools)](https://CRAN.R-project.org/package=bioimagetools)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/bioimagetools)](https://cran.r-project.org/package=bioimagetools)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/grand-total/bioimagetools)](https://cran.r-project.org/package=bioimagetools)
<!-- badges: end -->
  
## Synopsis

bioimagetools is an R package for working with images in 3D and 4D, mostly for biology/microscopy. 

* Read and write TIFF stacks. 
* Functions for filtering and segmentation.
* Analysis of 3D point patterns.

## Installation

The stable version is available on [CRAN](https://cran.r-project.org/):

    setRepositories(ind=c(1,2))
    install.packages("bioimagetools")

The development version is available on [github](https://github.com/bioimaginggroup): 

    setRepositories(ind=c(1,2))
    install.packages(c("devtools","tiff","EBImage"))
<<<<<<< HEAD
    remotes::install_github("bioimaginggroup/bioimagetools")
=======
    remotes::install_github("volkerschmid/bioimagetools")
>>>>>>> bf6b1b594b484f76b914a464e07208b12acb72e4

You may need to install additional libraries on your OS before you can install bioimagetools. E.g. on Ubuntu/Debian systems, please execute

    sudo apt install libssl-dev libcurl4-openssl-dev libtiff5-dev libfftw3-dev
in the terminal before installing bioimagetools.

## Contributors

This package is developed at the BioImaging group at the Department of Statistics, in cooperation with the Biocenter, Department of Biology II, both at LMU Munich.

* Main development and implementation: Volker J Schmid
* Biological expertise: Marion and Thomas Cremer, Barbara Hübner, Yolanda Markaki, Jens Popken, Lothar Schermelleh, Daniel Smeets
* Alpha testing: Priyanka Kukreja, Ramin Norousi and Marius Wagner
* Contributions by Priyanka Kukreja and Fabian Scheipl
