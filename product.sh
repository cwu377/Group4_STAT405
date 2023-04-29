#!/bin/bash

# untar your R installation
tar -xzf R413.tar.gz
tar -xzf packages.tar.gz
#tar -xzf packages_FITSio_tidyverse.tar.gz

#tar -xzf tidytext_0.4.1.tar.gz
#tar -xzf sentimentr_2.9.0.tar.gz

# make sure the script will use your R installation, 
# and the working directory as its home location
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages

# run your script
Rscript semanticsProject.R $1

