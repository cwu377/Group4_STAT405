rm(list=ls())
args = (commandArgs(trailingOnly=TRUE))

if (require("FITSio")) { # require() is like library(), but returns TRUE or FALSE
  print("Loaded package FITSio.")
} else {
  print("Failed to load package FITSio.")  
}

if (require("tidyverse")) {
  print("Loaded package tidyverse.")
} else {
  print("Failed to load package tidyverse.")  
}

data <- read_tsv(args[1])
