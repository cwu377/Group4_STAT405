rm(list=ls())
args = (commandArgs(trailingOnly=TRUE))

if (require("tidyverse")) {
  print("Loaded package tidyverse.")
} else {
  print("Failed to load package tidyverse.")  
}

data <- read_tsv(args[1])
print(head(data))
