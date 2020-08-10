#### This file takes a all of the .txt files in a directory, filters the files for LLDs, and provides output
#### of these LLDs to another CSV file for later processing.
#
# Copyright: Athlone Institute of Technology, 2020
# Author: j.odwyer@research.ait.ie

#### Libraries ####
library(data.table)

require(doMC)  #  Note, using doMC is machine (no. of cores) dependent
registerDoMC(cores = 2)  #  Increase this number with desired (and available) no. of cores as appropriate

#### Custom Functions ####  #  change directory path here, and in feature_calculation R file to suit your system
source("~/YOUR/DIRECTORY/R/head_pose_wavelet_features_calculation.R")  

#### Variables ####
window_size <- 8

setwd("~/CSV/input_files/location")
csv_file_list = list.files(pattern="*.csv")
mytable <- fread(csv_file_list[1])
frames_per_second <- floor(1 / mytable[2, timestamp]) # f = 1/t

calc <- function(file) {  #  function for parallel file processing
  
  mytable <- fread(file)
  # Remove data unrelated to head pose
  mytable <- mytable[,c(1,2,4:(length(mytable)-42), (length(mytable)-35):length(mytable)):=NULL]
  
  # Set to the directory that you want to provide data output to
  setwd("~/CSV/output_files/location")
  
  # Calculate the features and write the data output in CSV format
  filename = paste("Wavelet_", csv_file_list[i], window_size, "s_results.csv", sep = "")
  file = calculate_features(mytable, frames_per_second, window_size)
  write.csv(file, filename)
  
  # Set directory to the input files folder again
  setwd("~/CSV/input_files/location")
}

# Extract the features from files in parallel fashion
foreach(i=1:length(csv_file_list), .inorder=FALSE) %dopar%
  calc(csv_file_list[i])

