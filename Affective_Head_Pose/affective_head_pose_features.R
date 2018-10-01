#### This file takes a all of the .csv files in a directory, calculates LLD and the statistical functionals
#### on X second segments of the file, and outputs the results to CSV file format. 
#
# Copyright: Athlone Institute of Technology, 2018
# Author: j.odwyer@research.ait.ie

#### Libraries ####
library(data.table)                         # used for fast table manipulation

#### Functions ####
setwd("~/YOUR/DIRECTORY/Affective_Head_Pose")  # main functions directory
source("R/head_pose_features_calculation.R")  # used to calculate statistics for raw eye gaze data

#### Variables ####
feature_calculation_window_size <- 8

setwd("~/CSV/input_files/location")   #  Set directory to the input files folder 
csv_file_list = list.files(pattern="*.csv")
mytable <- fread(csv_file_list[1])
frames_per_second <- floor(1 / mytable[2, timestamp]) # f = 1/t

for(counter in 1:length(csv_file_list))
{
  mytable <- fread(csv_file_list[counter])
  mytable <- mytable[,c(1,3:(length(mytable)-6)):=NULL]   #  Remove data unrelated to head pose
  setwd("~/CSV/output_files/location")  #  Set to the directory that you want to provide data output to
  
  # Calculate the features and write the data output in ARFF format
  filename = paste(csv_file_list[counter], feature_calculation_window_size, "s_results.csv", sep = "")
  file = calculate_features(mytable, frames_per_second, feature_calculation_window_size)
  write.csv(file, filename)
  
  setwd("~/CSV/input_files/location")  #  Set directory to the input files folder again
}

