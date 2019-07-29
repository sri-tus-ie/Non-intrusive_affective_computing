#### This file takes a all of the .txt files in a directory, filters the files, calculates statistical functionals
#### on 2 second segments of the file, and outputs the results to ARFF file format. 
#
# Copyright: Athlone Institute of Technology, 2018
# Author: j.odwyer@research.ait.ie

#### Libraries ####
library(data.table)                         # used for fast table manipulation

#### Functions ####
setwd("~/YOUR/DIRECTORY/Affective_Eye_Gaze")  # functions directory
source("R/eye_gaze_features_calculation.R")  # used to calculate statistics for raw eye gaze data

#### Variables ####
feature_calculation_window_size <- 4

setwd("~/CSV/input_files/location")  
csv_file_list = list.files(pattern="*.csv")
mytable <- fread(csv_file_list[1])
frames_per_second <- floor(1 / mytable[2, timestamp]) # f = 1/t

for(counter in 1:length(csv_file_list))
{
  mytable <- fread(csv_file_list[counter])
  #  Remove data unrelated to eye gaze 
  mytable <- mytable[,c(1,2,4:11,14:245,247:279,281:(length(mytable)-19),(length(mytable)-17):(length(mytable)-1)):=NULL]
  setwd("~/CSV/output_files/location")  #  Set to the directory that you want to provide data output to
  
  # Calculate the features and write the data output in CSV format
  filename = paste(csv_file_list[counter], feature_calculation_window_size, "s_results.csv", sep = "")
  file = calculate_features(mytable, frames_per_second, feature_calculation_window_size)
  write.csv(file, filename)
  
  setwd("~/CSV/input_files/location")  #  Set directory to the input files folder again
}
