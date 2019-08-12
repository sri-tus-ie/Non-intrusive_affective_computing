#### This file takes a all of the .csv files in a directory, filters the files, calculates features
#### on 8 second segments of the file, and outputs the results to CSV file format.
#
# Copyright: Athlone Institute of Technology, 2019
# Author: j.odwyer@research.ait.ie

#### Libraries ####
library(data.table)                         # used for fast table manipulation

#### Functions ####
setwd("~/YOUR_PATH_HERE/Affective_Eye-based_cues")  # functions directory
source("R/features_calculation.R")  # used to calculate features for raw eye gaze data

#### Variables ####
feature_calculation_window_size <- 8

setwd("~/PATH/TO/CSV_INPUT_FILES")
csv_file_list = list.files(pattern="*.csv")
temp <- fread(csv_file_list[1])
frames_per_second <- floor(1 / temp[2, timestamp]) # f = 1/t

for(counter in 1:length(csv_file_list))
{
  temp <- fread(csv_file_list[counter])
  initial_LLDs <- data.table("timestamp" = temp$timestamp,
                             "gaze_angle_x" = temp$gaze_angle_x, "gaze_angle_y" = temp$gaze_angle_y,
                             "eyes_closed" = temp$AU45_c, "eye_blink_intensity" = temp$AU45_r,
                             "direct_gaze" = temp$direct_gaze)
  
  LLDs_for_calculation <- data.table(
                        "gaze_angle_x" = temp$gaze_angle_x, "gaze_angle_y" = temp$gaze_angle_y,
                        "left_pupil_x2" = temp$eye_lmk_X_55, "left_pupil_x1" = temp$eye_lmk_X_51,
                        "left_pupil_y2" = temp$eye_lmk_Y_53, "left_pupil_y1" = temp$eye_lmk_Y_49,
                        "eye_lmk_Z_8" = temp$eye_lmk_Z_8, "eye_lmk_Z_8" = temp$eye_lmk_Z_42
                        )
  
  setwd("~/PATH/TO/CSV_OUTPUT_FILES_LOC")  #  Data output directory
  
  # Calculate the features and write the data output in CSV format
  filename = paste(csv_file_list[counter], feature_calculation_window_size, "s_results.csv", sep = "")
  file = calculate_features(LLDs_for_calculation, initial_LLDs, frames_per_second, feature_calculation_window_size)
  write.csv(file, filename)
  
  setwd("~/PATH/TO/CSV_INPUT_FILES")  #  Set directory to the input files folder again
}
