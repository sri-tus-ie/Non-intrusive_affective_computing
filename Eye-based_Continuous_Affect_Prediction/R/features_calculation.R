#### This function calculates the base feature set specified in the paper for a given data.table
#### and returns the result.
#
# Copyright: Athlone Institute of Technology, 2019
# Author: j.odwyer@research.ait.ie

calculate_features <- function(LLDs_for_calculation, initial_LLDs, fps, feature_calculation_window_size_seconds) {

  #### Libraries ####
  library(e1071)      #  used for kurtosis and skewness calculations
  library(wavelets)   #  wavelet decomposition
  library(seewave)    #  zero crossing rate, root mean square
  #### Functions ####
    source("~/YOUR_PATH_HERE/Affective_Eye-based_cues/R/LLD_calculation.R")
  
  frames_per_second <- fps
  frame_time_in_seconds <- 1 / frames_per_second
  feature_calculation_sliding_window <- frames_per_second * feature_calculation_window_size_seconds
  
  #### Output table model that will be used to store calculated data for later output to file
  output_table <- data.table(
    "sample_number" = 1:floor((dim(initial_LLDs)[1]) - (feature_calculation_sliding_window - 1)),
    
    "gaze_angle_x_min" = 0.0, "gaze_angle_x_quartile_1" = 0.0, "gaze_angle_x_median" = 0.0, "gaze_angle_x_mean" = 0.0,
    "gaze_angle_x_quartile_3" = 0.0, "gaze_angle_x_max" = 0.0, "gaze_angle_x_SD" = 0.0, "gaze_angle_x_skewness" = 0.0,
    "gaze_angle_x_kurtosis" = 0.0, "gaze_angle_x_IQR_1_2" = 0.0, "gaze_angle_x_IQR_2_3" = 0.0,
    "gaze_angle_x_IQR_1_3" = 0.0, "gaze_angle_x_LR_intercept" = 0.0, "gaze_angle_x_LR_slope" = 0.0,
    
    "gaze_angle_y_min" = 0.0, "gaze_angle_y_quartile_1" = 0.0, "gaze_angle_y_median" = 0.0, "gaze_angle_y_mean" = 0.0,
    "gaze_angle_y_quartile_3" = 0.0, "gaze_angle_y_max" = 0.0, "gaze_angle_y_SD" = 0.0, "gaze_angle_y_skewness" = 0.0,
    "gaze_angle_y_kurtosis" = 0.0, "gaze_angle_y_IQR_1_2" = 0.0, "gaze_angle_y_IQR_2_3" = 0.0,
    "gaze_angle_y_IQR_1_3" = 0.0, "gaze_angle_y_LR_intercept" = 0.0, "gaze_angle_y_LR_slope" = 0.0,
    
    "delta_gaze_angle_x_min" = 0.0, "delta_gaze_angle_x_quartile_1" = 0.0, "delta_gaze_angle_x_median" = 0.0,
    "delta_gaze_angle_x_mean" = 0.0, "delta_gaze_angle_x_quartile_3" = 0.0, "delta_gaze_angle_x_max" = 0.0,
    "delta_gaze_angle_x_SD" = 0.0, "delta_gaze_angle_x_skewness" = 0.0, "delta_gaze_angle_x_kurtosis" = 0.0,
    "delta_gaze_angle_x_IQR_1_2" = 0.0, "delta_gaze_angle_x_IQR_2_3" = 0.0, "delta_gaze_angle_x_IQR_1_3" = 0.0,
    "delta_gaze_angle_x_LR_intercept" = 0.0, "delta_gaze_angle_x_LR_slope" = 0.0,
    
    "delta_gaze_angle_y_min" = 0.0, "delta_gaze_angle_y_quartile_1" = 0.0, "delta_gaze_angle_y_median" = 0.0,
    "delta_gaze_angle_y_mean" = 0.0, "delta_gaze_angle_y_quartile_3" = 0.0, "delta_gaze_angle_y_max" = 0.0,
    "delta_gaze_angle_y_SD" = 0.0, "delta_gaze_angle_y_skewness" = 0.0, "delta_gaze_angle_y_kurtosis" = 0.0,
    "delta_gaze_angle_y_IQR_1_2" = 0.0, "delta_gaze_angle_y_IQR_2_3" = 0.0, "delta_gaze_angle_y_IQR_1_3" = 0.0,
    "delta_gaze_angle_y_LR_intercept" = 0.0, "delta_gaze_angle_y_LR_slope" = 0.0,
    
    "eye_blink_intensity_median" = 0.0, "eye_blink_intensity_mean" = 0.0,
    "eye_blink_intensity_quartile_3" = 0.0, "eye_blink_intensity_max" = 0.0, "eye_blink_intensity_SD" = 0.0, 
    "eye_blink_intensity_IQR_1_2" = 0.0, "eye_blink_intensity_IQR_2_3" = 0.0, "eye_blink_intensity_IQR_1_3" = 0.0,
    "eye_blink_intensity_LR_intercept" = 0.0, "eye_blink_intensity_LR_slope" = 0.0,
    
    "pupil_diameter_mm_min" = 0.0, "pupil_diameter_mm_quartile_1" = 0.0,
    "pupil_diameter_mm_median" = 0.0, "pupil_diameter_mm_mean" = 0.0,
    "pupil_diameter_mm_quartile_3" = 0.0, "pupil_diameter_mm_max" = 0.0,
    "pupil_diameter_mm_SD" = 0.0, "pupil_diameter_mm_skewness" = 0.0,
    "pupil_diameter_mm_kurtosis" = 0.0, "pupil_diameter_mm_IQR_1_2" = 0.0,
    "pupil_diameter_mm_IQR_2_3" = 0.0, "pupil_diameter_mm_IQR_1_3" = 0.0,
    "pupil_diameter_mm_LR_intercept" = 0.0, "pupil_diameter_mm_LR_slope" = 0.0,

    "delta_pupil_diameter_mm_min" = 0.0, "delta_pupil_diameter_mm_quartile_1" = 0.0,
    "delta_pupil_diameter_mm_mean" = 0.0,
    "delta_pupil_diameter_mm_quartile_3" = 0.0, "delta_pupil_diameter_mm_max" = 0.0,
    "delta_pupil_diameter_mm_SD" = 0.0, "delta_pupil_diameter_mm_skewness" = 0.0,
    "delta_pupil_diameter_mm_kurtosis" = 0.0, "delta_pupil_diameter_mm_IQR_1_2" = 0.0,
    "delta_pupil_diameter_mm_IQR_2_3" = 0.0, "delta_pupil_diameter_mm_IQR_1_3" = 0.0,
    "delta_pupil_diameter_mm_LR_intercept" = 0.0, "delta_pupil_diameter_mm_LR_slope" = 0.0,

    "db_wavelet_coefficients_l1_min" = 0.0, "db_wavelet_coefficients_l1_quartile_1" = 0.0,
    "db_wavelet_coefficients_l1_median" = 0.0,
    "db_wavelet_coefficients_l1_quartile_3" = 0.0, "db_wavelet_coefficients_l1_max" = 0.0,
    "db_wavelet_coefficients_l1_SD" = 0.0, "db_wavelet_coefficients_l1_skewness" = 0.0,
    "db_wavelet_coefficients_l1_kurtosis" = 0.0, "db_wavelet_coefficients_l1_IQR_1_2" = 0.0,
    "db_wavelet_coefficients_l1_IQR_2_3" = 0.0, "db_wavelet_coefficients_l1_IQR_1_3" = 0.0,
    "db_wavelet_coefficients_l1_RMS" = 0.0, "db_wavelet_coefficients_l1_ZCR" = 0.0,

    "db_scale_coefficients_l1_min" = 0.0, "db_scale_coefficients_l1_quartile_1" = 0.0,
    "db_scale_coefficients_l1_median" = 0.0, "db_scale_coefficients_l1_quartile_3" = 0.0,
    "db_scale_coefficients_l1_max" = 0.0, "db_scale_coefficients_l1_SD" = 0.0,
    "db_scale_coefficients_l1_skewness" = 0.0, "db_scale_coefficients_l1_kurtosis" = 0.0,
    "db_scale_coefficients_l1_IQR_1_2" = 0.0, "db_scale_coefficients_l1_IQR_2_3" = 0.0,
    "db_scale_coefficients_l1_IQR_1_3" = 0.0, "db_scale_coefficients_l1_RMS" = 0.0, 

    "db_wavelet_coefficients_l2_min" = 0.0, "db_wavelet_coefficients_l2_quartile_1" = 0.0,
    "db_wavelet_coefficients_l2_median" = 0.0, "db_wavelet_coefficients_l2_quartile_3" = 0.0,
    "db_wavelet_coefficients_l2_max" = 0.0, "db_wavelet_coefficients_l2_SD" = 0.0,
    "db_wavelet_coefficients_l2_skewness" = 0.0, "db_wavelet_coefficients_l2_kurtosis" = 0.0,
    "db_wavelet_coefficients_l2_IQR_1_2" = 0.0, "db_wavelet_coefficients_l2_IQR_2_3" = 0.0,
    "db_wavelet_coefficients_l2_IQR_1_3" = 0.0, "db_wavelet_coefficients_l2_RMS" = 0.0,
    "db_wavelet_coefficients_l2_ZCR" = 0.0,

    "db_scale_coefficients_l2_min" = 0.0, "db_scale_coefficients_l2_quartile_1" = 0.0,
    "db_scale_coefficients_l2_median" = 0.0, "db_scale_coefficients_l2_quartile_3" = 0.0,
    "db_scale_coefficients_l2_max" = 0.0, "db_scale_coefficients_l2_SD" = 0.0,
    "db_scale_coefficients_l2_skewness" = 0.0, "db_scale_coefficients_l2_kurtosis" = 0.0,
    "db_scale_coefficients_l2_IQR_1_2" = 0.0, "db_scale_coefficients_l2_IQR_2_3" = 0.0,
    "db_scale_coefficients_l2_IQR_1_3" = 0.0, "db_scale_coefficients_l2_RMS" = 0.0,

    "db_wavelet_coefficients_l3_min" = 0.0, "db_wavelet_coefficients_l3_quartile_1" = 0.0,
    "db_wavelet_coefficients_l3_median" = 0.0, "db_wavelet_coefficients_l3_quartile_3" = 0.0,
    "db_wavelet_coefficients_l3_max" = 0.0, "db_wavelet_coefficients_l3_SD" = 0.0,
    "db_wavelet_coefficients_l3_skewness" = 0.0, "db_wavelet_coefficients_l3_kurtosis" = 0.0,
    "db_wavelet_coefficients_l3_IQR_1_2" = 0.0, "db_wavelet_coefficients_l3_IQR_2_3" = 0.0,
    "db_wavelet_coefficients_l3_IQR_1_3" = 0.0, "db_wavelet_coefficients_l3_RMS" = 0.0,
    "db_wavelet_coefficients_l3_ZCR" = 0.0,

    "db_scale_coefficients_l3_min" = 0.0, "db_scale_coefficients_l3_quartile_1" = 0.0,
    "db_scale_coefficients_l3_median" = 0.0, "db_scale_coefficients_l3_quartile_3" = 0.0,
    "db_scale_coefficients_l3_max" = 0.0, "db_scale_coefficients_l3_SD" = 0.0,
    "db_scale_coefficients_l3_skewness" = 0.0, "db_scale_coefficients_l3_kurtosis" = 0.0,
    "db_scale_coefficients_l3_IQR_1_2" = 0.0, "db_scale_coefficients_l3_IQR_2_3" = 0.0,
    "db_scale_coefficients_l3_IQR_1_3" = 0.0, "db_scale_coefficients_l3_RMS" = 0.0,

    "db_wavelet_coefficients_l4_min" = 0.0, "db_wavelet_coefficients_l4_quartile_1" = 0.0,
    "db_wavelet_coefficients_l4_median" = 0.0, "db_wavelet_coefficients_l4_quartile_3" = 0.0,
    "db_wavelet_coefficients_l4_max" = 0.0, "db_wavelet_coefficients_l4_SD" = 0.0,
    "db_wavelet_coefficients_l4_skewness" = 0.0, "db_wavelet_coefficients_l4_kurtosis" = 0.0,
    "db_wavelet_coefficients_l4_IQR_1_2" = 0.0, "db_wavelet_coefficients_l4_IQR_2_3" = 0.0,
    "db_wavelet_coefficients_l4_IQR_1_3" = 0.0, "db_wavelet_coefficients_l4_RMS" = 0.0,
    "db_wavelet_coefficients_l4_ZCR" = 0.0,

    "db_scale_coefficients_l4_min" = 0.0, "db_scale_coefficients_l4_quartile_1" = 0.0,
    "db_scale_coefficients_l4_median" = 0.0, "db_scale_coefficients_l4_quartile_3" = 0.0,
    "db_scale_coefficients_l4_max" = 0.0, "db_scale_coefficients_l4_SD" = 0.0,
    "db_scale_coefficients_l4_skewness" = 0.0, "db_scale_coefficients_l4_kurtosis" = 0.0,
    "db_scale_coefficients_l4_IQR_1_2" = 0.0, "db_scale_coefficients_l4_IQR_2_3" = 0.0,
    "db_scale_coefficients_l4_IQR_1_3" = 0.0, "db_scale_coefficients_l4_RMS" = 0.0, 

    "db_wavelet_coefficients_l5_min" = 0.0, "db_wavelet_coefficients_l5_quartile_1" = 0.0,
    "db_wavelet_coefficients_l5_median" = 0.0, "db_wavelet_coefficients_l5_quartile_3" = 0.0,
    "db_wavelet_coefficients_l5_max" = 0.0, "db_wavelet_coefficients_l5_SD" = 0.0,
    "db_wavelet_coefficients_l5_skewness" = 0.0, "db_wavelet_coefficients_l5_kurtosis" = 0.0,
    "db_wavelet_coefficients_l5_IQR_1_2" = 0.0, "db_wavelet_coefficients_l5_IQR_2_3" = 0.0,
    "db_wavelet_coefficients_l5_IQR_1_3" = 0.0, "db_wavelet_coefficients_l5_RMS" = 0.0,
    "db_wavelet_coefficients_l5_ZCR" = 0.0,

    "db_scale_coefficients_l5_min" = 0.0, "db_scale_coefficients_l5_quartile_1" = 0.0,
    "db_scale_coefficients_l5_median" = 0.0, "db_scale_coefficients_l5_quartile_3" = 0.0,
    "db_scale_coefficients_l5_max" = 0.0, "db_scale_coefficients_l5_SD" = 0.0,
    "db_scale_coefficients_l5_skewness" = 0.0, "db_scale_coefficients_l5_kurtosis" = 0.0,
    "db_scale_coefficients_l5_IQR_1_2" = 0.0, "db_scale_coefficients_l5_IQR_2_3" = 0.0,
    "db_scale_coefficients_l5_IQR_1_3" = 0.0, "db_scale_coefficients_l5_RMS" = 0.0, 

    "db_wavelet_coefficients_l6_min" = 0.0, "db_wavelet_coefficients_l6_quartile_1" = 0.0,
    "db_wavelet_coefficients_l6_median" = 0.0, "db_wavelet_coefficients_l6_quartile_3" = 0.0,
    "db_wavelet_coefficients_l6_max" = 0.0, "db_wavelet_coefficients_l6_SD" = 0.0,
    "db_wavelet_coefficients_l6_skewness" = 0.0, "db_wavelet_coefficients_l6_kurtosis" = 0.0,
    "db_wavelet_coefficients_l6_IQR_1_2" = 0.0, "db_wavelet_coefficients_l6_IQR_2_3" = 0.0,
    "db_wavelet_coefficients_l6_IQR_1_3" = 0.0, "db_wavelet_coefficients_l6_RMS" = 0.0,
    "db_wavelet_coefficients_l6_ZCR" = 0.0,

    "db_scale_coefficients_l6_min" = 0.0, "db_scale_coefficients_l6_quartile_1" = 0.0,
    "db_scale_coefficients_l6_median" = 0.0, "db_scale_coefficients_l6_quartile_3" = 0.0,
    "db_scale_coefficients_l6_max" = 0.0, "db_scale_coefficients_l6_SD" = 0.0,
    "db_scale_coefficients_l6_skewness" = 0.0, "db_scale_coefficients_l6_kurtosis" = 0.0,
    "db_scale_coefficients_l6_IQR_1_2" = 0.0, "db_scale_coefficients_l6_IQR_2_3" = 0.0,
    "db_scale_coefficients_l6_IQR_1_3" = 0.0, "db_scale_coefficients_l6_RMS" = 0.0, 

    "db_wavelet_coefficients_l7_min" = 0.0, "db_wavelet_coefficients_l7_quartile_1" = 0.0,
    "db_wavelet_coefficients_l7_median" = 0.0, "db_wavelet_coefficients_l7_quartile_3" = 0.0,
    "db_wavelet_coefficients_l7_max" = 0.0, "db_wavelet_coefficients_l7_SD" = 0.0,
    "db_wavelet_coefficients_l7_skewness" = 0.0, "db_wavelet_coefficients_l7_IQR_1_2" = 0.0,
    "db_wavelet_coefficients_l7_IQR_2_3" = 0.0, "db_wavelet_coefficients_l7_IQR_1_3" = 0.0,
    "db_wavelet_coefficients_l7_RMS" = 0.0, "db_wavelet_coefficients_l7_ZCR" = 0.0,

    "db_scale_coefficients_l7_min" = 0.0, "db_scale_coefficients_l7_quartile_1" = 0.0,
    "db_scale_coefficients_l7_median" = 0.0, "db_scale_coefficients_l7_quartile_3" = 0.0,
    "db_scale_coefficients_l7_max" = 0.0, "db_scale_coefficients_l7_SD" = 0.0,
    "db_scale_coefficients_l7_skewness" = 0.0, "db_scale_coefficients_l7_IQR_1_2" = 0.0,
    "db_scale_coefficients_l7_IQR_2_3" = 0.0, "db_scale_coefficients_l7_IQR_1_3" = 0.0,
    "db_scale_coefficients_l7_RMS" = 0.0,
    
    "pupil_dilation_time_ratio" = 0.0, "pupil_dilation_time_secs_mean" = 0.0,
    "pupil_dilation_time_secs_max" = 0.0, "pupil_dilation_time_secs_total" = 0.0,
    
    "pupil_constriction_time_ratio" = 0.0, "pupil_constriction_time_secs_mean" = 0.0,
    "pupil_constriction_time_secs_max" = 0.0, "pupil_constriction_time_secs_total" = 0.0,
    
    "direct_gaze_time_ratio" = 0.0, "direct_gaze_time_secs_mean" = 0.0,
    "direct_gaze_time_secs_max" = 0.0, "direct_gaze_time_secs_total" = 0.0,
    
    "eyes_closed_time_ratio" = 0.0, "eyes_closed_time_secs_min" = 0.0, "eyes_closed_time_secs_median" = 0.0,
    "eyes_closed_time_secs_mean" = 0.0, "eyes_closed_time_secs_max" = 0.0,
    
    "gaze_fixation_time_ratio" = 0.0, "gaze_fixation_time_secs_min" = 0.0, "gaze_fixation_time_secs_median" = 0.0,
    "gaze_fixation_time_secs_mean" = 0.0, "gaze_fixation_time_secs_max" = 0.0,
    
    "gaze_approach_time_ratio" = 0.0, "gaze_approach_time_secs_median" = 0.0,
    "gaze_approach_time_secs_mean" = 0.0, "gaze_approach_time_secs_max" = 0.0,

    "unused_target_label" = 0.0
  )
  
  LLD_table <- calculate_LLDs(LLDs_for_calculation, frames_per_second)  #  Gather low-level descriptors
  LLD_table <- cbind(initial_LLDs, LLD_table)
  
  features_base_names <- c("gaze_angle_x", "gaze_angle_y", "delta_gaze_angle_x", "delta_gaze_angle_y",
                           "eye_blink_intensity", "pupil_diameter_mm", "delta_pupil_diameter_mm",
                           "db_wavelet_coefficients_l1", "db_scale_coefficients_l1",
                           "db_wavelet_coefficients_l2", "db_scale_coefficients_l2",
                           "db_wavelet_coefficients_l3", "db_scale_coefficients_l3",
                           "db_wavelet_coefficients_l4", "db_scale_coefficients_l4",
                           "db_wavelet_coefficients_l5", "db_scale_coefficients_l5",
                           "db_wavelet_coefficients_l6", "db_scale_coefficients_l6",
                           "db_wavelet_coefficients_l7", "db_scale_coefficients_l7",
                           "pupil_dilation", "pupil_constriction", "direct_gaze",
                           "eyes_closed", "gaze_fixation", "gaze_approach"
                           )
  
  summary_feature_names <- c("_min", "_quartile_1", "_median", "_mean", "_quartile_3", "_max", "_SD",
                             "_skewness", "_kurtosis", "_IQR_1_2", "_IQR_2_3", "_IQR_1_3"
                            )
  wavelets <- c("NA", "NA", "W1", "V1", "W2", "V2", "W3", "V3", "W4", "V4", "W5", "V5", "W6", "V6", 
                     "W7", "V7")

  additional_statistical_feature_names <- c("_LR_intercept", "_LR_slope")
  additional_wavelet_feature_names <- c("_RMS", "_ZCR")
  binary_variables_feature_names <- c("_time_ratio", "_time_secs_mean", "_time_secs_max", "_time_secs_total",
                                      "_time_secs_min", "_time_secs_median"
                                      )
  avg_counter <- 1  #  Set average counter for sliding window movement
  w_index <- 3  #  where to start indexing wavelet_coefficients from
  # Some variables used to index into the summary statistics results
  MIN <- 1
  FirstQu <- 2
  Median <- 3
  Mean <- 4
  ThirdQu <- 5
  MAX <- 6
  
  for(j in 1:dim(output_table)[1]) {
    
    range <- avg_counter:(avg_counter+(feature_calculation_sliding_window - 1))
    
    wavelet_coefficients <- dwt(
      LLD_table$pupil_diameter_mm[range], filter="d10", n.levels=7, boundary="reflection", fast=TRUE
    )
    
    for (index in 1:length(features_base_names)) {
      if(index >= 1 && index <= 7) {
        summary_stats <- summary(LLD_table[[features_base_names[index]]][range])
        output_table[j, paste(features_base_names[index], summary_feature_names[4], sep = "")] <- summary_stats[Mean]
        output_table[j, paste(features_base_names[index], summary_feature_names[7], sep = "")] <- sd(
          LLD_table[[features_base_names[index]]][range]
        )

        fit <- lm(rep(t(LLD_table[[features_base_names[index]]][range])) ~ LLD_table[,timestamp][range])
        
        output_table[j, paste(features_base_names[index], additional_statistical_feature_names[1], sep = "")] <-
          fit$coefficients["(Intercept)"]
        output_table[j, paste(features_base_names[index], additional_statistical_feature_names[2], sep = "")] <-
          fit$coefficients[2]
        
        if(index != 5) {  #  eye blink intensity is one-sided, no negative
          output_table[j, paste(features_base_names[index], summary_feature_names[8], sep = "")] <- skewness(
            LLD_table[[features_base_names[index]]][range]
          )
          output_table[j, paste(features_base_names[index], summary_feature_names[9], sep = "")] <- kurtosis(
            LLD_table[[features_base_names[index]]][range]
          )
          output_table[j, paste(features_base_names[index], summary_feature_names[1], sep = "")] <- summary_stats[MIN]
          output_table[j, paste(features_base_names[index], summary_feature_names[2], sep = "")] <-
            summary_stats[FirstQu]
        }
        
        if(index != 7) {  #  delta pupil diameter mm median has problematic feature, repeating zero or very tiny number
          output_table[j, paste(features_base_names[index], summary_feature_names[3], sep = "")] <- summary_stats[Median]
        }
        
        output_table[j, paste(features_base_names[index], summary_feature_names[5], sep = "")] <- summary_stats[ThirdQu]
        output_table[j, paste(features_base_names[index], summary_feature_names[6], sep = "")] <- summary_stats[MAX]
        
        output_table[j, paste(features_base_names[index], summary_feature_names[10], sep = "")] <- 
          summary_stats[Median] - summary_stats[FirstQu]
        output_table[j, paste(features_base_names[index], summary_feature_names[11], sep = "")] <-
          summary_stats[ThirdQu] - summary_stats[Median]
        inter_quartile_range <- summary_stats[ThirdQu] - summary_stats[FirstQu]
        output_table[j, paste(features_base_names[index], summary_feature_names[12], sep = "")] <- inter_quartile_range
        
      } else if (index >= 8 && index <= 21) {    #  else, we are dealing with wavelet features ....
          
          if(index %% 2 == 0) {  #  even numbers are wavelet coefficients, odd numbers are scale coefficients
            coeffs <-
              wavelet_coefficients@W[[wavelets[w_index]]][1:length(wavelet_coefficients@W[[wavelets[w_index]]])]
            
            summary_stats <- summary(coeffs)
            output_table[j, paste(features_base_names[index], summary_feature_names[7], sep = "")] <- sd(coeffs)
            output_table[j, paste(features_base_names[index], summary_feature_names[8], sep = "")] <- skewness(coeffs)
            
            if(index != 20) {  #  do not gather problematic (wavelet decomp. level 7) kurtosis measurement
              output_table[j, paste(features_base_names[index], summary_feature_names[9], sep = "")] <- kurtosis(coeffs)
            }
            
            output_table[j, paste(features_base_names[index], additional_wavelet_feature_names[1],sep = "")] <-
                                                                                                        rms(coeffs)
            ZCR <- zcr(coeffs, f = frames_per_second, wl = length(coeffs), ovlp = 0, plot = FALSE)
            output_table[j, paste(features_base_names[index],additional_wavelet_feature_names[2], sep = "")] <- ZCR[1,2]
            
        } else {            #  else, scale coefficients, handle indexing accordingly & DO NOT take ZCR
          coeffs <- 
            wavelet_coefficients@V[[wavelets[w_index]]][1:length(wavelet_coefficients@V[[wavelets[w_index]]])]
          summary_stats <- summary(coeffs)
          output_table[j, paste(features_base_names[index], summary_feature_names[7], sep = "")] <- sd(coeffs)
          output_table[j, paste(features_base_names[index], summary_feature_names[8], sep = "")] <- skewness(coeffs)
          
          if(index != 21) { #  do not gather problematic (wavelet decomp. level 7) kurtosis measurement
            output_table[j, paste(features_base_names[index], summary_feature_names[9], sep = "")] <- kurtosis(coeffs)
          }
          
          output_table[j, paste(features_base_names[index], additional_wavelet_feature_names[1], sep = "")] <-
                                                                                                        rms(coeffs)
        }
        output_table[j, paste(features_base_names[index], summary_feature_names[1], sep = "")] <- summary_stats[MIN]
        output_table[j, paste(features_base_names[index], summary_feature_names[2], sep = "")] <- summary_stats[FirstQu]
        output_table[j, paste(features_base_names[index], summary_feature_names[3], sep = "")] <- summary_stats[Median]
        output_table[j, paste(features_base_names[index], summary_feature_names[5], sep = "")] <- summary_stats[ThirdQu]
        output_table[j, paste(features_base_names[index], summary_feature_names[6], sep = "")] <- summary_stats[MAX]
        
        output_table[j, paste(features_base_names[index], summary_feature_names[10], sep = "")] <- 
          summary_stats[Median] - summary_stats[FirstQu]
        output_table[j, paste(features_base_names[index], summary_feature_names[11], sep = "")] <-
          summary_stats[ThirdQu] - summary_stats[Median]
        inter_quartile_range <- summary_stats[ThirdQu] - summary_stats[FirstQu]
        output_table[j, paste(features_base_names[index], summary_feature_names[12], sep = "")] <- inter_quartile_range
        
        w_index <- w_index + 1
      }
      
      if(index >= 22 && index <= 27) {  
        true_frames <- {}
        frame_count <- 0
        output_table[j, paste(features_base_names[index], binary_variables_feature_names[1], sep = "")] <- 
                    sum(LLD_table[[features_base_names[index]]][range] == 1) / (feature_calculation_sliding_window)
        for(i in range){
          if(LLD_table[[features_base_names[index]]][i] == 1){
            frame_count <- frame_count + 1
          } else if(LLD_table[[features_base_names[index]]][i] == 0 && frame_count == 0){
            frame_count <- 0
          } else {
            true_frames <- c(true_frames, frame_count)
            frame_count <- 0
          }
        }
        if(is.null(true_frames)) {
          output_table[j, paste(features_base_names[index], binary_variables_feature_names[2], sep = "")] <- 0.0
          output_table[j, paste(features_base_names[index], binary_variables_feature_names[3], sep = "")] <- 0.0
          
          if(index == 22 || index == 23 || index == 24) {  #  pupil_dilation, pupil_constriction, direct_gaze features
            output_table[j, paste(features_base_names[index], binary_variables_feature_names[4], sep = "")] <- 0.0
          } else {  #  eyes_closed, gaze_fixation, gaze_approach features
            
            if(index != 27) {  #  no min gaze approach (it is one-sided)
              output_table[j, paste(features_base_names[index], binary_variables_feature_names[5], sep = "")] <- 0.0
            }
            output_table[j, paste(features_base_names[index], binary_variables_feature_names[6], sep = "")] <- 0.0
          }
          
        } else {
          output_table[j, paste(features_base_names[index], binary_variables_feature_names[2], sep = "")] <-
                                                                          mean(true_frames) * frame_time_in_seconds
          output_table[j, paste(features_base_names[index], binary_variables_feature_names[3], sep = "")] <-
                                                                          max(true_frames) * frame_time_in_seconds
          
          if(index == 22 || index == 23 || index == 24) {  #  pupil_dilation, pupil_constriction, direct_gaze features
          output_table[j, paste(features_base_names[index], binary_variables_feature_names[4], sep = "")] <-
                                                                          sum(true_frames) * frame_time_in_seconds
          } else {  #  eyes_closed, gaze_fixation, gaze_approach features
            if(index != 27) {  #  no min gaze approach (it is one-sided)
              output_table[j, paste(features_base_names[index], binary_variables_feature_names[5], sep = "")] <-
                min(true_frames) * frame_time_in_seconds
            }
            output_table[j, paste(features_base_names[index], binary_variables_feature_names[6], sep = "")] <-
              median(true_frames) * frame_time_in_seconds
          }
          
        }
        frame_count <- 0
        true_frames <- {}
      }
    
    }
    
    w_index <- 3  #  reset, for next wavelet coeffs
    avg_counter <- avg_counter + 1
  }
  
  return(output_table)
}
