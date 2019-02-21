#### This function takes a specific OpenFace data.table input, and returns output of the
#### eye gaze and eye blink/closure features.
#
# Copyright: Athlone Institute of Technology, 2019
# Author: j.odwyer@research.ait.ie

calculate_features <- function(mytable, fps, feature_calculation_temporal_window_size){

  #### Libraries ####
  library(e1071)                     #  used for kurtosis and skewness calculations
  #### Functions ####
  source("~/YOUR/DIRECTORY/Affective_Eye_Gaze/R/eye_gaze_LLD_calculation.R") 
  
  frames_per_second <- fps
  frame_time_in_seconds <- 1 / frames_per_second
  feature_calculation_sliding_window_size <- frames_per_second * feature_calculation_temporal_window_size
  
  #### Output table model that will be used to store calculated data for later output to file
  output_table <- data.table(
    "sample_number" = 1:floor(length(mytable[,gaze_angle_x]) - (feature_calculation_sliding_window_size - 1)),
    
    "gaze_x_min" = 0.0, "gaze_x_quartile_1" = 0.0, "gaze_x_median" = 0.0, "gaze_x_mean" = 0.0,
    "gaze_x_quartile_3" = 0.0, "gaze_x_max" = 0.0, "gaze_x_SD" = 0.0, "gaze_x_skewness" = 0.0,
    "gaze_x_kurtosis" = 0.0, "gaze_x_IQR_1_2" = 0.0, "gaze_x_IQR_2_3" = 0.0, "gaze_x_IQR_1_3" = 0.0,
    "gaze_x_LR_intercept" = 0.0, "gaze_x_LR_slope" = 0.0,
    
    "gaze_y_min" = 0.0, "gaze_y_quartile_1" = 0.0, "gaze_y_median" = 0.0, "gaze_y_mean" = 0.0,
    "gaze_y_quartile_3" = 0.0, "gaze_y_max" = 0.0, "gaze_y_SD" = 0.0, "gaze_y_skewness" = 0.0,
    "gaze_y_kurtosis" = 0.0, "gaze_y_IQR_1_2" = 0.0, "gaze_y_IQR_2_3" = 0.0, "gaze_y_IQR_1_3" = 0.0,
    "gaze_y_LR_intercept" = 0.0, "gaze_y_LR_slope" = 0.0,
    
    "delta_gaze_x_min" = 0.0, "delta_gaze_x_quartile_1" = 0.0, "delta_gaze_x_median" = 0.0, "delta_gaze_x_mean" = 0.0,
    "delta_gaze_x_quartile_3" = 0.0, "delta_gaze_x_max" = 0.0, "delta_gaze_x_SD" = 0.0, "delta_gaze_x_skewness" = 0.0,
    "delta_gaze_x_kurtosis" = 0.0, "delta_gaze_x_IQR_1_2" = 0.0, "delta_gaze_x_IQR_2_3" = 0.0,
    "delta_gaze_x_IQR_1_3" = 0.0, "delta_gaze_x_LR_intercept" = 0.0, "delta_gaze_x_LR_slope" = 0.0,
    
    "delta_gaze_y_min" = 0.0, "delta_gaze_y_quartile_1" = 0.0, "delta_gaze_y_median" = 0.0, "delta_gaze_y_mean" = 0.0,
    "delta_gaze_y_quartile_3" = 0.0, "delta_gaze_y_max" = 0.0, "delta_gaze_y_SD" = 0.0, "delta_gaze_y_skewness" = 0.0,
    "delta_gaze_y_kurtosis" = 0.0, "delta_gaze_y_IQR_1_2" = 0.0, "delta_gaze_y_IQR_2_3" = 0.0,
    "delta_gaze_y_IQR_1_3" = 0.0, "delta_gaze_y_LR_intercept" = 0.0, "delta_gaze_y_LR_slope" = 0.0,
    
    "eye_blink_intensity_median" = 0.0, "eye_blink_intensity_mean" = 0.0,
    "eye_blink_intensity_quartile_3" = 0.0, "eye_blink_intensity_max" = 0.0, "eye_blink_intensity_SD" = 0.0, 
    "eye_blink_intensity_IQR_1_2" = 0.0, "eye_blink_intensity_IQR_2_3" = 0.0,
     "eye_blink_intensity_LR_intercept" = 0.0, "eye_blink_intensity_LR_slope" = 0.0,
    
    "eyes_closed_time_ratio" = 0.0, "min_eyes_closed_time_secs" = 0.0, "median_eyes_closed_time_secs" = 0.0,
    "mean_eyes_closed_time_secs" = 0.0, "max_eyes_closed_time_secs" = 0.0,
    
    "gaze_fixation_time_ratio" = 0.0, "min_gaze_fixation_time_secs" = 0.0, "median_gaze_fixation_time_secs" = 0.0,
    "mean_gaze_fixation_time_secs" = 0.0, "max_gaze_fixation_time_secs" = 0.0,
    
    "gaze_approach_time_ratio" = 0.0, "median_gaze_approach_time_secs" = 0.0,
    "mean_gaze_approach_time_secs" = 0.0, "max_gaze_approach_time_secs" = 0.0,

    "unused_target_label" = 0.0
  )
  
  LLD_table <- calculate_LLDs(mytable)  #  Gather low-level descriptors
  avg_counter <- 1  #  Set average counter for sliding window movement
  # Some variables used to index into the summary statistics tables
  MIN <- 1
  FirstQu <- 2
  Median <- 3
  Mean <- 4
  ThirdQu <- 5
  MAX <- 6
  eye_blink_frames <- {}
  eye_fixation_frames <- {}
  eye_gaze_approach_frames <- {}
  frame_count <- 0
  
  for(j in 1:dim(output_table)[1]) {
    gaze_x_summary_stats <- apply(
      mytable[,"gaze_angle_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,summary
    )
    output_table[j,"gaze_x_min"] <- gaze_x_summary_stats[MIN]
    output_table[j,"gaze_x_quartile_1"] <- gaze_x_summary_stats[FirstQu]
    output_table[j,"gaze_x_median"] <- gaze_x_summary_stats[Median]
    output_table[j,"gaze_x_mean"] <- gaze_x_summary_stats[Mean]
    output_table[j,"gaze_x_quartile_3"] <- gaze_x_summary_stats[ThirdQu]
    output_table[j,"gaze_x_max"] <- gaze_x_summary_stats[MAX]
    output_table[j,"gaze_x_SD"] <- sd(
      rep(t(mytable[,"gaze_angle_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"gaze_x_skewness"] <- skewness(
      rep(t(mytable[,"gaze_angle_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"gaze_x_kurtosis"] <- kurtosis(
      rep(t(mytable[,"gaze_angle_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"gaze_x_IQR_1_2"] <- gaze_x_summary_stats[Median] - gaze_x_summary_stats[FirstQu]
    output_table[j,"gaze_x_IQR_2_3"] <- gaze_x_summary_stats[ThirdQu] - gaze_x_summary_stats[Median]
    inter_quartile_range <- gaze_x_summary_stats[ThirdQu] - gaze_x_summary_stats[FirstQu]
    output_table[j,"gaze_x_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(mytable[,"gaze_angle_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
      ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"gaze_x_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"gaze_x_LR_slope"] <- fit$coefficients[2]
    
    
    gaze_y_summary_stats <- apply(
      mytable[,"gaze_angle_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,summary
    )
    output_table[j,"gaze_y_min"] <- gaze_y_summary_stats[MIN]
    output_table[j,"gaze_y_quartile_1"] <- gaze_y_summary_stats[FirstQu]
    output_table[j,"gaze_y_median"] <- gaze_y_summary_stats[Median]
    output_table[j,"gaze_y_mean"] <- gaze_y_summary_stats[Mean]
    output_table[j,"gaze_y_quartile_3"] <- gaze_y_summary_stats[ThirdQu]
    output_table[j,"gaze_y_max"] <- gaze_y_summary_stats[MAX]
    output_table[j,"gaze_y_SD"] <- sd(
      rep(t(mytable[,"gaze_angle_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"gaze_y_skewness"] <- skewness(
      rep(t(mytable[,gaze_angle_y][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"gaze_y_kurtosis"] <- kurtosis(
      rep(t(mytable[,"gaze_angle_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"gaze_y_IQR_1_2"] <- gaze_y_summary_stats[Median] - gaze_y_summary_stats[FirstQu]
    output_table[j,"gaze_y_IQR_2_3"] <- gaze_y_summary_stats[ThirdQu] - gaze_y_summary_stats[Median]
    inter_quartile_range <- gaze_y_summary_stats[ThirdQu] - gaze_y_summary_stats[FirstQu]
    output_table[j,"gaze_y_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(mytable[,"gaze_angle_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
      ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"gaze_y_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"gaze_y_LR_slope"] <- fit$coefficients[2]
    

    delta_gaze_x_summary_stats <- apply(
      LLD_table[,"delta_gaze_angle_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,summary
    )
    output_table[j,"delta_gaze_x_min"] <- delta_gaze_x_summary_stats[MIN]
    output_table[j,"delta_gaze_x_quartile_1"] <- delta_gaze_x_summary_stats[FirstQu]
    output_table[j,"delta_gaze_x_median"] <- delta_gaze_x_summary_stats[Median]
    output_table[j,"delta_gaze_x_mean"] <- delta_gaze_x_summary_stats[Mean]
    output_table[j,"delta_gaze_x_quartile_3"] <- delta_gaze_x_summary_stats[ThirdQu]
    output_table[j,"delta_gaze_x_max"] <- delta_gaze_x_summary_stats[MAX]
    output_table[j,"delta_gaze_x_SD"] <- sd(
      rep(t(LLD_table[,"delta_gaze_angle_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"delta_gaze_x_skewness"] <- skewness(
      rep(t(LLD_table[,"delta_gaze_angle_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"delta_gaze_x_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"delta_gaze_angle_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"delta_gaze_x_IQR_1_2"] <- delta_gaze_x_summary_stats[Median] - delta_gaze_x_summary_stats[FirstQu]
    output_table[j,"delta_gaze_x_IQR_2_3"] <- delta_gaze_x_summary_stats[ThirdQu] - delta_gaze_x_summary_stats[Median]
    inter_quartile_range <- delta_gaze_x_summary_stats[ThirdQu] - delta_gaze_x_summary_stats[FirstQu]
    output_table[j,"delta_gaze_x_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"delta_gaze_angle_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
      ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"delta_gaze_x_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"delta_gaze_x_LR_slope"] <- fit$coefficients[2]
    
    
    delta_gaze_y_summary_stats <- apply(
      LLD_table[,"delta_gaze_angle_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,summary
    )
    output_table[j,"delta_gaze_y_min"] <- delta_gaze_y_summary_stats[MIN]
    output_table[j,"delta_gaze_y_quartile_1"] <- delta_gaze_y_summary_stats[FirstQu]
    output_table[j,"delta_gaze_y_median"] <- delta_gaze_y_summary_stats[Median]
    output_table[j,"delta_gaze_y_mean"] <- delta_gaze_y_summary_stats[Mean]
    output_table[j,"delta_gaze_y_quartile_3"] <- delta_gaze_y_summary_stats[ThirdQu]
    output_table[j,"delta_gaze_y_max"] <- delta_gaze_y_summary_stats[MAX]
    output_table[j,"delta_gaze_y_SD"] <- sd(
      rep(t(LLD_table[,"delta_gaze_angle_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"delta_gaze_y_skewness"] <- skewness(
      rep(t(LLD_table[,"delta_gaze_angle_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"delta_gaze_y_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"delta_gaze_angle_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"delta_gaze_y_IQR_1_2"] <- delta_gaze_y_summary_stats[Median] - delta_gaze_y_summary_stats[FirstQu]
    output_table[j,"delta_gaze_y_IQR_2_3"] <- delta_gaze_y_summary_stats[ThirdQu] - delta_gaze_y_summary_stats[Median]
    inter_quartile_range <- delta_gaze_y_summary_stats[ThirdQu] - delta_gaze_y_summary_stats[FirstQu]
    output_table[j,"delta_gaze_y_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"delta_gaze_angle_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
      ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"delta_gaze_y_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"delta_gaze_y_LR_slope"] <- fit$coefficients[2]
    
    
    eye_blink_intensity_summary_stats <- apply(
      LLD_table[,"eye_blink_intensity_value"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,summary
    )
    output_table[j,"eye_blink_intensity_median"] <- eye_blink_intensity_summary_stats[Median]
    output_table[j,"eye_blink_intensity_mean"] <- eye_blink_intensity_summary_stats[Mean]
    output_table[j,"eye_blink_intensity_quartile_3"] <- eye_blink_intensity_summary_stats[ThirdQu]
    output_table[j,"eye_blink_intensity_max"] <- eye_blink_intensity_summary_stats[MAX]
    output_table[j,"eye_blink_intensity_SD"] <- sd(
      rep(t(LLD_table[,"eye_blink_intensity_value"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"delta_gaze_y_skewness"] <- skewness(
      rep(t(LLD_table[,"eye_blink_intensity_value"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"delta_gaze_y_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"eye_blink_intensity_value"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"eye_blink_intensity_IQR_1_2"] <- eye_blink_intensity_summary_stats[Median] - eye_blink_intensity_summary_stats[FirstQu]
    output_table[j,"eye_blink_intensity_IQR_2_3"] <- eye_blink_intensity_summary_stats[ThirdQu] - eye_blink_intensity_summary_stats[Median]
    fit <- lm(
      rep(t(LLD_table[,"eye_blink_intensity_value"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
      ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"eye_blink_intensity_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"eye_blink_intensity_LR_slope"] <- fit$coefficients[2]
    
    
    output_table[j,"eyes_closed_time_ratio"] <- 
      sum(LLD_table[,"eye_blink"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))] == 1) /
      (feature_calculation_sliding_window_size)
    for(i in avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))){
      if(LLD_table[i,"eye_blink"] == 1){
        frame_count <- frame_count + 1
      } else if(LLD_table[i,"eye_blink"] == 0 && frame_count == 0){
        frame_count <- 0
      } else {
        eye_blink_frames <- c(eye_blink_frames, frame_count)
        frame_count <- 0
      }
    }
    if(is.null(eye_blink_frames)){
      output_table[j,"min_eyes_closed_time_secs"] <- 0.0
      output_table[j,"median_eyes_closed_time_secs"] <- 0.0
      output_table[j,"mean_eyes_closed_time_secs"] <- 0.0
      output_table[j,"max_eyes_closed_time_secs"] <- 0.0
    } else {
      output_table[j,"min_eyes_closed_time_secs"] <- min(eye_blink_frames) * frame_time_in_seconds
      output_table[j,"median_eyes_closed_time_secs"] <- median(eye_blink_frames) * frame_time_in_seconds
      output_table[j,"mean_eyes_closed_time_secs"] <- mean(eye_blink_frames) * frame_time_in_seconds
      output_table[j,"max_eyes_closed_time_secs"] <- max(eye_blink_frames) * frame_time_in_seconds
    }
    frame_count <- 0
    eye_blink_frames <- {}
    
    
    output_table[j,"gaze_fixation_time_ratio"] <- 
      sum(LLD_table[,"eyes_fixated"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))] == 1) /
      (feature_calculation_sliding_window_size)
    for(i in avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))){
      if(LLD_table[i,"eyes_fixated"] == 1){
        frame_count <- frame_count + 1
      } else if(LLD_table[i,"eyes_fixated"] == 0 && frame_count == 0){
        frame_count <- 0
      } else {
        eye_fixation_frames <- c(eye_fixation_frames, frame_count)
        frame_count <- 0
      }
    }
    if(is.null(eye_fixation_frames)){
      output_table[j,"min_gaze_fixation_time_secs"] <- 0.0
      output_table[j,"median_gaze_fixation_time_secs"] <- 0.0
      output_table[j,"mean_gaze_fixation_time_secs"] <- 0.0
      output_table[j,"max_gaze_fixation_time_secs"] <- 0.0
    } else {
      output_table[j,"min_gaze_fixation_time_secs"] <- min(eye_fixation_frames) * frame_time_in_seconds
      output_table[j,"median_gaze_fixation_time_secs"] <- median(eye_fixation_frames) * frame_time_in_seconds
      output_table[j,"mean_gaze_fixation_time_secs"] <- mean(eye_fixation_frames) * frame_time_in_seconds
      output_table[j,"max_gaze_fixation_time_secs"] <- max(eye_fixation_frames) * frame_time_in_seconds
    }
    frame_count <- 0
    eye_fixation_frames <- {}
    
    output_table[j,"gaze_approach_time_ratio"] <-
      sum(LLD_table[,"eye_gaze_approach"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))] == 1) /
      (feature_calculation_sliding_window_size)
    for(i in avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))){
      if(LLD_table[i,"eye_gaze_approach"] == 1){
        frame_count <- frame_count + 1
      } else if(LLD_table[i,"eye_gaze_approach"] == 0 && frame_count == 0){
        frame_count <- 0
      } else{
        eye_gaze_approach_frames <- c(eye_gaze_approach_frames, frame_count)
        frame_count <- 0
      }
    }
    if(is.null(eye_gaze_approach_frames)){
      output_table[j,"median_gaze_approach_time_secs"] <- 0.0
      output_table[j,"mean_gaze_approach_time_secs"] <- 0.0
      output_table[j,"max_gaze_approach_time_secs"] <- 0.0
    } else {
      output_table[j,"median_gaze_approach_time_secs"] <- median(eye_gaze_approach_frames) * frame_time_in_seconds
      output_table[j,"mean_gaze_approach_time_secs"] <- mean(eye_gaze_approach_frames) * frame_time_in_seconds
      output_table[j,"max_gaze_approach_time_secs"] <- max(eye_gaze_approach_frames) * frame_time_in_seconds
    }
    frame_count <- 0
    eye_gaze_approach_frames <- {}
    
    avg_counter <- avg_counter + 1
  }
  
  return(output_table)
}
