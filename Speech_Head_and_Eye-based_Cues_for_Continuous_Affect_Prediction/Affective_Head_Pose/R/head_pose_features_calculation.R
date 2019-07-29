#### This function takes a specific OpenFace data.table input, and returns output of the head pose features.
#
# Copyright: Athlone Institute of Technology, 2018
# Author: j.odwyer@research.ait.ie

calculate_features <- function(mytable, fps, feature_calculation_temporal_window_size) {

  #### Libraries ####
  library(e1071)                     #  used for kurtosis and skewness calculations
  
  #### LLD calculation function
  source("~/YOUR/DIRECTORY/Affective_Head_Pose/R/head_pose_LLD_calculation.R") 

  frames_per_second <- fps
  feature_calculation_sliding_window_size <- frames_per_second * feature_calculation_temporal_window_size
  
  #### Output table model that will be used to store calculated feature data for later output to file
  output_table <- data.table(
    "sample_number" = 1:floor(length(mytable[,pose_Tx]) - (feature_calculation_sliding_window_size - 1)),
    
    "head_location_x_min" = 0.0, "head_location_x_quartile_1" = 0.0, "head_location_x_median" = 0.0,
    "head_location_x_mean" = 0.0, "head_location_x_quartile_3" = 0.0, "head_location_x_max" = 0.0,
    "head_location_x_SD" = 0.0, "head_location_x_skewness" = 0.0, "head_location_x_kurtosis" = 0.0,
    "head_location_x_IQR_1_2" = 0.0, "head_location_x_IQR_2_3" = 0.0, "head_location_x_IQR_1_3" = 0.0,
    "head_location_x_LR_intercept" = 0.0, "head_location_x_LR_slope" = 0.0,
    
    "head_location_y_min" = 0.0, "head_location_y_quartile_1" = 0.0, "head_location_y_median" = 0.0,
    "head_location_y_mean" = 0.0, "head_location_y_quartile_3" = 0.0, "head_location_y_max" = 0.0,
    "head_location_y_SD" = 0.0, "head_location_y_skewness" = 0.0, "head_location_y_kurtosis" = 0.0,
    "head_location_y_IQR_1_2" = 0.0, "head_location_y_IQR_2_3" = 0.0, "head_location_y_IQR_1_3" = 0.0,
    "head_location_y_LR_intercept" = 0.0, "head_location_y_LR_slope" = 0.0,
    
    "head_location_z_min" = 0.0, "head_location_z_quartile_1" = 0.0, "head_location_z_median" = 0.0,
    "head_location_z_mean" = 0.0, "head_location_z_quartile_3" = 0.0, "head_location_z_max" = 0.0,
    "head_location_z_SD" = 0.0, "head_location_z_skewness" = 0.0, "head_location_z_kurtosis" = 0.0,
    "head_location_z_IQR_1_2" = 0.0, "head_location_z_IQR_2_3" = 0.0, "head_location_z_IQR_1_3" = 0.0,
    "head_location_z_LR_intercept" = 0.0, "head_location_z_LR_slope" = 0.0,
    
    "delta_head_location_x_min" = 0.0, "delta_head_location_x_quartile_1" = 0.0, "delta_head_location_x_median" = 0.0,
    "delta_head_location_x_mean" = 0.0, "delta_head_location_x_quartile_3" = 0.0, "delta_head_location_x_max" = 0.0,
    "delta_head_location_x_SD" = 0.0, "delta_head_location_x_skewness" = 0.0, "delta_head_location_x_kurtosis" = 0.0,
    "delta_head_location_x_IQR_1_2" = 0.0, "delta_head_location_x_IQR_2_3" = 0.0, "delta_head_location_x_IQR_1_3" = 0.0,
    "delta_head_location_x_LR_intercept" = 0.0, "delta_head_location_x_LR_slope" = 0.0,
    
    "delta_head_location_y_min" = 0.0, "delta_head_location_y_quartile_1" = 0.0, "delta_head_location_y_median" = 0.0,
    "delta_head_location_y_mean" = 0.0, "delta_head_location_y_quartile_3" = 0.0, "delta_head_location_y_max" = 0.0,
    "delta_head_location_y_SD" = 0.0, "delta_head_location_y_skewness" = 0.0, "delta_head_location_y_kurtosis" = 0.0,
    "delta_head_location_y_IQR_1_2" = 0.0, "delta_head_location_y_IQR_2_3" = 0.0, "delta_head_location_y_IQR_1_3" = 0.0,
    "delta_head_location_y_LR_intercept" = 0.0, "delta_head_location_y_LR_slope" = 0.0,
    
    "delta_head_location_z_min" = 0.0, "delta_head_location_z_quartile_1" = 0.0, "delta_head_location_z_median" = 0.0,
    "delta_head_location_z_mean" = 0.0, "delta_head_location_z_quartile_3" = 0.0, "delta_head_location_z_max" = 0.0,
    "delta_head_location_z_SD" = 0.0, "delta_head_location_z_skewness" = 0.0, "delta_head_location_z_kurtosis" = 0.0,
    "delta_head_location_z_IQR_1_2" = 0.0, "delta_head_location_z_IQR_2_3" = 0.0, "delta_head_location_z_IQR_1_3" = 0.0,
    "delta_head_location_z_LR_intercept" = 0.0, "delta_head_location_z_LR_slope" = 0.0,
    
    "head_rotation_x_min" = 0.0, "head_rotation_x_quartile_1" = 0.0, "head_rotation_x_median" = 0.0,
    "head_rotation_x_mean" = 0.0, "head_rotation_x_quartile_3" = 0.0, "head_rotation_x_max" = 0.0,
    "head_rotation_x_SD" = 0.0, "head_rotation_x_skewness" = 0.0, "head_rotation_x_kurtosis" = 0.0,
    "head_rotation_x_IQR_1_2" = 0.0, "head_rotation_x_IQR_2_3" = 0.0, "head_rotation_x_IQR_1_3" = 0.0,
    "head_rotation_x_LR_intercept" = 0.0, "head_rotation_x_LR_slope" = 0.0,
    
    "head_rotation_y_min" = 0.0, "head_rotation_y_quartile_1" = 0.0, "head_rotation_y_median" = 0.0,
    "head_rotation_y_mean" = 0.0, "head_rotation_y_quartile_3" = 0.0, "head_rotation_y_max" = 0.0,
    "head_rotation_y_SD" = 0.0, "head_rotation_y_skewness" = 0.0, "head_rotation_y_kurtosis" = 0.0,
    "head_rotation_y_IQR_1_2" = 0.0, "head_rotation_y_IQR_2_3" = 0.0, "head_rotation_y_IQR_1_3" = 0.0,
    "head_rotation_y_LR_intercept" = 0.0, "head_rotation_y_LR_slope" = 0.0,
    
    "head_rotation_z_min" = 0.0, "head_rotation_z_quartile_1" = 0.0, "head_rotation_z_median" = 0.0,
    "head_rotation_z_mean" = 0.0, "head_rotation_z_quartile_3" = 0.0, "head_rotation_z_max" = 0.0,
    "head_rotation_z_SD" = 0.0, "head_rotation_z_skewness" = 0.0, "head_rotation_z_kurtosis" = 0.0,
    "head_rotation_z_IQR_1_2" = 0.0, "head_rotation_z_IQR_2_3" = 0.0, "head_rotation_z_IQR_1_3" = 0.0,
    "head_rotation_z_LR_intercept" = 0.0, "head_rotation_z_LR_slope" = 0.0,
    
    "delta_head_rotation_x_min" = 0.0, "delta_head_rotation_x_quartile_1" = 0.0, "delta_head_rotation_x_median" = 0.0,
    "delta_head_rotation_x_mean" = 0.0, "delta_head_rotation_x_quartile_3" = 0.0, "delta_head_rotation_x_max" = 0.0,
    "delta_head_rotation_x_SD" = 0.0, "delta_head_rotation_x_skewness" = 0.0, "delta_head_rotation_x_kurtosis" = 0.0,
    "delta_head_rotation_x_IQR_1_2" = 0.0, "delta_head_rotation_x_IQR_2_3" = 0.0, "delta_head_rotation_x_IQR_1_3" = 0.0,
    "delta_head_rotation_x_LR_intercept" = 0.0, "delta_head_rotation_x_LR_slope" = 0.0,
    
    "delta_head_rotation_y_min" = 0.0, "delta_head_rotation_y_quartile_1" = 0.0, "delta_head_rotation_y_median" = 0.0,
    "delta_head_rotation_y_mean" = 0.0, "delta_head_rotation_y_quartile_3" = 0.0, "delta_head_rotation_y_max" = 0.0,
    "delta_head_rotation_y_SD" = 0.0, "delta_head_rotation_y_skewness" = 0.0, "delta_head_rotation_y_kurtosis" = 0.0,
    "delta_head_rotation_y_IQR_1_2" = 0.0, "delta_head_rotation_y_IQR_2_3" = 0.0, "delta_head_rotation_y_IQR_1_3" = 0.0,
    "delta_head_rotation_y_LR_intercept" = 0.0, "delta_head_rotation_y_LR_slope" = 0.0,
    
    "delta_head_rotation_z_min" = 0.0, "delta_head_rotation_z_quartile_1" = 0.0, "delta_head_rotation_z_median" = 0.0,
    "delta_head_rotation_z_mean" = 0.0, "delta_head_rotation_z_quartile_3" = 0.0, "delta_head_rotation_z_max" = 0.0,
    "delta_head_rotation_z_SD" = 0.0, "delta_head_rotation_z_skewness" = 0.0, "delta_head_rotation_z_kurtosis" = 0.0,
    "delta_head_rotation_z_IQR_1_2" = 0.0, "delta_head_rotation_z_IQR_2_3" = 0.0, "delta_head_rotation_z_IQR_1_3" = 0.0,
    "delta_head_rotation_z_LR_intercept" = 0.0, "delta_head_rotation_z_LR_slope" = 0.0,
    
    "unused_target_label" = 0.0  #  Regression target
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
  
  # Some string variables used for concatenation and assiting indexing into the LLD and output tables
  
  for(j in 1:dim(output_table)[1]) {
    head_location_x_summary_stats <- apply(
      LLD_table[,"head_location_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,summary
    )
    output_table[j,"head_location_x_min"] <- head_location_x_summary_stats[MIN]
    output_table[j,"head_location_x_quartile_1"] <- head_location_x_summary_stats[FirstQu]
    output_table[j,"head_location_x_median"] <- head_location_x_summary_stats[Median]
    output_table[j,"head_location_x_mean"] <- head_location_x_summary_stats[Mean]
    output_table[j,"head_location_x_quartile_3"] <- head_location_x_summary_stats[ThirdQu]
    output_table[j,"head_location_x_max"] <- head_location_x_summary_stats[MAX]
    output_table[j,"head_location_x_SD"] <- sd(
      rep(t(LLD_table[,"head_location_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_location_x_skewness"] <- skewness(
      rep(t(LLD_table[,"head_location_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_location_x_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"head_location_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_location_x_IQR_1_2"] <- 
      head_location_x_summary_stats[Median] - head_location_x_summary_stats[FirstQu]
    output_table[j,"head_location_x_IQR_2_3"] <- 
      head_location_x_summary_stats[ThirdQu] - head_location_x_summary_stats[Median]
    inter_quartile_range <- head_location_x_summary_stats[ThirdQu] - head_location_x_summary_stats[FirstQu]
    output_table[j,"head_location_x_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"head_location_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
      ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"head_location_x_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"head_location_x_LR_slope"] <- fit$coefficients[2]
    
    
    head_location_y_summary_stats <- apply(
      LLD_table[,"head_location_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,summary
    )
    output_table[j,"head_location_y_min"] <- head_location_y_summary_stats[MIN]
    output_table[j,"head_location_y_quartile_1"] <- head_location_y_summary_stats[FirstQu]
    output_table[j,"head_location_y_median"] <- head_location_y_summary_stats[Median]
    output_table[j,"head_location_y_mean"] <- head_location_y_summary_stats[Mean]
    output_table[j,"head_location_y_quartile_3"] <- head_location_y_summary_stats[ThirdQu]
    output_table[j,"head_location_y_max"] <- head_location_y_summary_stats[MAX]
    output_table[j,"head_location_y_SD"] <- sd(
      rep(t(LLD_table[,"head_location_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_location_y_skewness"] <- skewness(
      rep(t(LLD_table[,"head_location_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_location_y_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"head_location_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_location_y_IQR_1_2"] <-
      head_location_y_summary_stats[Median] - head_location_y_summary_stats[FirstQu]
    output_table[j,"head_location_y_IQR_2_3"] <-
      head_location_y_summary_stats[ThirdQu] - head_location_y_summary_stats[Median]
    inter_quartile_range <- head_location_y_summary_stats[ThirdQu] - head_location_y_summary_stats[FirstQu]
    output_table[j,"head_location_y_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"head_location_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
      ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"head_location_y_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"head_location_y_LR_slope"] <- fit$coefficients[2]
    
    
    head_location_z_summary_stats <- apply(
      LLD_table[,"head_location_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,summary
    )
    output_table[j,"head_location_z_min"] <- head_location_z_summary_stats[MIN]
    output_table[j,"head_location_z_quartile_1"] <- head_location_z_summary_stats[FirstQu]
    output_table[j,"head_location_z_median"] <- head_location_z_summary_stats[Median]
    output_table[j,"head_location_z_mean"] <- head_location_z_summary_stats[Mean]
    output_table[j,"head_location_z_quartile_3"] <- head_location_z_summary_stats[ThirdQu]
    output_table[j,"head_location_z_max"] <- head_location_z_summary_stats[MAX]
    output_table[j,"head_location_z_SD"] <- sd(
      rep(t(LLD_table[,"head_location_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_location_z_skewness"] <- skewness(
      rep(t(LLD_table[,"head_location_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_location_z_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"head_location_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_location_z_IQR_1_2"] <- 
      head_location_z_summary_stats[Median] - head_location_z_summary_stats[FirstQu]
    output_table[j,"head_location_z_IQR_2_3"] <-
      head_location_z_summary_stats[ThirdQu] - head_location_z_summary_stats[Median]
    inter_quartile_range <- head_location_z_summary_stats[ThirdQu] - head_location_z_summary_stats[FirstQu]
    output_table[j,"head_location_z_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"head_location_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
      ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"head_location_z_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"head_location_z_LR_slope"] <- fit$coefficients[2]
    
    
    delta_head_location_x_summary_stats <- apply(
      LLD_table[,"delta_head_location_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,
      summary
    )
    output_table[j,"delta_head_location_x_min"] <- delta_head_location_x_summary_stats[MIN]
    output_table[j,"delta_head_location_x_quartile_1"] <- delta_head_location_x_summary_stats[FirstQu]
    output_table[j,"delta_head_location_x_median"] <- delta_head_location_x_summary_stats[Median]
    output_table[j,"delta_head_location_x_mean"] <- delta_head_location_x_summary_stats[Mean]
    output_table[j,"delta_head_location_x_quartile_3"] <- delta_head_location_x_summary_stats[ThirdQu]
    output_table[j,"delta_head_location_x_max"] <- delta_head_location_x_summary_stats[MAX]
    output_table[j,"delta_head_location_x_SD"] <- sd(
      rep(t(LLD_table[,"delta_head_location_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_location_x_skewness"] <- skewness(
      rep(t(LLD_table[,"delta_head_location_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_location_x_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"delta_head_location_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_location_x_IQR_1_2"] <-
      delta_head_location_x_summary_stats[Median] - head_location_x_summary_stats[FirstQu]
    output_table[j,"delta_head_location_x_IQR_2_3"] <-
      delta_head_location_x_summary_stats[ThirdQu] - head_location_x_summary_stats[Median]
    inter_quartile_range <- delta_head_location_x_summary_stats[ThirdQu] - delta_head_location_x_summary_stats[FirstQu]
    output_table[j,"delta_head_location_x_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"delta_head_location_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      )) ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"delta_head_location_x_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"delta_head_location_x_LR_slope"] <- fit$coefficients[2]
    
    
    delta_head_location_y_summary_stats <- apply(
      LLD_table[,"delta_head_location_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,
      summary
    )
    output_table[j,"delta_head_location_y_min"] <- delta_head_location_y_summary_stats[MIN]
    output_table[j,"delta_head_location_y_quartile_1"] <- delta_head_location_y_summary_stats[FirstQu]
    output_table[j,"delta_head_location_y_median"] <- delta_head_location_y_summary_stats[Median]
    output_table[j,"delta_head_location_y_mean"] <- delta_head_location_y_summary_stats[Mean]
    output_table[j,"delta_head_location_y_quartile_3"] <- delta_head_location_y_summary_stats[ThirdQu]
    output_table[j,"delta_head_location_y_max"] <- delta_head_location_y_summary_stats[MAX]
    output_table[j,"delta_head_location_y_SD"] <- sd(
      rep(t(LLD_table[,"delta_head_location_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_location_y_skewness"] <- skewness(
      rep(t(LLD_table[,"delta_head_location_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_location_y_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"delta_head_location_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_location_y_IQR_1_2"] <-
      delta_head_location_y_summary_stats[Median] - delta_head_location_y_summary_stats[FirstQu]
    output_table[j,"delta_head_location_y_IQR_2_3"] <-
      delta_head_location_y_summary_stats[ThirdQu] - delta_head_location_y_summary_stats[Median]
    inter_quartile_range <- delta_head_location_y_summary_stats[ThirdQu] - delta_head_location_y_summary_stats[FirstQu]
    output_table[j,"delta_head_location_y_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"delta_head_location_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      )) ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"delta_head_location_y_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"delta_head_location_y_LR_slope"] <- fit$coefficients[2]
    
    
    delta_head_location_z_summary_stats <- apply(
      LLD_table[,"delta_head_location_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,
      summary
    )
    output_table[j,"delta_head_location_z_min"] <- delta_head_location_z_summary_stats[MIN]
    output_table[j,"delta_head_location_z_quartile_1"] <- delta_head_location_z_summary_stats[FirstQu]
    output_table[j,"delta_head_location_z_median"] <- delta_head_location_z_summary_stats[Median]
    output_table[j,"delta_head_location_z_mean"] <- delta_head_location_z_summary_stats[Mean]
    output_table[j,"delta_head_location_z_quartile_3"] <- delta_head_location_z_summary_stats[ThirdQu]
    output_table[j,"delta_head_location_z_max"] <- delta_head_location_z_summary_stats[MAX]
    output_table[j,"delta_head_location_z_SD"] <- sd(
      rep(t(LLD_table[,"delta_head_location_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_location_z_skewness"] <- skewness(
      rep(t(LLD_table[,"delta_head_location_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_location_z_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"delta_head_location_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_location_z_IQR_1_2"] <-
      delta_head_location_z_summary_stats[Median] - delta_head_location_z_summary_stats[FirstQu]
    output_table[j,"delta_head_location_z_IQR_2_3"] <-
      delta_head_location_z_summary_stats[ThirdQu] - delta_head_location_z_summary_stats[Median]
    inter_quartile_range <- head_location_z_summary_stats[ThirdQu] - delta_head_location_z_summary_stats[FirstQu]
    output_table[j,"delta_head_location_z_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"delta_head_location_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      )) ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"delta_head_location_z_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"delta_head_location_z_LR_slope"] <- fit$coefficients[2]
    
    
    head_rotation_x_summary_stats <- apply(
      LLD_table[,"head_rotation_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,summary)
    output_table[j,"head_rotation_x_min"] <- head_rotation_x_summary_stats[MIN]
    output_table[j,"head_rotation_x_quartile_1"] <- head_rotation_x_summary_stats[FirstQu]
    output_table[j,"head_rotation_x_median"] <- head_rotation_x_summary_stats[Median]
    output_table[j,"head_rotation_x_mean"] <- head_rotation_x_summary_stats[Mean]
    output_table[j,"head_rotation_x_quartile_3"] <- head_rotation_x_summary_stats[ThirdQu]
    output_table[j,"head_rotation_x_max"] <- head_rotation_x_summary_stats[MAX]
    output_table[j,"head_rotation_x_SD"] <- sd(
      rep(t(LLD_table[,"head_rotation_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_rotation_x_skewness"] <- skewness(
      rep(t(LLD_table[,"head_rotation_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_rotation_x_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"head_rotation_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_rotation_x_IQR_1_2"] <-
      head_rotation_x_summary_stats[Median] - head_rotation_x_summary_stats[FirstQu]
    output_table[j,"head_rotation_x_IQR_2_3"] <-
      head_rotation_x_summary_stats[ThirdQu] - head_rotation_x_summary_stats[Median]
    inter_quartile_range <- head_rotation_x_summary_stats[ThirdQu] - head_rotation_x_summary_stats[FirstQu]
    output_table[j,"head_rotation_x_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"head_rotation_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
      ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"head_rotation_x_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"head_rotation_x_LR_slope"] <- fit$coefficients[2]
    
    
    head_rotation_y_summary_stats <- apply(
      LLD_table[,"head_rotation_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,summary)
    output_table[j,"head_rotation_y_min"] <- head_rotation_y_summary_stats[MIN]
    output_table[j,"head_rotation_y_quartile_1"] <- head_rotation_y_summary_stats[FirstQu]
    output_table[j,"head_rotation_y_median"] <- head_rotation_y_summary_stats[Median]
    output_table[j,"head_rotation_y_mean"] <- head_rotation_y_summary_stats[Mean]
    output_table[j,"head_rotation_y_quartile_3"] <- head_rotation_y_summary_stats[ThirdQu]
    output_table[j,"head_rotation_y_max"] <- head_rotation_y_summary_stats[MAX]
    output_table[j,"head_rotation_y_SD"] <- sd(
      rep(t(LLD_table[,"head_rotation_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_rotation_y_skewness"] <- skewness(
      rep(t(LLD_table[,"head_rotation_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_rotation_y_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"head_rotation_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_rotation_y_IQR_1_2"] <-
      head_rotation_y_summary_stats[Median] - head_rotation_y_summary_stats[FirstQu]
    output_table[j,"head_rotation_y_IQR_2_3"] <-
      head_rotation_y_summary_stats[ThirdQu] - head_rotation_y_summary_stats[Median]
    inter_quartile_range <- head_rotation_y_summary_stats[ThirdQu] - head_rotation_y_summary_stats[FirstQu]
    output_table[j,"head_rotation_y_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"head_rotation_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
      ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))])
    output_table[j,"head_rotation_y_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"head_rotation_y_LR_slope"] <- fit$coefficients[2]
    
    
    head_rotation_z_summary_stats <- apply(
      LLD_table[,"head_rotation_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,summary)
    output_table[j,"head_rotation_z_min"] <- head_rotation_z_summary_stats[MIN]
    output_table[j,"head_rotation_z_quartile_1"] <- head_rotation_z_summary_stats[FirstQu]
    output_table[j,"head_rotation_z_median"] <- head_rotation_z_summary_stats[Median]
    output_table[j,"head_rotation_z_mean"] <- head_rotation_z_summary_stats[Mean]
    output_table[j,"head_rotation_z_quartile_3"] <- head_rotation_z_summary_stats[ThirdQu]
    output_table[j,"head_rotation_z_max"] <- head_rotation_z_summary_stats[MAX]
    output_table[j,"head_rotation_z_SD"] <- sd(
      rep(t(LLD_table[,"head_rotation_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_rotation_z_skewness"] <- skewness(
      rep(t(LLD_table[,"head_rotation_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_rotation_z_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"head_rotation_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
    )
    output_table[j,"head_rotation_z_IQR_1_2"] <-
      head_rotation_z_summary_stats[Median] - head_rotation_z_summary_stats[FirstQu]
    output_table[j,"head_rotation_z_IQR_2_3"] <-
      head_rotation_z_summary_stats[ThirdQu] - head_rotation_z_summary_stats[Median]
    inter_quartile_range <- head_rotation_z_summary_stats[ThirdQu] - head_rotation_z_summary_stats[FirstQu]
    output_table[j,"head_rotation_z_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"head_rotation_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]))
      ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"head_rotation_z_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"head_rotation_z_LR_slope"] <- fit$coefficients[2]
    
    
    delta_head_rotation_x_summary_stats <- apply(
      LLD_table[,"delta_head_rotation_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,
      summary
    )
    output_table[j,"delta_head_rotation_x_min"] <- delta_head_rotation_x_summary_stats[MIN]
    output_table[j,"delta_head_rotation_x_quartile_1"] <- delta_head_rotation_x_summary_stats[FirstQu]
    output_table[j,"delta_head_rotation_x_median"] <- delta_head_rotation_x_summary_stats[Median]
    output_table[j,"delta_head_rotation_x_mean"] <- delta_head_rotation_x_summary_stats[Mean]
    output_table[j,"delta_head_rotation_x_quartile_3"] <- delta_head_rotation_x_summary_stats[ThirdQu]
    output_table[j,"delta_head_rotation_x_max"] <- delta_head_rotation_x_summary_stats[MAX]
    output_table[j,"delta_head_rotation_x_SD"] <- sd(
      rep(t(LLD_table[,"delta_head_rotation_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_rotation_x_skewness"] <- skewness(
      rep(t(LLD_table[,"delta_head_rotation_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_rotation_x_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"delta_head_rotation_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_rotation_x_IQR_1_2"] <-
      delta_head_rotation_x_summary_stats[Median] - head_rotation_x_summary_stats[FirstQu]
    output_table[j,"delta_head_rotation_x_IQR_2_3"] <-
      delta_head_rotation_x_summary_stats[ThirdQu] - head_rotation_x_summary_stats[Median]
    inter_quartile_range <- delta_head_rotation_x_summary_stats[ThirdQu] - delta_head_rotation_x_summary_stats[FirstQu]
    output_table[j,"delta_head_rotation_x_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"delta_head_rotation_x"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      )) ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"delta_head_rotation_x_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"delta_head_rotation_x_LR_slope"] <- fit$coefficients[2]
    
    
    delta_head_rotation_y_summary_stats <- apply(
      LLD_table[,"delta_head_rotation_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,
      summary
    )
    output_table[j,"delta_head_rotation_y_min"] <- delta_head_rotation_y_summary_stats[MIN]
    output_table[j,"delta_head_rotation_y_quartile_1"] <- delta_head_rotation_y_summary_stats[FirstQu]
    output_table[j,"delta_head_rotation_y_median"] <- delta_head_rotation_y_summary_stats[Median]
    output_table[j,"delta_head_rotation_y_mean"] <- delta_head_rotation_y_summary_stats[Mean]
    output_table[j,"delta_head_rotation_y_quartile_3"] <- delta_head_rotation_y_summary_stats[ThirdQu]
    output_table[j,"delta_head_rotation_y_max"] <- delta_head_rotation_y_summary_stats[MAX]
    output_table[j,"delta_head_rotation_y_SD"] <- sd(
      rep(t(LLD_table[,"delta_head_rotation_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_rotation_y_skewness"] <- skewness(
      rep(t(LLD_table[,"delta_head_rotation_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_rotation_y_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"delta_head_rotation_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_rotation_y_IQR_1_2"] <-
      delta_head_rotation_y_summary_stats[Median] - delta_head_rotation_y_summary_stats[FirstQu]
    output_table[j,"delta_head_rotation_y_IQR_2_3"] <-
      delta_head_rotation_y_summary_stats[ThirdQu] - delta_head_rotation_y_summary_stats[Median]
    inter_quartile_range <- delta_head_rotation_y_summary_stats[ThirdQu] - delta_head_rotation_y_summary_stats[FirstQu]
    output_table[j,"delta_head_rotation_y_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"delta_head_rotation_y"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      )) ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"delta_head_rotation_y_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"delta_head_rotation_y_LR_slope"] <- fit$coefficients[2]
    
    
    delta_head_rotation_z_summary_stats <- apply(
      LLD_table[,"delta_head_rotation_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))],2,
      summary
    )
    output_table[j,"delta_head_rotation_z_min"] <- delta_head_rotation_z_summary_stats[MIN]
    output_table[j,"delta_head_rotation_z_quartile_1"] <- delta_head_rotation_z_summary_stats[FirstQu]
    output_table[j,"delta_head_rotation_z_median"] <- delta_head_rotation_z_summary_stats[Median]
    output_table[j,"delta_head_rotation_z_mean"] <- delta_head_rotation_z_summary_stats[Mean]
    output_table[j,"delta_head_rotation_z_quartile_3"] <- delta_head_rotation_z_summary_stats[ThirdQu]
    output_table[j,"delta_head_rotation_z_max"] <- delta_head_rotation_z_summary_stats[MAX]
    output_table[j,"delta_head_rotation_z_SD"] <- sd(
      rep(t(LLD_table[,"delta_head_rotation_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_rotation_z_skewness"] <- skewness(
      rep(t(LLD_table[,"delta_head_rotation_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_rotation_z_kurtosis"] <- kurtosis(
      rep(t(LLD_table[,"delta_head_rotation_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      ))
    )
    output_table[j,"delta_head_rotation_z_IQR_1_2"] <-
      delta_head_rotation_z_summary_stats[Median] - delta_head_rotation_z_summary_stats[FirstQu]
    output_table[j,"delta_head_rotation_z_IQR_2_3"] <-
      delta_head_rotation_z_summary_stats[ThirdQu] - delta_head_rotation_z_summary_stats[Median]
    inter_quartile_range <- head_rotation_z_summary_stats[ThirdQu] - delta_head_rotation_z_summary_stats[FirstQu]
    output_table[j,"delta_head_rotation_z_IQR_1_3"] <- inter_quartile_range
    fit <- lm(
      rep(t(LLD_table[,"delta_head_rotation_z"][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
      )) ~ mytable[,timestamp][avg_counter:(avg_counter+(feature_calculation_sliding_window_size - 1))]
    )
    output_table[j,"delta_head_rotation_z_LR_intercept"] <- fit$coefficients["(Intercept)"]
    output_table[j,"delta_head_rotation_z_LR_slope"] <- fit$coefficients[2]
    
    avg_counter <- avg_counter + 1
  }

  return(output_table)
}
