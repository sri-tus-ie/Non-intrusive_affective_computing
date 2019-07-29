#### This function calculates the low-level descriptors (LLDs) for eye gaze, namely, whether the eyes are scanning or
#### fixated, whether the gaze is shortened (approach-oriented gaze) or lengthened (avoidance oriented gaze), and 
#### the delta (or change) in the x,y eye gaze angles. Eye blink binary and intensity features are also gathered 
#### here.
#
# Copyright: Athlone Institute of Technology, 2019
# Author: j.odwyer@research.ait.ie

calculate_LLDs <- function(mytable) {
  
  # According to Lappi, 2016 there are 3.2 degrees/sec eye movement on fixation. We transform to frame-wise radians
  fixation_radians_per_frame <- (0.055851 / frames_per_second)
  eyes_fixated_gaze <- t(rep(1, dim(mytable)[1]))
  eye_gaze_approach <- t(rep(1, dim(mytable)[1]))
  
  output_table <- data.table(
    # 6 x Low-level descriptors (LLD) include delta of gaze angles, eyes fixated, eyes gaze approach, and eye blink/intensity
    "delta_gaze_angle_x" = 0.0,
    "delta_gaze_angle_y" = 0.0,
    "eye_blink" = 0.0,
    "eye_blink_intensity_value" = 0.0,
    "eyes_fixated" = 1:dim(mytable)[1],
    "eye_gaze_approach" = 0.0
  )
  
  # Gather raw data into vectors
  gaze_angle_radians_x <- rep(t(mytable[,"gaze_angle_x"][1:dim(mytable)[1]]))
  gaze_angle_radians_y <- rep(t(mytable[,"gaze_angle_y"][1:dim(mytable)[1]]))
  eye_landmark_Z_8 <- rep(t(mytable[,"eye_lmk_Z_8"][1:dim(mytable)[1]]))
  eye_landmark_Z_42 <- rep(t(mytable[,"eye_lmk_Z_42"][1:dim(mytable)[1]]))
  
  vector_b <- rbind(tail(gaze_angle_radians_x, -1), tail(gaze_angle_radians_y, -1))
  vector_a <- rbind(
    gaze_angle_radians_x[-length(gaze_angle_radians_x)], gaze_angle_radians_y[-length(gaze_angle_radians_y)]
  )
  
  gaze_theta <- t(rep(0, length(vector_a)/2))  #  Calculate angular distances moved in each frame
  for(i in 1:length(vector_a)/2){
    theta <- acos(
      sum(vector_a[,i] * vector_b[,i]) / 
      (sqrt(sum(vector_a[,i] * vector_a[,i])) * sqrt(sum(vector_b[,i] * vector_b[,i])))
    )
    if(is.na(theta))
      gaze_theta[i] <- 0
    else
      gaze_theta[i] <- theta
  }
  
  gaze_distance_pixels <- (eye_landmark_Z_8 + eye_landmark_Z_42) / 2  #  Calculate gaze distance
  delta_gaze_distance <- diff(gaze_distance_pixels)
  
  
  # LLD decision logic
  eyes_fixated_gaze[gaze_theta <= fixation_radians_per_frame] <- 0  # eyes fixated (=1) or scanning (=0) determination
  eye_gaze_approach[delta_gaze_distance < 0] <- 0  # gaze approach or static (=1) or avoidance (=0) determination
  
  # LLD calculation (deltas of raw data). The first delta is 0
  delta_gaze_angle_x <- c(0,diff(gaze_angle_radians_x))
  delta_gaze_angle_y <- c(0,diff(gaze_angle_radians_y))
  
  # write the data to the output results table
  output_table[,"delta_gaze_angle_x"] <- delta_gaze_angle_x
  output_table[,"delta_gaze_angle_y"] <- delta_gaze_angle_y
  output_table[,"eye_blink"] <- rep(t(mytable[,"AU45_c"][1:dim(mytable)[1]]))
  output_table[,"eye_blink_intensity_value"] <- rep(t(mytable[,"AU45_r"][1:dim(mytable)[1]]))
  output_table[,"eyes_fixated"] <- t(eyes_fixated_gaze)
  output_table[,"eye_gaze_approach"] <- t(eye_gaze_approach)

  return(output_table)

}