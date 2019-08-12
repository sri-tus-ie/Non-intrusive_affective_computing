#### This function calculates the low-level descriptors (LLDs) eye gaze angle change, gaze fixation,
#### gaze approach (distance) pupil diameter change, pupil dilation & constriction.
#
# Copyright: Athlone Institute of Technology, 2019
# Author: j.odwyer@research.ait.ie

calculate_LLDs <- function(mytable, frames_per_second) {
  
  # According to Lappi, 2016 there are 3.2 degrees/sec eye movement on fixation. We transform to frame-wise radians
  fixation_radians_per_frame <- (0.055851 / frames_per_second)
  eyes_fixated_gaze <- t(rep(1, dim(mytable)[1]))
  eye_gaze_approach <- t(rep(1, dim(mytable)[1]))
  pupil_dilation <- rep(0, dim(mytable)[1])      #  Initialise these values to 0, static pupils or unknown pupil change
  pupil_constriction <- rep(0, dim(mytable)[1])
  
  output_table <- data.table(
    "delta_gaze_angle_x" = 0.0,
    "delta_gaze_angle_y" = 0.0,
    "gaze_fixation" = 1:dim(mytable)[1],
    "gaze_approach" = 0.0,
    "pupil_diameter_mm" = 0.0, "delta_pupil_diameter_mm" = 0.0, 
    "pupil_dilation" = pupil_dilation, "pupil_constriction" = pupil_constriction
  )
  
  # Gather some raw data into vectors, then eye gaze angles, distance numerical calculations
  gaze_angle_radians_x <- mytable$gaze_angle_x
  gaze_angle_radians_y <- mytable$gaze_angle_y
  eye_landmark_Z_8 <- mytable$eye_lmk_Z_8
  eye_landmark_Z_42 <- mytable$eye_lmk_Z_42
  
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
  
  delta_gaze_angle_x <- c(0,diff(gaze_angle_radians_x))
  delta_gaze_angle_y <- c(0,diff(gaze_angle_radians_y))
  gaze_distance_mm <- (eye_landmark_Z_8 + eye_landmark_Z_42) / 2  #  Calculate gaze distance
  delta_gaze_distance <- diff(gaze_distance_mm)
  
  # Pupil numerical calculation
  # left_pupil_x_diameter <- abs(mytable[,2]-mytable[,3])  #  Error, estimation will always be x diameter. Have left
  # left_pupil_y_diameter <- abs(mytable[,4]-mytable[,5])  #  code below which provided ACII 2019 result for reproducing
  # pupil_diameter_mm <- pmax(left_pupil_x_diameter, left_pupil_y_diameter)
  pupil_diameter_mm <- abs(mytable$left_pupil_x2 - mytable$left_pupil_x1)
  delta_pupil_diameter_mm <- c(0,diff(as.matrix(pupil_diameter_mm)))
  
  # LLD decisions for logical variables
  eyes_fixated_gaze[gaze_theta <= fixation_radians_per_frame] <- 0  # eyes fixated (=1) or scanning (=0) determination
  eye_gaze_approach[delta_gaze_distance < 0] <- 0  # gaze approach or static (=1) or avoidance (=0) determination
  pupil_dilation[delta_pupil_diameter_mm > 0] <- 1      #  pupil dilation (=1) or static/constriction (=0) determination
  pupil_constriction[delta_pupil_diameter_mm < 0] <- 1  #  pupil constriction (=1) or static/dilation (=0) determination 
  
  # write the calculated data to the output results table
  output_table[,"delta_gaze_angle_x"] <- delta_gaze_angle_x
  output_table[,"delta_gaze_angle_y"] <- delta_gaze_angle_y
  output_table[,"gaze_fixation"] <- t(eyes_fixated_gaze)
  output_table[,"gaze_approach"] <- t(eye_gaze_approach)
  output_table[,"pupil_diameter_mm"] <- pupil_diameter_mm
  output_table[,"delta_pupil_diameter_mm"] <- delta_pupil_diameter_mm
  output_table[,"pupil_dilation"] <- pupil_dilation
  output_table[,"pupil_constriction"] <- pupil_constriction

  return(output_table)

}
