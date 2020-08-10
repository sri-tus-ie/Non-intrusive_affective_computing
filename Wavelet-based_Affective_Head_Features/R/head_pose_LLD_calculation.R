#### This function calculates the low-level descriptors (LLDs) for head pose, namely head location, rotation, and delta
#### of these in 3 axes
#
# Copyright: Athlone Institute of Technology, 2018
# Author: j.odwyer@research.ait.ie

calculate_LLDs <- function(mytable) {
  #### Table that will be used to store calculated LLD data for returning
  output_table <- data.table(
    # 12 x Low-level descriptors (LLD) include head locations, rotations, and delta of these
    "head_location_x" = 1:floor(dim(mytable)[1]), "delta_head_location_x" = 0.0, 
    "head_location_y" = 0.0, "delta_head_location_y" = 0.0,
    "head_location_z" = 0.0, "delta_head_location_z" = 0.0,
    "head_rotation_x" = 0.0, "delta_head_rotation_x" = 0.0, 
    "head_rotation_y" = 0.0, "delta_head_rotation_y" = 0.0,
    "head_rotation_z" = 0.0, "delta_head_rotation_z" = 0.0
  )
  
  # Gather raw data into vectors
  head_location_x <- rep(t(mytable[,"pose_Tx"][1:dim(mytable)[1]])) 
  head_location_y <- rep(t(mytable[,"pose_Ty"][1:dim(mytable)[1]]))
  head_location_z <- rep(t(mytable[,"pose_Tz"][1:dim(mytable)[1]]))
  head_rotation_x <- rep(t(mytable[,"pose_Rx"][1:dim(mytable)[1]]))
  head_rotation_y <- rep(t(mytable[,"pose_Ry"][1:dim(mytable)[1]]))
  head_rotation_z <- rep(t(mytable[,"pose_Rz"][1:dim(mytable)[1]]))
  
  # LLD calculation (deltas of raw data). The first delta is 0
  delta_head_location_x <- c(0,diff(head_location_x))
  delta_head_location_y <- c(0,diff(head_location_y))
  delta_head_location_z <- c(0,diff(head_location_z))
  delta_head_rotation_x <- c(0,diff(head_rotation_x))
  delta_head_rotation_y <- c(0,diff(head_rotation_y))
  delta_head_rotation_z <- c(0,diff(head_rotation_z))
  
  
  # write the data to the output results table
  output_table[, "head_location_x"] <- head_location_x
  output_table[, "delta_head_location_x"] <- delta_head_location_x
  output_table[, "head_location_y"] <- head_location_y
  output_table[, "delta_head_location_y"] <- delta_head_location_y
  output_table[, "head_location_z"] <- head_location_z
  output_table[, "delta_head_location_z"] <- delta_head_location_z
  output_table[, "head_rotation_x"] <- head_rotation_x
  output_table[, "delta_head_rotation_x"] <- delta_head_rotation_x
  output_table[, "head_rotation_y"] <- head_rotation_y
  output_table[, "delta_head_rotation_y"] <- delta_head_rotation_y
  output_table[, "head_rotation_z"] <- head_rotation_z
  output_table[, "delta_head_rotation_z"] <- delta_head_rotation_z
  
  return(output_table)
}