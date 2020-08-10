#### This function takes a specific OpenFace data.table input
# and returns output of 600 head pose wavelet features to make up part of
# ePoseVID feature set. 
#
# Feature vector is comprised of:
#     head location x, y, z and head rotation x, y, z (6 features)
#     * 4 levels Daubechies wavelet and scale coefficients
#     * 13 wavelet stats OR 12 scale stats
#     = (6 * 4 * 13) + (6 * 4 * 12) = 600 total wavelet features
#
# Copyright: Athlone Institute of Technology, 2020
# Author: j.odwyer@research.ait.ie

calculate_features <- function(mytable, frames_per_second, window_size_secs) {

    #### Libraries ####
    library(e1071)      #  used for kurtosis and skewness calculations
    library(wavelets)   #  wavelet decomposition
    library(seewave)    #  zero crossing rate, root mean square

    # Alter path below to suit your computer
    path <- "~/YOUR/DIRECTORY/R/"
    function_file <- "head_pose_LLD_calculation.R"
    source(paste(path, function_file, sep = ""))

    sliding_window_size <- frames_per_second * window_size_secs

    output <- data.table(
        "sample_number" = 1:floor((dim(mytable)[1]) - (sliding_window_size - 1))
    )

    LLD_table <- calculate_LLDs(mytable)

    # The xx_name & concat variables are used to build final feature names
    base_name <- c("db_wavelet_coeffs_l1_", "db_scale_coeffs_l1_",
                   "db_wavelet_coeffs_l2_", "db_scale_coeffs_l2",
                   "db_wavelet_coeffs_l3_", "db_scale_coeffs_l3",
                   "db_wavelet_coeffs_l4_", "db_scale_coeffs_l4"
                   )

    lld_name <- c("head_location_x", "head_location_y",
                  "head_location_z", "head_rotation_x",
                  "head_rotation_y", "head_rotation_z"
                  )

    concat <- c("_min", "_quartile_1", "_median", "_quartile_3",
                "_max", "_SD", "_skewness", "_kurtosis",
                "_IQR_1_2", "_IQR_2_3", "_IQR_1_3", "_RMS", "_ZCR"
                )

    wavelet_index <- c("W1", "V1", "W2", "V2", "W3", "V3", "W4", "V4")

    # Index variables for summary statistics tables
    MIN <- 1
    FirstQu <- 2
    Median <- 3
    ThirdQu <- 5
    MAX <- 6

    for (i in 1:dim(output)[1]) {

        range <- i:(i + (sliding_window_size - 1))

        for (index in 1:length(lld_name)) {

            dwt_coeffs_list <- {}
            for (index in 1:length(lld_name)) {
                dwt_coeffs_list <-
                    c(dwt_coeffs_list,
                      dwt(LLD_table[[lld_name[index]]][range],
                              filter="d10", n.levels=4,
                              boundary="reflection", fast=TRUE
                         )
                     )
            }
        }

        for (j in 1:length(lld_name)) {

            for (k in 1:length(base_name)) {

                if (k %% 2 != 0) {
                    coeffs <- dwt_coeffs_list[[j]]@"W"[[wavelet_index[k]]]
                    ZCR <- zcr(coeffs, f = frames_per_second,
                               wl = length(coeffs),
                               ovlp = 0, plot = FALSE
                               )
                } else {

                    coeffs <- dwt_coeffs_list[[j]]@"V"[[wavelet_index[k]]]

                }
                name <- paste(base_name[k], lld_name[j], sep = "")

                stats <- summary(as.numeric(coeffs))

                IQR_1_2 <- stats[Median] - stats[FirstQu]
                IQR_2_3 <- stats[ThirdQu] - stats[Median]
                IQR <- stats[ThirdQu] - stats[FirstQu]

                output[i, paste(name, concat[1], sep = "")] <- stats[MIN]
                output[i, paste(name, concat[2], sep = "")] <- stats[FirstQu]
                output[i, paste(name, concat[3], sep = "")] <- stats[Median]
                output[i, paste(name, concat[4], sep = "")] <- stats[ThirdQu]
                output[i, paste(name, concat[5], sep = "")] <- stats[MAX]
                output[i, paste(name, concat[6], sep = "")] <- sd(coeffs)
                output[i, paste(name, concat[7], sep = "")] <- skewness(coeffs)
                output[i, paste(name, concat[8], sep = "")] <- kurtosis(coeffs)
                output[i, paste(name, concat[9], sep = "")] <- IQR_1_2
                output[i, paste(name, concat[10], sep = "")] <- IQR_2_3
                output[i, paste(name, concat[11], sep = "")] <- IQR
                output[i, paste(name, concat[12], sep = "")] <- rms(coeffs)
                if (k %% 2 != 0)
                    output[i, paste(name, concat[13], sep = "")] <- ZCR[1,2]
            }
        }
    }

    output[,"unused_target_label"] <- 0.0
    return(output)
}
