#################################################################################################
#### This file contains a function to write a data.frame to Cuda recurrent neural network (CURRENNT)
#### netcdf format with one regression target.
#
# Instructions for use: 
#   install.packages("ncdf4")
#   source("~/WHERE/EVER/YOU/ARE/KEEPING/THIS/FILE/f_write_CURRENNT_regression_target.netcdf.R")
#   Pass this function a data.frame and filename that you wish to use e.g. write.netcdf(mydata, "val.nc")
#
# Parameters: (data.frame) the data.frame you are writing, (string) the filename you wish for 
# Returns: Nothing, writes a netcdf file with the filename that you have specified into your directory
#
# Copyright: Athlone Institute of Technology, 2018
# Author: j.odwyer@research.ait.ie

library(ncdf4)

write.CURRENNT.regression.target.netcdf = function(mydata, filenamestring) {
  
  # Variables for netCDF file
  targets <- t(as.matrix(mydata[, length(mydata)]))
  inVals <- t(mydata[, -length(mydata)])
  sequences <- t(rep(1, dim(mydata)[1]))
  tags <- t(rep("t", dim(mydata)[1]))

  # Dimensions for netCDF file
  numSeqs <- ncdim_def("numSeqs", "", 1:dim(mydata)[1])                       #  Number of sequences           
  numTimesteps <- ncdim_def("numTimesteps", "", 1:dim(mydata)[1])      #  Total number of timesteps
  inputPattSize <- ncdim_def("inputPattSize", "", 1:dim(inVals)[1])  #  Size of each input pattern
  maxSeqTagLength <- ncdim_def("maxSeqTagLength", "", 4000)       #  Maximum length of a sequence tag
  targetPattSize <- ncdim_def("targetPattSize", "", 1:dim(targets)[1])     #  Size of each output pattern
  
  # Required variables for netCDF file
  seqTags <- ncvar_def(                      #  Tag (name) for each sequence
    name="seqTags", units="", dim=list(maxSeqTagLength, numSeqs), missval=NULL,
    longname="sequence tags", prec="char" 
  )
  seqLengths <- ncvar_def(                   #  Length of each sequence
    name="seqLengths", units="", dim=numSeqs, missval=1, longname="sequence seqLengths",
    prec="integer"
  )
  inputs <- ncvar_def(                        #  Input Patterns
    name="inputs", units="", dim=list(inputPattSize, numTimesteps), missval=NULL,
    longname="inputs adjusted for mean 0 and std dev 1", prec="float"
  )
  targetPatterns <- ncvar_def(                #  Target patterns
    name="targetPatterns", units="", dim=list(targetPattSize, numTimesteps), missval=NULL,
    longname="regression targets",prec="float"
  )
  
  nc <- nc_create(
    filename = filenamestring, vars = list(seqTags, seqLengths, inputs, targetPatterns),
    force_v4=FALSE, verbose=FALSE
  )
  
  ncvar_put(nc, seqTags, tags)
  ncvar_put(nc, seqLengths, sequences)
  ncvar_put(nc, inputs, inVals)
  ncvar_put(nc, targetPatterns, targets)
  
  # Close the file, writing data to disk
  nc_close(nc)
}
