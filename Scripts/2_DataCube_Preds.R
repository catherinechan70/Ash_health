## Scripts calculates the predictors for hyperspectral images
## Calculates reflectance values for bands resampled every 5, 10 , 50 and 100nm (Only headwall sensor)
## Calculates the vegitation index

library(spectrolab)
library(tidyverse)
library(hsdar)
library(parallel)

source("/Forests/Functions/HypIMGPredictor_generator.R")

# Create names for output and input folders
# Input folders are the dir path to where your data is stored
# Output folders are the dir path to where you want your processed data to be stored
# Replace these before running 
input_folder <-"/Forests/Data/Sub_images/"
outputs_folder<-"/Forests/ROutputs/Predictors/"

# Import names of Hyperspectral datcubes into a list
# For now we'll work with headwall datacubes
# This could be incorporated in the function by doing something like 
# list.files (file_location, pattern = ".dat| .tif") and other readable hyperspectral file types
names_HyperSpecImage = list.files(input_folder, 
                                  pattern = ".dat",full.names = T)[1:4]


# Reads in spectral library for each sensor
# This could also be incorporated into a function
Image_preds<-lapply(names_HyperSpecImage,ImagePredictor_generator)%>%
  # Renames objects
  setNames(gsub(input_folder,"",names_HyperSpecImage))

# Writes out each dataframe as a .csv file
lapply(1:length(Image_preds), function (x) 
  write.csv(Image_preds[[x]],
            file = paste(outputs_folder,
                         gsub(".dat","sub_images_preds",names (Image_preds[x])),
                         '.csv',sep=""), row.names = F))

# Cleans up R memeory
rm(list=ls())
