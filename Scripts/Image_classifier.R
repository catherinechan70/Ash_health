# ----------------------------------- Build a model on Spectral Library (Headwall) ------------------------------------
# List of packages to install
library(spectrolab)
library(tidyverse)
library(raster)
library(SpaDES)
library(doParallel)
library(parallel)
library(hsdar)
library(caret)
library(randomForest)
library(ranger)
library(tools)


# --------------------------------------------- Building Models ------------------------------------------------------
# Source the function that will calculate derivatives of our new spectral library
# The function will do the same for a datacube
source("/Forests/Functions/HyperspecGenFunctionRanger.R")
source("/Forests/Functions/LandCoverEstimator.R")


# Calculate Derivative for spectral libaray
Spectral_Library_mask<-HyperSpec_DerivGeneratorRanger("/Forests/ROutputs/all_cat_df.csv", out_file= "/Forests/ROutputs/all_cat")

Spectral_Library_mask<-LandCoverEstimator(filename= "/Forests/ROutputs/all_cat_df.csv", 
                                          out_file= "/Forests/ROutputs/landcover_allcat_derivs",
                                          datatype = "csv",
                                          extension = FALSE)

# Source Function That will Build randomForest model
###source("Functions/Spectral_classifier.R")

# Build Model
###Headwall_RFMOD<-Spectral_classifier("Output/D_002_SpecLib_Derivs.csv",out_file = "Output/")

# Save results
###save(Headwall_RFMOD,file = "Output/Headwall_model.rda")

# --------------------------------------------- Classify Raster ------------------------------------------------------
# Classify Image using the HyperSpec_DerivGenerator function
### STEP 1, CLASSIFYING THE ORIGINAL CUBE IMAGE
#"/Forests/OriginalData/Buffers/1_Buffer.tif"
system.time(PredLayer<-HyperSpec_DerivGeneratorRanger(filename = "/Forests/OriginalData/nhti_0_crop.tif",
                                     out_file = "/Forests/ROutputs/nhti_0_ashshad",
                                     Classif_Model = "/Forests/ROutputs/Best_Model10vars_ash_shad.rda"))


### STEP 2, CLASSIFYING THE MASKED IMAGES
system.time(PredLayer<-HyperSpec_DerivGeneratorRanger(filename = "/Forests/OriginalData/nhti_0_crop.tif",
                                                      out_file = "/Forests/ROutputs/MaskedImages/nhti_0_health2",
                                                      Classif_Model = "/Forests/ROutputs/Best_Model20vars_ash_health.rda"))

#source("/Forests/Functions/LandCoverEstimator")


#LandCoverEstimator<-function(filename,out_file,Classif_Model,datatype,extension){

system.time(PredLayer<-LandCoverEstimator(filename = "/Chan_Thesis_Missions/Ash_07262019/100131_ash_atkinson1_2019_07_27_13_58_10/raw_3983_rd_rf_or",
                                                        out_file = "/Forests/ROutputs/atkin1_3983_landcover_allcat",
                                                        Classif_Model = "/Forests/ROutputs/Best_Model25vars_landcover_allcat.rda",
                                                        datatype = "raster", 
                                                        extension = TRUE)) 
  
#/Thesis Data/Ash Trees/Imagesamples/ImageBuffers/nhti_2489_samps1.envi
#/Thesis Data/Ash Trees/Imagesamples/ImageBuffers/buffertest"

# Classify BUFFER images using the Hyperspec_DerivGenerator function

setwd("/Forests/OriginalData/Buffers/") # Set path for buffer samples
mypath_buffer = "/Forests/OriginalData/Buffers/"
# Import names of shadow sample tif files into character list
buffer_samples = list.files(mypath_buffer, pattern="\\.tif$")

buffer_raster_list<-lapply(buffer_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",buffer_samples))  # Substitutes file name and removes .tif from the name

output_path = "/Forests/ROutputs/ClassifiedBuffers/"

lapply(buffer_samples, function(x) {
  system.time(PredLayer <- HyperSpec_DerivGeneratorRanger(filename = x,
                            out_file =  "/Forests/ROutputs/ClassifiedBuffers/buffer",
                            Classif_Model = "/Forests/ROutputs/Best_Model10vars_ash_shad.rda"))
  
  
})

 
system.time(PredLayer <- lapply(buffer_samples, function(x){
                                                      HyperSpec_DerivGeneratorRanger(filename = x,
                                                      out_file =  "/Forests/ROutputs/ClassifiedBuffers/buffer",
                                                      Classif_Model = "/Forests/ROutputs/Best_Model10vars_ash_shad.rda")
}))
#file.path(dirname(mypath_buffer), gsub(basename(mypath_buffer)))
#"/Forests/ROutputs/ClassifiedBuffers/",
#Add column Tree_numbe to dataframe
#nhti_raster_list <- Map(cbind, nhti_raster_list, Tree_numbe = names(nhti_raster_list))

# E:/Chan_Thesis_Missions/Ash_07262019/100131_ash_atkinson1_2019_07_27_13_58_10/raw_15366_rd_rf_or
# E:/Chan_Thesis_Missions/Ash_07262019/100139_ash_hooksett2_2019_07_27_17_00_31/raw_2575_rd_rf_or
# E:/Forests/ROutputs/MaskedImages/Atkinson_15366_all_mask.tif"


