# Creates a Predicted outpput for Imagery
library(spectrolab)
library(randomForest)
library(raster)
library(tidyverse)
library(hsdar)
library(randomcoloR)
library(randomForestExplainer)

# Lets create names for output and input folders
# Input folder is the dir path to where yor data is stored
# output folder is the dir path to where you want your processed data to be stored
# Replace these before running 
outputs_folder<-"/Forests/ROutputs/"

#input_folder  <-"/Forests/"
input_folder2 <-"/Forests/ROutputs/Predictors"

# Import names of Hyperspectral datcubes into a list
# For now we'll work with headwall datacubes
# This could be incorporated in the function by doing something like 
# list.files (file_location, pattern = ".dat| .tif") and other readable hyperspectral file types
names_HyperspecPreds= list.files(input_folder2, 
                                 pattern = "images_preds",full.names = T)

names_validationData<-list.files(input_folder2,
                                 pattern = "all_preds_validation",full.names = T)


# Reads in spectral library and their predictors for each sensor
validationData<-lapply(names_validationData,read.csv)%>% 
  
  # Removes dir path from the name
  setNames(gsub("/Forests/ROutputs/","",names_validationData)) 

# Reads in spectral library and their predictors for each sensor
HyperspecPreds<-lapply(names_HyperspecPreds,read.csv)%>% 
  
  # Removes dir path from the name
  setNames(gsub(input_folder2,"",names_HyperspecPreds)) # Lets Read in our model
ash_model<-load("/Forests/ROutputs/Predictors/all_ash_model_preds.rda")

# Lets Make a prediction for validation dataset
Raster_predictionVal<-lapply(validationData,function(x){
  results<-predict(get(ash_model),x[-1:-2])
  return(results)})

# Compares the actual vs predicted infection classes
actual<-as.data.frame(validationData[[1]]$Condition_)%>%'names<-'("actual_cond")
predicted<-Raster_predictionVal[[1]]%>%as.data.frame()%>%'names<-'("predicted_cond")

# overall classification accuracy
accuracy = mean (actual$actual_cond == predicted$predicted_cond)
error    = mean (actual$actual_cond != predicted$predicted_cond)

# creates confusing matrix
Confusion_matrix = table(actual$actual_cond, predicted$predicted_cond)

# Lets Make a prediction
Raster_prediction<-lapply(HyperspecPreds,function(x){
  results<-predict(get(ash_model),x[-1:-2])

  
  # Converts prediction from rf model to dataframe and changes column name to predicte
  Results_df<-as.data.frame(results)%>%
    'names<-'("predicted")
  
  # Creates Unique IDs for classes/categories 
  Unique_class<-unique(as.data.frame(Results_df$predicted)) 
  Unique_class$Category<-seq(1:nrow(Unique_class))
  names(Unique_class)[1]<-"predicted"
  
  # Create dataframe with unique class IDs and location info
  Results_final<-merge(Results_df,
                       Unique_class, 
                       by="predicted")
  
  Final<-cbind(x[1:2],Results_final)%>%
    dplyr::select(x,y,Category)
  
  # Converts Dataframes To Rasters
  rasterFromXYZ(Final, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
})%>%
  
  # Rename objects
  setNames(gsub(input_folder2,"",names_HyperspecPreds))


# Writes out each raster as a .tiff file
lapply(1:length(Raster_prediction), function (x) 
  writeRaster(Raster_prediction[[x]],
              file = paste(outputs_folder,
                           gsub("images_preds.csv","raster_preds",names (Raster_prediction[x])),
                           '.tif',sep=""), options=c('TFW=YES'), overwrite=TRUE))
