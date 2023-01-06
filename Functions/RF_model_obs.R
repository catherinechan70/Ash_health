## Builds a random forest model from PSR data
## Need to run scripts 1_By_site_PSR, 2, 3, 4 in folder "1_FieldSpec/" prior this script if the data is not present in the output folder below
## 'OutputsPSR/Processing/Sensors/'
## At the end of the script memeroy will be cleaned (to stop this put a # infront the last line of code)
library(tidyverse)
library(randomForest)
library(randomForestExplainer)
library(ranger)

# Reads in random forest classifier
# The model classifies the data based on the 50 most important variables

source("/Forests/Functions/Spectral_classifier_ranger_new.R")


# Lets create names for output and input folders
# Input folder is the dir path to where yor data is stored
# output folder is the dir path to where you want your processed data to be stored
# Replace these before running 
outputs_folder<-"/Forests/ROutputs/"
input_folder  <-"/Forests/ROutputs/Predictors/"

# Import names of Spectral libraries and thier predictors
# For now we'll work with headwall
names_SpecLibPreds = list.files(input_folder, pattern="all_preds_omitClass_cal",full.names = T)

Final_model_omit<-Spectral_classifier_ranger(names_SpecLibPreds, out_file = "/Forests/ROutputs/rangerOutput")




### OLD RF model script 

# Reads in spectral library and their predictors for each sensor

#SpecLibs_Preds<-lapply(names_SpecLibPreds,Spectral_classifier_ranger) %>% 
#  
#  # Removes dir path from the name
#  setNames(gsub(input_folder,"",names_SpecLibPreds))
#
## Unlist models to the environment
#list2env(SpecLibs_Preds ,.GlobalEnv)
#
#all_confusionMatrix_ranger<-all_preds.csv$confusion%>%as.data.frame()

#save(all_preds.csv,file = "/Forests/ROutputs/Predictors/all_ash_model_preds_ranger.rda")
#write.csv(all_confusionMatrix,file = "/Forests/ROutputs/Predictors/all_confusionMatrix_ranger.csv",row.names = FALSE)
