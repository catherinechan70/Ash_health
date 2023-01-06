# Arrange extracted tiff samples for masking into a dataframe to derive predictors (applied only to ash buffers)
# Categories: Ash trees and shadows
# Previous step is ash_extraction_furthertest1.R
# Next step image_classifier/HyperspecGenFunctionRanger 

library(tidyverse)
library(sp)
library(rgdal)
library(raster)
library(readxl)

setwd("E:/") 
#Set working drive
sheet_data <- read_excel(path = "Forests/OriginalData/EAB_NH_Chan_OnlyAsh.xlsx")
sheet_data$Tree_numbe<-gsub(".tif","",sheet_data$Tree_numbe)

##Read in headwall wavelength
Headwall_wv<-scan("/Forests/OriginalData/Headwall_wv",numeric())

#Ash Trees

#Atkinson Ash
setwd("/Forests/OriginalData/Atkinson/") # Set path for Atkinson samples
mypath_atkinA = "/Forests/OriginalData/Atkinson/"
# Import names of Atkinson sample tif files into character list
atkinA_samples = list.files(mypath_atkinA, pattern="\\.tif$")

atkinA_raster_list<-lapply(atkinA_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",atkinA_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#tree_raster_list <- Map(cbind, tree_raster_list, Tree_numbe = names(tree_raster_list))

#Renames columns to headwall bandpasses
atkinA_raster_list <- lapply(atkinA_raster_list, function(atkinA_df) {
  names(atkinA_df)[3:328] <- Headwall_wv
  atkinA_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
atkinA_df <- do.call("rbind", atkinA_raster_list)
atkinA_df <- add_column(atkinA_df, "Condition_" = 1, .after=2)
row_names <- rownames(atkinA_df)
atkinA_df <- add_column(atkinA_df, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
atkin_tst1<-lapply(atkinA_df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
atkin_tst1$V1%>%range()##There are weird values   
atkin_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
atkinA_df<-atkinA_df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))


#Hooksett Ash
setwd("/Forests/OriginalData/Hooksett/") # Set path for Atkinson samples
mypath_hookA = "/Forests/OriginalData/Hooksett/"
# Import names of Atkinson sample tif files into character list
hookA_samples = list.files(mypath_hookA, pattern="\\.tif$")

hookA_raster_list<-lapply(hookA_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",hookA_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#tree_raster_list <- Map(cbind, tree_raster_list, Tree_numbe = names(tree_raster_list))

#Renames columns to headwall bandpasses
hookA_raster_list <- lapply(hookA_raster_list, function(hookA_df) {
  names(hookA_df)[3:328] <- Headwall_wv
  hookA_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
hookA_df <- do.call("rbind", hookA_raster_list)
hookA_df <- add_column(hookA_df, "Condition_" = 1, .after=2)
row_names <- rownames(hookA_df)
hookA_df <- add_column(hookA_df, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()


##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
hook_tst1<-lapply(hookA_df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
hook_tst1$V1%>%range()##There are weird values   
hook_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
hookA_df<-hookA_df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))

#NHTI Ash
setwd("/Forests/OriginalData/NHTI/") # Set path for Atkinson samples
mypath_nhtiA = "/Forests/OriginalData/NHTI/"
# Import names of Atkinson sample tif files into character list
nhtiA_samples = list.files(mypath_nhtiA, pattern="\\.tif$")

nhtiA_raster_list<-lapply(nhtiA_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",nhtiA_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#tree_raster_list <- Map(cbind, tree_raster_list, Tree_numbe = names(tree_raster_list))

#Renames columns to headwall bandpasses
nhtiA_raster_list <- lapply(nhtiA_raster_list, function(nhtiA_df) {
  names(nhtiA_df)[3:328] <- Headwall_wv
  nhtiA_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
nhtiA_df <- do.call("rbind", nhtiA_raster_list)
nhtiA_df <- add_column(nhtiA_df, "Condition_" = 1, .after=2)
row_names <- rownames(nhtiA_df)
nhtiA_df <- add_column(nhtiA_df, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()


##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
nhti_tst1<-lapply(nhtiA_df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
nhti_tst1$V1%>%range()##There are weird values   
nhti_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
nhtiA_df<-nhtiA_df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))


all_ash_df <- rbind(atkinA_df, hookA_df, nhtiA_df)




#SHADOWS
setwd("/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedShadows/") # Set path for Atkinson samples
mypath_shadows = "/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedShadows/"
# Import names of shadow sample tif files into character list
shadow_samples = list.files(mypath_shadows, pattern="\\.tif$")

shadow_raster_list<-lapply(shadow_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",shadow_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#nhti_raster_list <- Map(cbind, nhti_raster_list, Tree_numbe = names(nhti_raster_list))

#Renames columns to headwall bandpasses
shadow_raster_list <- lapply(shadow_raster_list, function(shadow_df) {
  names(shadow_df)[3:328] <- Headwall_wv
  shadow_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
shadow_df <- do.call("rbind", shadow_raster_list)
shadow_df <- add_column(shadow_df, "Condition_" = 2, .after=2)
row_names <- rownames(shadow_df)
shadow_df <- add_column(shadow_df, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
shad_tst1<-lapply(shadow_df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
shad_tst1$V1%>%range()##There are weird values   
shad_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
shadow_df<-shadow_df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))



#Combined dataframe of all classifications 1-5
all_df <- rbind(all_ash_df, shadow_df)


write.csv(all_df,"/Forests/ROutputs/ash_shadow_df.csv", row.names = FALSE)

