# Arrange extracted tiff samples for masking into a dataframe to derive predictors
# Categories: Ash, non-ash
# Previous step is model to mask out unwanted categories (shadows, water, shrub, etc.) 
# Next step image_classifier/HyperspecGenFunctionRanger
library(tidyverse)
library(sp)
library(rgdal)
library(raster)
library(readxl)


##Read in headwall wavelength
Headwall_wv<-scan("/Forests/OriginalData/Headwall_wv",numeric())


#Atkinson Ash
setwd("/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/Atkinson/") # Set path for Atkinson samples
mypath_atkinA = "/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/Atkinson/"
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
atkinA_dF <- do.call("rbind", atkinA_raster_list)
atkinA_dF <- add_column(atkinA_dF, "Condition_" = 1, .after=2)
row_names <- rownames(atkinA_dF)
atkinA_dF <- add_column(atkinA_dF, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()


#Hooksett Ash
setwd("/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/Hooksett/") # Set path for Atkinson samples
mypath_hookA = "/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/Hooksett/"
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
hookA_raster_list <- lapply(hookA_raster_list, function(hookA_dF) {
  names(hookA_dF)[3:328] <- Headwall_wv
  hookA_dF
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
hookA_dF <- do.call("rbind", hookA_raster_list)
hookA_dF <- add_column(hookA_dF, "Condition_" = 1, .after=2)
row_names <- rownames(hookA_dF)
hookA_dF <- add_column(hookA_dF, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()


#NHTI Ash
setwd("/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/NHTI/") # Set path for Atkinson samples
mypath_nhtiA = "/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/NHTI/"
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
nhtiA_raster_list <- lapply(nhtiA_raster_list, function(nhtiA_dF) {
  names(nhtiA_dF)[3:328] <- Headwall_wv
  nhtiA_dF
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
nhtiA_dF <- do.call("rbind", nhtiA_raster_list)
nhtiA_dF <- add_column(nhtiA_dF, "Condition_" = 1, .after=2)
row_names <- rownames(nhtiA_dF)
nhtiA_dF <- add_column(nhtiA_dF, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()


all_ash_dF <- rbind(atkinA_dF, hookA_dF, nhtiA_dF)

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
ash_tst1<-lapply(all_ash_dF[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
ash_tst1$V1%>%range()##There are weird values   
ash_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
all_ash_dF<-all_ash_dF%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))



# NON-ASH

#Atkinson Non-Ash
setwd("/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/Atkinson/Non-Ash/") # Set path for Atkinson samples
mypath_atkinNA = "/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/Atkinson/Non-Ash/"
# Import names of Atkinson sample tif files into character list
atkinNA_samples = list.files(mypath_atkinNA, pattern="\\.tif$")

atkinNA_raster_list<-lapply(atkinNA_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",atkinNA_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#tree_raster_list <- Map(cbind, tree_raster_list, Tree_numbe = names(tree_raster_list))

#Renames columns to headwall bandpasses
atkinNA_raster_list <- lapply(atkinNA_raster_list, function(atkinNA_dF) {
  names(atkinNA_dF)[3:328] <- Headwall_wv
  atkinNA_dF
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
atkinNA_dF <- do.call("rbind", atkinNA_raster_list)
atkinNA_dF <- add_column(atkinNA_dF, "Condition_" = 2, .after=2)
row_names <- rownames(atkinNA_dF)
atkinNA_dF <- add_column(atkinNA_dF, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()


#Hooksett Non-Ash (NO NON-ASH SAMPLES FROM HOOKSETT)
setwd("/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/Hooksett/Non-Ash/") # Set path for Atkinson samples
mypath_hookNA = "/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/Hooksett/Non-Ash/"
# Import names of Atkinson sample tif files into character list
hookNA_samples = list.files(mypath_hookNA, pattern="\\.tif$")

hookNA_raster_list<-lapply(hookNA_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",hookNA_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#tree_raster_list <- Map(cbind, tree_raster_list, Tree_numbe = names(tree_raster_list))

#Renames columns to headwall bandpasses
hookNA_raster_list <- lapply(hookNA_raster_list, function(hookNA_dF) {
  names(hookNA_dF)[3:328] <- Headwall_wv
  hookNA_dF
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
hookNA_dF <- do.call("rbind", hookNA_raster_list)
hookNA_dF <- add_column(hookNA_dF, "Condition_" = 2, .after=2)
row_names <- rownames(hookNA_dF)
hookNA_dF <- add_column(hookNA_dF, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()

#NHTI Non-Ash
setwd("/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/NHTI/Non-Ash/") # Set path for Atkinson samples
mypath_nhtiNA = "/Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/NHTI/Non-Ash/"
# Import names of Atkinson sample tif files into character list
nhtiNA_samples = list.files(mypath_nhtiNA, pattern="\\.tif$")

nhtiNA_raster_list<-lapply(nhtiNA_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",nhtiNA_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#tree_raster_list <- Map(cbind, tree_raster_list, Tree_numbe = names(tree_raster_list))

#Renames columns to headwall bandpasses
nhtiNA_raster_list <- lapply(nhtiNA_raster_list, function(nhtiNA_dF) {
  names(nhtiNA_dF)[3:328] <- Headwall_wv
  nhtiNA_dF
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
nhtiNA_dF <- do.call("rbind", nhtiNA_raster_list)
nhtiNA_dF <- add_column(nhtiNA_dF, "Condition_" = 2, .after=2)
row_names <- rownames(nhtiNA_dF)
nhtiNA_dF <- add_column(nhtiNA_dF, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()

other_dF <- rbind(atkinNA_dF, hookNA_dF, nhtiNA_dF)

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
tree_tst1<-lapply(other_dF[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
tree_tst1$V1%>%range()##There are weird values   
tree_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
other_dF<-other_dF%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))



############

#Combined dataframe of ash and non-ash (ash=1, non_ash=2)
all_tree_dF <- rbind(all_ash_dF, other_dF)


#Dataframe of "5" classifcations converted to "4"
all_dF_combineClass <- rbind(atkin_Df, hook_Df, nhti_Df, shadow_Df, shrub_Df, water_Df, grass_Df, rock_Df, dirt_Df) 
all_dF_combineClass$Condition_[all_dF_combineClass$Condition_ == 5] <- 4


#Dataframe of "5" classifications omitted 
all_dF_omitClass <- all_dF[!(all_dF$Condition_ == 5), ]


write.csv(all_tree_dF,"E://Forests//ROutputs//tree_mask_Df.csv", row.names = FALSE)

write.csv(all_dF_combineClass,"E://Forests//ROutputs//all_ash_shad_Df_combineClass.csv", row.names = FALSE)


#length(all_dF_combineClass$Condition_[all_dF_combineClass$Condition_==5])

#Df%>%filter(Tree_numbe=="201D")%>%View()