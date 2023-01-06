# Arrange extracted tiff samples for masking into a dataframe to derive predictors
# Categories: Trees, shadows, water, shrub, grass, rock, dirt
# Previous step is ash_extraction_furthertest1.R 
# Next step image_classifier/HyperspecGenFunctionRanger

library(tidyverse)
library(sp)
library(rgdal)
library(raster)
library(readxl)


##Read in headwall wavelength
Headwall_wv<-scan("/Forests/OriginalData/Headwall_wv",numeric())


#TREES
setwd("/Thesis Data/Ash Trees/ExtractedMaskSamples/SelectedTreeSamps/") # Set path for Atkinson samples
mypath_trees = "/Thesis Data/Ash Trees/ExtractedMaskSamples/SelectedTreeSamps"
# Import names of Atkinson sample tif files into character list
tree_samples = list.files(mypath_trees, pattern="\\.tif$")

tree_raster_list<-lapply(tree_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",tree_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#tree_raster_list <- Map(cbind, tree_raster_list, Tree_numbe = names(tree_raster_list))

#Renames columns to headwall bandpasses
tree_raster_list <- lapply(tree_raster_list, function(tree_df) {
  names(tree_df)[3:328] <- Headwall_wv
  tree_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
tree_Df <- do.call("rbind", tree_raster_list)
tree_Df <- add_column(tree_Df, "Condition_" = 1, .after=2)
row_names <- rownames(tree_Df)
tree_Df <- add_column(tree_Df, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()


##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
tree_tst1<-lapply(tree_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
tree_tst1$V1%>%range()##There are weird values   
tree_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
tree_Df<-tree_Df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))


#SHADOWS
setwd("/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedShadows/") # Set path for Atkinson samples
mypath_shadows = "/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedShadows"
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
shadow_Df <- do.call("rbind", shadow_raster_list)
shadow_Df <- add_column(shadow_Df, "Condition_" = 7, .after=2)
row_names <- rownames(shadow_Df)
shadow_Df <- add_column(shadow_Df, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
shad_tst1<-lapply(shadow_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
shad_tst1$V1%>%range()##There are weird values   
shad_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
shadow_Df<-shadow_Df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))

#GRASS
setwd("/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedGrass/") # Set path for Atkinson samples
mypath_grass = "/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedGrass/"
# Import names of shadow sample tif files into character list
grass_samples = list.files(mypath_grass, pattern="\\.tif$")

grass_raster_list<-lapply(grass_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",grass_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#nhti_raster_list <- Map(cbind, nhti_raster_list, Tree_numbe = names(nhti_raster_list))

#Renames columns to headwall bandpasses
grass_raster_list <- lapply(grass_raster_list, function(grass_df) {
  names(grass_df)[3:328] <- Headwall_wv
  grass_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
grass_Df <- do.call("rbind", grass_raster_list)
grass_Df <- add_column(grass_Df, "Condition_" = 2, .after=2)
row_names <- rownames(grass_Df)
grass_Df <- add_column(grass_Df, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
grass_tst1<-lapply(grass_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
grass_tst1$V1%>%range()##There are weird values   
grass_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
grass_Df<-grass_Df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))

#SHRUB
setwd("/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedShrub/") # Set path for Atkinson samples
mypath_shrub = "/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedShrub"
# Import names of shadow sample tif files into character list
shrub_samples = list.files(mypath_shrub, pattern="\\.tif$")

shrub_raster_list<-lapply(shrub_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",shrub_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#nhti_raster_list <- Map(cbind, nhti_raster_list, Tree_numbe = names(nhti_raster_list))

#Renames columns to headwall bandpasses
shrub_raster_list <- lapply(shrub_raster_list, function(shrub_df) {
  names(shrub_df)[3:328] <- Headwall_wv
  shrub_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
shrub_Df <- do.call("rbind", shrub_raster_list)
shrub_Df <- add_column(shrub_Df, "Condition_" = 3, .after=2)
row_names <- rownames(shrub_Df)
shrub_Df <- add_column(shrub_Df, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
shrub_tst1<-lapply(shrub_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
shrub_tst1$V1%>%range()##There are weird values   
shrub_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
shrub_Df<-shrub_Df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))

#WATER
setwd("/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedWater/") # Set path for Atkinson samples
mypath_water = "/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedWater/"
# Import names of shadow sample tif files into character list
water_samples = list.files(mypath_water, pattern="\\.tif$")

water_raster_list<-lapply(water_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",water_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#nhti_raster_list <- Map(cbind, nhti_raster_list, Tree_numbe = names(nhti_raster_list))

#Renames columns to headwall bandpasses
water_raster_list <- lapply(water_raster_list, function(water_df) {
  names(water_df)[3:328] <- Headwall_wv
  water_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
water_Df <- do.call("rbind", water_raster_list)
water_Df <- add_column(water_Df, "Condition_" = 4, .after=2)
row_names <- rownames(water_Df)
water_Df <- add_column(water_Df, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
water_tst1<-lapply(water_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
water_tst1$V1%>%range()##There are weird values   
water_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
water_Df<-water_Df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))


#ROCK
setwd("/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedRock/") # Set path for Atkinson samples
mypath_rock = "/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedRock/"
# Import names of shadow sample tif files into character list
rock_samples = list.files(mypath_rock, pattern="\\.tif$")

rock_raster_list<-lapply(rock_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",rock_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#nhti_raster_list <- Map(cbind, nhti_raster_list, Tree_numbe = names(nhti_raster_list))

#Renames columns to headwall bandpasses
rock_raster_list <- lapply(rock_raster_list, function(rock_df) {
  names(rock_df)[3:328] <- Headwall_wv
  rock_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
rock_Df <- do.call("rbind", rock_raster_list)
rock_Df <- add_column(rock_Df, "Condition_" = 5, .after=2)
row_names <- rownames(rock_Df)
rock_Df <- add_column(rock_Df, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
rock_tst1<-lapply(rock_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
rock_tst1$V1%>%range()##There are weird values   
rock_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
rock_Df<-rock_Df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))

#DIRT
setwd("/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedDirt/") # Set path for Atkinson samples
mypath_dirt = "/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedDirt/"
# Import names of shadow sample tif files into character list
dirt_samples = list.files(mypath_dirt, pattern="\\.tif$")

dirt_raster_list<-lapply(dirt_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",dirt_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
#nhti_raster_list <- Map(cbind, nhti_raster_list, Tree_numbe = names(nhti_raster_list))

#Renames columns to headwall bandpasses
dirt_raster_list <- lapply(dirt_raster_list, function(dirt_df) {
  names(dirt_df)[3:328] <- Headwall_wv
  dirt_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
dirt_Df <- do.call("rbind", dirt_raster_list)
dirt_Df <- add_column(dirt_Df, "Condition_" = 6, .after=2)
row_names <- rownames(dirt_Df)
dirt_Df <- add_column(dirt_Df, "Tree_numbe" = row_names, .after = 2)%>%
  dplyr::select(-(`901.276`:`999.42`))%>%
  na.omit()

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
dirt_tst1<-lapply(dirt_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
dirt_tst1$V1%>%range()##There are weird values   
dirt_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
dirt_Df<-dirt_Df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))



#Combined dataframe of all classifications 1-5
all_dF <- rbind(tree_Df, shadow_Df, shrub_Df, water_Df, grass_Df, rock_Df, dirt_Df)


#Dataframe of "5" classifcations converted to "4"
all_dF_combineClass <- rbind(atkin_Df, hook_Df, nhti_Df, shadow_Df, shrub_Df, water_Df, grass_Df, rock_Df, dirt_Df) 
all_dF_combineClass$Condition_[all_dF_combineClass$Condition_ == 5] <- 4


#Dataframe of "5" classifications omitted 
all_dF_omitClass <- all_dF[!(all_dF$Condition_ == 5), ]


write.csv(all_dF,"E://Forests//ROutputs//all_mask_Df.csv", row.names = FALSE)

write.csv(all_dF_combineClass,"E://Forests//ROutputs//all_ash_shad_Df_combineClass.csv", row.names = FALSE)


#length(all_dF_combineClass$Condition_[all_dF_combineClass$Condition_==5])

#Df%>%filter(Tree_numbe=="201D")%>%View()

