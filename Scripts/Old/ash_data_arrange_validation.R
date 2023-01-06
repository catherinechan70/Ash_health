### ash_data_Kevaughan
### Arranging extracted tiff samples into a dataframe in order to derive predictors
### For calibration or validation data samples
### Next step after ash_extraction_furthertest1.R script

library(tidyverse)
library(sp)
library(rgdal)
library(raster)
library(readxl)


#Set working drive
sheet_data <- read_excel(path = "/Forests/OriginalData/EAB_NH_Chan_OnlyAsh.xlsx")
sheet_data$Tree_numbe<-gsub(".tif","",sheet_data$Tree_numbe)

##Read in headwall wavelength
Headwall_wv<-scan("/Forests/OriginalData/Headwall_wv",numeric())

## ATKINSON SAMPLES
#setwd("/Forests/OriginalData/Atkinson_validation/") # Set path for Atkinson samples
#mypath_atkin = "/Forests/OriginalData/Atkinson_validation"
## Import names of Atkinson sample tif files into character list
#atkin_samples = list.files(mypath_atkin, pattern="\\.tif$") 
#
##HOOKSETT SAMPLES
#setwd("/Forests/OriginalData/Hooksett_validation/") # Set path for Atkinson samples
#mypath_hook = "/Forests/OriginalData/Hooksett_validation"
## Import names of Atkinson sample tif files into character list
#hook_samples = list.files(mypath_hook, pattern="\\.tif$")
#
##NHTI SAMPLES
#setwd("/Forests/OriginalData/NHTI_validation/") # Set path for Atkinson samples
#mypath_nhti = "/Forests/OriginalData/NHTI_validation"
## Import names of Atkinson sample tif files into character list
#nhti_samples = list.files(mypath_nhti, pattern="\\.tif$")

##Unit test
#gsub(".tif","",atkin_samples[1:2]) # Substitutes file name and removes .tif from the name
#A<-brick(paste(mypath_atkin,atkin_samples[1],sep="/"))%>% # Reads in the hypersectral raster as a brick object
#  rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
#  as.data.frame()%>% # Convert raster to dataframe
#  mutate(Tree_numbe = gsub(".tif","",atkin_samples[1])) #Add column Tree_numbe to dataframe
#B<-inner_join(A ,sheet_data[ ,c("Tree_numbe", "Condition_" )],by="Tree_numbe")%>% ## adds the Condtion classes
#  dplyr::select(x,y,Tree_numbe,Condition_,everything())     ## Rearrages dataframe
#colnames(B)[-1:-4]<-Headwall_wv ##Renames columns to headwall bandpasses




##All functions above worked now you want to integrate those into a loop or mapped function
##For now we'll apply each function above using the lines of code below

#ATKINSON
setwd("/Forests/OriginalData/Atkinson_validation/") # Set path for Atkinson samples
mypath_atkin = "/Forests/OriginalData/Atkinson_validation"
# Import names of Atkinson sample tif files into character list
atkin_samples = list.files(mypath_atkin, pattern="\\.tif$")

atkin_raster_list<-lapply(atkin_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",atkin_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
atkin_raster_list <- Map(cbind, atkin_raster_list, Tree_numbe = names(atkin_raster_list))

#Renames columns to headwall bandpasses
atkin_raster_list <- lapply(atkin_raster_list, function(atkin_df) {
  names(atkin_df)[3:328] <- Headwall_wv
  atkin_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
atkin_Df <- do.call("rbind", atkin_raster_list)%>%
  inner_join(sheet_data[ ,c("Tree_numbe", "Condition_" )],by="Tree_numbe")%>%
  dplyr::select(x,y,Tree_numbe,Condition_,everything(),-(`901.276`:`999.42`))%>%
  na.omit()

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
atkin_tst1<-lapply(atkin_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
atkin_tst1$V1%>%range()##There are weird values   
atkin_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
atkin_Df<-atkin_Df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))


#HOOKSETT
setwd("/Forests/OriginalData/Hooksett_validation/") # Set path for Atkinson samples
mypath_hook = "/Forests/OriginalData/Hooksett_validation"
# Import names of Atkinson sample tif files into character list
hook_samples = list.files(mypath_hook, pattern="\\.tif$")

hook_raster_list<-lapply(hook_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",hook_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
hook_raster_list <- Map(cbind, hook_raster_list, Tree_numbe = names(hook_raster_list))

#Renames columns to headwall bandpasses
hook_raster_list <- lapply(hook_raster_list, function(df) {
  names(df)[3:328] <- Headwall_wv
  df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
hook_Df <- do.call("rbind", hook_raster_list)%>%
  inner_join(sheet_data[ ,c("Tree_numbe", "Condition_" )],by="Tree_numbe")%>%
  dplyr::select(x,y,Tree_numbe,Condition_,everything(),-(`901.276`:`999.42`))%>%
  na.omit()

##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
hook_tst1<-lapply(hook_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
hook_tst1$V1%>%range()##There are weird values   
hook_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
hook_Df<-hook_Df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))


#NHTI
setwd("/Forests/OriginalData/NHTI_validation/") # Set path for Atkinson samples
mypath_nhti = "/Forests/OriginalData/NHTI_validation"
# Import names of Atkinson sample tif files into character list
nhti_samples = list.files(mypath_nhti, pattern="\\.tif$")

nhti_raster_list<-lapply(nhti_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",nhti_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
nhti_raster_list <- Map(cbind, nhti_raster_list, Tree_numbe = names(nhti_raster_list))

#Renames columns to headwall bandpasses
nhti_raster_list <- lapply(nhti_raster_list, function(nhti_df) {
  names(nhti_df)[3:328] <- Headwall_wv
  nhti_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
nhti_Df <- do.call("rbind", nhti_raster_list)%>%
  inner_join(sheet_data[ ,c("Tree_numbe", "Condition_" )],by="Tree_numbe")%>%
  dplyr::select(x,y,Tree_numbe,Condition_,everything(),-(`901.276`:`999.42`))%>%
  na.omit()


##Now that we have our dataframe, lets check for wired values 
###Lets run logical test for the dataframe created to see if there are any wired values (those are values outside of 0 and 2)
##Lets convert those weired values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
nhti_tst1<-lapply(nhti_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
nhti_tst1$V1%>%range()##There are weird values   
nhti_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
nhti_Df<-nhti_Df%>%
  filter_at(vars(-x,-y,-Tree_numbe,-Condition_),all_vars(. >=0))


#Combined dataframe of all classifications 1-5
all_dF_v <- rbind(atkin_Df, hook_Df, nhti_Df)


#Dataframe of "5" classifcations converted to "4"
all_dF_combineClass_v <- rbind(atkin_Df, hook_Df, nhti_Df) 
all_dF_combineClass_v$Condition_[all_dF_combineClass_v$Condition_ == 5] <- 4


#Dataframe of "5" classifications omitted 
#all_dF_omitClass_v <- all_dF_v[!(all_dF_v$Condition_ == 5), ]


#write.csv(all_dF_v,"E://Forests//ROutputs//all_ash_Df_validation.csv", row.names = FALSE)

write.csv(all_dF_combineClass_v,"E://Forests//ROutputs//all_ash_Df_validation.csv", row.names = FALSE)

#write.csv(all_dF_omitClass_v,"E://Forests//ROutputs//all_ash_Df_omitClass_validation.csv", row.names = FALSE)

#length(all_dF_combineClass_v$Condition_[all_dF_combineClass_v$Condition_==5])

#Df%>%filter(Tree_numbe=="201D")%>%View()