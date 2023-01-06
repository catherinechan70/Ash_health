library(tidyverse)
library(sp)
library(rgdal)
library(raster)
library(readxl)
library(xlsx)

setwd("E:/") 
#Set working drive
sheet_data <- read_excel(path = "Forests/OriginalData/EAB_NH_Chan_OnlyAsh.xlsx")
sheet_data$Tree_numbe<-gsub(".tif","",sheet_data$Tree_numbe)

##All functions above worked now you want to integrate those into a loop or mapped function
##For now we'll apply each function above using the lines of code below

# Call in all classified buffer samples
setwd("/Forests/FinalOutputs/") # Set path for Atkinson samples
mypath_buffers = "/Forests/FinalOutputs/"
# Import names of Atkinson sample tif files into character list
classed_samples = list.files(mypath_buffers, pattern="\\.tif$")

buffer_raster_list<-lapply(classed_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",classed_samples))  # Substitutes file name and removes .tif from the name


#Add column Tree_numbe to dataframe
buffer_raster_list <- Map(cbind, buffer_raster_list, Tree_numbe = names(buffer_raster_list))

# Adds condition class
buffer_raster_list2<-lapply(1:length(buffer_raster_list), function(x){
  
  matchingRow<-sheet_data[names(buffer_raster_list[x])==sheet_data$Tree_numbe3,]
  
  buffer_raster_list[[x]]%>%mutate(Condition_ = matchingRow$Condition_ )
  
})%>% setNames(names(buffer_raster_list))


# Creates pixel counts and accuracy column
pixel_count <- lapply(1:length(buffer_raster_list2), function(x){
  matchingcount <- sum(--(buffer_raster_list2[[x]][3]) == buffer_raster_list2[[x]][5])
  totalcount <- nrow(buffer_raster_list2[[x]])
  accuracy <- matchingcount/totalcount 
                                                                     
})

accuracy_average <- mean(pixel_count[[3]])

pixel_count2 <- transpose(as.data.frame(pixel_count))

write.csv(pixel_count2, "Forests/FinalOutputs/pixel_count.csv")
write.csv(list, "Forests/FinalOutputs/buffer_names.csv")


#buffer_raster_list3 <- as.data.frame(buffer_raster_list2)

list <- as.data.frame(names(buffer_raster_list2))# %>%
  #cbind(pixel_count2)

test_merge <- merge(list, pixel_count2, by.x =list[1], by.y=)

list <-  c(buffer_raster_list2, pixel_count)









#TESTING

write.csv(buffer_raster_list2[12], file="Forests/testelement109_GA.csv")


test_samp <- buffer_raster_list$"109_GA"
test_samp <- inner_join(sheet_data$Condition_)
