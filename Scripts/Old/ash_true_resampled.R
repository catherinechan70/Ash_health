####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
# Ash Resampled Bands

library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in image as dataframe 
all_ash_samps<-read.csv("ROutputs//all_ash_Df", check.names = FALSE) 


##Reads in bandpasses for imagery to be used later
HDW_wv<-scan("OriginalData//Headwall_wv", numeric())

##lets remove all those bads that had noise
#all_ash_samps[275:328]<-NULL

##change colnames to correct band names
#colnames(all_ash_samps)[-1:-2]<-HDW_wv


##Now lets check the range of the values in the image
#test<-lapply(all_ash_samps[,-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
#test%>%View()
#test%>%lapply(range) ### All values fall between 0 and 1.2 and there are no NA values

#all_ash_samps<-na.omit(all_ash_samps)

##create a datframe with the coordinates for imagery to be used later
cords<-all_ash_samps%>%dplyr::select(1:4)


##Lets remove this row
#alaskaSpeclib_HDW_50nm<-alaskaSpeclib_HDW_50nm%>%subset(`529.444`>0) ##dim()  1974  333
##you could run logical test above just to check the dataset before moving on
##Do the same steps above for imagery
all_ash_samps_005nm<-all_ash_samps%>%dplyr::select(-1:-4)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,5  ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
all_ash_samps_010nm<-all_ash_samps%>%dplyr::select(-1:-4)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,10 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
all_ash_samps_050nm<-all_ash_samps%>%dplyr::select(-1:-4)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,50 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
all_ash_samps_100nm<-all_ash_samps%>%dplyr::select(-1:-4)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,100))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)

####Lets run logical test for all dataframes
#tst2<-lapply(EightMileTest_IMG_HDW_010nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
#tst2$V1%>%range()##There are no weird values, those are values outside of 0 and 2
#tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst2%>%subset(V1<0)%>%view()

##Don't need to run 3 lines below unless there are weird values in dataset above
#EightMileTest_IMG_HDW_010nm[-1:-2]%>%
#  dplyr::select(`397.593`)%>% 
#  subset(`397.593`<0)%>% nrow() ##58842 have negative values

#DF[!rowSums(DF < 0), ]

####Lets run that test on "EightMileTest_IMG_HDW_50nm"
#tst3<-lapply(EightMileTest_IMG_HDW_050nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
#tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
#tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst3%>%subset(V1<0)%>%view()

##Don't need to run 3 lines below unless there are weird values in dataset above
#EightMileTest_IMG_HDW_050nm[-1:-2]%>%
#  dplyr::select(`397.593`)%>% 
#  subset(`397.593`<0)%>% nrow() ##58842 rows have negative values

#DF[!rowSums(DF < 0), ]

####Lets run that test on "EightMileTest_IMG_HDW_100nm"
#tst4<-lapply(EightMileTest_IMG_HDW_100nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
#tst4$V1%>%range()##There are no weird values, those are values outside of 0 and 2
#tst4$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst4%>%subset(V1<0)%>%view()

##Don't need to run 3 lines below unless there are weird values in dataset above
#EightMileTest_IMG_HDW_100nm[-1:-2]%>%
#  dplyr::select(`397.593`)%>% 
#  subset(`397.593`<0)%>% nrow() ##58842 rows have negative values

#DF[!rowSums(DF < 0), ]

###Lets save our new dfs
write.csv(all_ash_samps       ,"ROutputs//all_ash_samps_resamp_df.csv"    ,row.names = FALSE)
write.csv(all_ash_samps_005nm ,"ROutputs//all_ash_samps_005nm.csv" ,row.names = FALSE)
write.csv(all_ash_samps_010nm ,"ROutputs//all_ash_samps_010nm.csv" ,row.names = FALSE)
write.csv(all_ash_samps_050nm ,"ROutputs//all_ash_samps_050nm.csv" ,row.names = FALSE)
write.csv(all_ash_samps_100nm ,"ROutputs//all_ash_samps_100nm.csv" ,row.names = FALSE)
