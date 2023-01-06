#######################Combines all predictiors into one dataframe######################################
#Ash All Predictors

library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for Imagery
all_ash_samps_005nm<-read.csv("ROutputs/all_ash_samps_005nm.csv")
all_ash_samps_010nm<-read.csv("ROutputs/all_ash_samps_010nm.csv")
all_ash_samps_050nm<-read.csv("ROutputs/all_ash_samps_050nm.csv")
all_ash_samps_100nm<-read.csv("ROutputs/all_ash_samps_100nm.csv")
all_ash_samps_VIs_A    <-read.csv("ROutputs/all_ash_samps_VIs.csv"      )

##Make names for colnames in each df unique
colnames(all_ash_samps_005nm)[-1:-2]<-paste0(colnames(all_ash_samps_005nm)[-1:-2],"_005nm")
colnames(all_ash_samps_010nm)[-1:-2]<-paste0(colnames(all_ash_samps_010nm)[-1:-2],"_010nm")
colnames(all_ash_samps_050nm)[-1:-2]<-paste0(colnames(all_ash_samps_050nm)[-1:-2],"_050nm")
colnames(all_ash_samps_100nm)[-1:-2]<-paste0(colnames(all_ash_samps_100nm)[-1:-2],"_100nm")
colnames(all_ash_samps_VIs_A    )[-1:-2]<-paste0(colnames(all_ash_samps_VIs_A    )[-1:-2],"_VIs"  )

##Let's merge these dataframes
ash_samps_preds <- Reduce(cbind,list(all_ash_samps_005nm
                                          ,all_ash_samps_010nm[-1:-2]
                                          ,all_ash_samps_050nm[-1:-2]
                                          ,all_ash_samps_100nm[-1:-2]
                                          ,all_ash_samps_VIs_A    [-1:-2]))
###Lets save dataframe
write.csv(ash_samps_preds    ,"ROutputs/ash_samps_preds.csv",row.names = FALSE)
