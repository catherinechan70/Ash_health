# Builds a random forest classification model from hyperspectral dataset 
# Function takes two arguments, the spectral library object and 
# output directory in which you want your files to be stored


Spectral_classifier_ranger<-function(x, out_file){
  
  # Creates a string of possible names that will be removed
  remove_names<-c("ScanID","Class1","Class2","Class4","Area","Class2_Freq"
                  ,"Class3_Freq","Class4_Freq","Tree_numbe","x","y")
  
  # Unit test PASSES
  # Spectral_Library<-read.csv(names_SpecLibPreds) %>% dim() #6506 by 268
  # Reads in spectral library
  Spectral_Library<-read.csv(names_SpecLibPreds)
  
  # Unit test PASSES 
  # Spectral_Library[remove_names] = NULL # dim( Spectral_Library) 6506 by 265 ... removes 3 columns
  # Removes unwanted metadata from dataframe 
  Spectral_Library[remove_names] = NULL
  
  # Change column name with all the levels to "classes"
  names(Spectral_Library)[1]<-"Classes"
  
  ##Try rerunning with the "Classes" being numeric or being an ordinal factor
  Spectral_Library[, "Classes"] <- as.factor(Spectral_Library[, "Classes"])
  
  set.seed(123)
  # Unit test passes
  rf_mod_rang<-ranger(Classes ~ .,data = Spectral_Library,
                      num.trees = 10000,
                      importance = "impurity_corrected",
                      local.importance = TRUE)
  
  # Selects the 50 most important variables
  ImportantVarsFrame<-enframe(rf_mod_rang$variable.importance, 
                              name="predictor", value="importance")
  #ModStat<-enframe(rf_mod_rang$prediction.error, 
  #                           name="predictor", value="error")
  #Modstat$
  # Selects the 50 most important variables
  Imp_Vars50<-ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:50,]
  
  # Creates a plot of the 25 most important varibles
  #pdf(paste(out_file,"ImpVars",".pdf",sep =""))#, units="px",height = 1400, width=2400, res=350)
  
  # Creates plot
  # ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:25,]%>%
  ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:25,]%>%
    ggplot()+
    # geom_col(aes(x  = predictor, y = importance))+
    geom_col(aes(x  = reorder(predictor, +importance), y = importance))+
    coord_flip()+
    xlab("predictor")+
    theme_bw()
  
  # Saves the plot 
  ggsave(paste(out_file,"ImpVars_omitClass.png", sep=""))
  dev.off
  
  # Grabs the names of the 50 most important variables from predictors dataframe
  ImpVars_names<-unique(Imp_Vars50$predictor)%>%as.character()
  
  # Creates a new model built on important variables
  New_Speclib<-Spectral_Library%>%
    dplyr::select(Classes,ImpVars_names)
  
  rfNew<-ranger(Classes ~ .,data = New_Speclib,
                                  num.trees =10000,
                                  importance = "impurity_corrected",
                                  local.importance = TRUE)
                   
  
  # Lets save the confusion matrix from the model above
  Confusion_matrix<-rfNew$confusion.matrix%>%
    as.data.frame()
  
  # Writes out confusion matrix to output folder
  write.csv(Confusion_matrix,paste0(out_file,"confusion_matrix_omitClass.csv"),row.names = F)
  
  return(rfNew)
  
} # Random Forest Function ends




## Heres an example of how to use this function (also in RF_model.R)
#outputs_folder<-"/Forests/ROutputs/"
#input_folder  <-"/Forests/ROutputs/Predictors/"
#
## Import names of Spectral libraries and thier predictors
## For now we'll work with headwall
#names_SpecLibPreds = list.files(input_folder, pattern="all_preds_cal",full.names = T)
#
#Final_model<-Spectral_classifier_ranger(names_SpecLibPreds, out_file = "/Forests/ROutputs/rangerOutput")
#
##Final_model<-Spectral_classifier_ranger("Output/SpecLib_Derivs.csv", out_file = "Output/")


