## Builds a random forest classification model from hyperspectral dataset 

Spectral_classifier_ranger<-function(classifier){
  
  # Creates a string of possible names that will be removed
  remove_names<-c("ScanID","PFT","PFT_2","PFT_4","Area","PFT2_Freq"
                  ,"PFT3_Freq","PFT4_Freq","Tree_numbe","x","y")
  
      #Unit test PASSES
      #Spectral_Library<-read.csv(names_SpecLibPreds) %>% dim() #6506 by 268
  # Reads in spectral library
  Spectral_Library<-read.csv(classifier)
  
      #Unit test PASSES 
      # Spectral_Library[remove_names] = NULL # dim( Spectral_Library) 6506 by 265 ... removes 3 columns
  # Removes unwanted metadata from dataframe 
  # (the only columns we need are the PFT_3 column and all the predictors)
  Spectral_Library[remove_names] = NULL
  
  # Change column name with all the levels to "classes"
  names(Spectral_Library)[1]<-"Classes"
  
  Spectral_Library[, "Classes"] <- as.factor(Spectral_Library[, "Classes"])
  
  # Function selects the 50 most importnat variables
  ImportantVars<-function(x){
    
    # Creates Random forest Model
    set.seed(2017)
    #rf_mod<-randomForest(Classes ~ .,data = x,
    #                   mtry = sqrt(ncol(x)),
    #                   ntree = 1001,
    #                   localImp = TRUE)
    ##Unit test passes
    rf_mod_rang<-ranger(Classes ~ .,data = x,
                        #mtry = sqrt(ncol(x)),
                        num.trees = 300,
                        importance = "impurity_corrected",
                        local.importance = TRUE)
    
    #rf_mod_rang<-enframe(rf_mod_rang$variable.importance, name="predictor", value="importance") %>%
    #  arrange(desc(rf_mod_rang)) %>%
    #  as.data.frame()
    
    # Selects the 50 most important variables
    # This is achieved by calculating distribution of minimal depth 
    ImportantVarsFrame<-enframe(rf_mod_rang$variable.importance, name="predictor", value="importance") %>% #works to here, produces table 262 by 2 of variable importance 
      arrange(desc(importance))  %>%
      as.data.frame() %>% 
      #filter(ImportantVarsFrame, top_n(50)) %>%
      select(predictor) 

    #ImportantVars<-plot_min_depth_distribution(
    #min_depth_distribution(rf_mod_rang),
    #min_no_of_trees = 200,
    #mean_sample = "relevant_trees",
    #k = 50)
     return(ImportantVars)
  } # ImportantVars function ends
  
  Randomforest_mod<-function(x){
    # creates a dataframe with the Important varibles
    mostImportantVars<-ImportantVars(Spectral_Library)
    
    # Grabs the 50 most important variables from predictors dataframe
    ImpVars<-unique(mostImportantVars$data$variable)%>%as.character()
    
    # Creates a new model built on important variables
    newdf<-Spectral_Library%>%
      dplyr::select(Classes,ImpVars)%>% View()
    
    
    rfNew<-ranger(Classes ~ .,data = newdf,
                  #mtry = sqrt(ncol(newdf)),
                  num.trees =500,
                  importance = "impurity_corrected",
                  local.importance = TRUE) 
    
    #randomForest(Classes ~ .,data = newdf,
    #                  mtry = sqrt(ncol(newdf)),
    #                  ntree = 1001,
    #                  localImp = TRUE)
    
    
    # You could write out the confusion matrix
    # You could save the model to desired folder
    return(rfNew)
  } # Random Forest Function ends
  
  Randomforest_mod(classifier)
  
}# Function Spectral ends

outputs_folder<-"/Forests/ROutputs/"
input_folder  <-"/Forests/ROutputs/Predictors/"


names_SpecLibPreds = list.files(input_folder, pattern="all_preds_cal",full.names = T)

# Reads in spectral library and their predictors for each sensor

SpecLibs_Preds<-lapply(names_SpecLibPreds,Spectral_classifier_ranger) %>% 
  
  # Removes dir path from the name
  setNames(gsub(input_folder,"",names_SpecLibPreds))

# Unlist models to the environment
list2env(SpecLibs_Preds ,.GlobalEnv)

all_confusionMatrix_ranger<-all_preds.csv$confusion%>%as.data.frame()
