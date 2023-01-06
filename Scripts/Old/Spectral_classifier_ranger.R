# ------------------------------------ vARIABLE IMPORTANCE sCRIPT --------------------------
library(caret)
library(ranger)
library(randomForest)
library(tidyverse)
library(hsdar)

#------------------Building Model without identifying important variables --------------
# Spectral Library

setwd("/Forests/ROutputs/") 

#SpecLib<-read.csv("D_002_SpecLib_Derivs.csv")
SpecLib <- read.csv("landcover_allcat_derivsD_002_SpecLib_Derivs.csv")
#SpecLib[is.na(LibSpec)] <- 0

# Remove Unwanted columns
# Creates a string of possible names that will be removed
remove_names<-c("ScanID","Class1","Class2","Class4","Area","Class2_Freq"
                
                ,"Class3_Freq","Class4_Freq","Tree_numbe","x","y")
# Remove Unwanted columns
SpecLib[remove_names] = NULL 
#SpecLib<-SpecLib%>%filter_all(all_vars(.== NA))

# SpecLib[rowSums(is.na(SpecLib)) == 0, ]

# Change column name with all the levels to "classes"
names(SpecLib)[1]<-"Classes"

#set.seed(123)
## Build Model
#rf_mod_rang1<-ranger(Classes ~ .,data = SpecLib,
#                    num.trees = 10000,
#                    importance = "impurity_corrected",
#                    local.importance = TRUE) # OOB prediction error (MSE):  0.2510 % , R2: 0.7946

# ------------------------------- Remove correlated variables ---------------------------
# Creates a sequence of numbers that, represents the number of variables to choose to build models
NoofVars1<-seq(0.99)
#seq(0.900,0.99,by = 0.03)
#seq(0.99)

# List of models
Modslist_cor<-lapply(1:length(NoofVars1),function(x){
  
  # Creates corelation matrix
  CorelationMatrix<-cor(SpecLib[-1]) 
  CorelationMatrix[is.na(CorelationMatrix)] <- 0
  
  
  # Select most correlated variables
  caret_findCorr<-findCorrelation(CorelationMatrix, cutoff = NoofVars1[x], names = T)
  
  # Remove corelated vars
  predictor_df_reduced<-SpecLib %>%
    dplyr::select(-caret_findCorr)
  
  # Rebuild Model from removal of intercorrelated variables
  rf_mod_rang2<-ranger(Classes ~ .,data = predictor_df_reduced,
                       num.trees = 10000,
                       importance = "impurity_corrected",
                       local.importance = TRUE) 
  return(rf_mod_rang2)
  
})%>%
  setNames(paste(NoofVars1,"Cor_Variables",sep="_"))

# We can print the prediction error to find which cutoff value produces best model
listofmoderors_COR<-lapply(Modslist_cor,function(x)
  return(x$prediction.error))

#$`0.9_Cor_Variables`
#[1] 0.3106023
#
#$`0.93_Cor_Variables`
#[1] 0.3004607
#
#$`0.96_Cor_Variables`
#[1] 0.2684431
#
#$`0.99_Cor_Variables`
#[1] 0.2499692

# Creates a dataframe with errors for each model
error_COR_df<-do.call("rbind",listofmoderors_COR)%>%
  as.data.frame()

# Adds another column
error_COR_df$Vars<-rownames(error_COR_df)

# Changes column names
names(error_COR_df)<-c("error","Vars")

# Lets R respect the order in data.frame.
error_COR_df$Vars <- factor(error_COR_df$Var,
                            levels = error_COR_df$Var
                            [order(error_COR_df$error)])

# Creates a plot of the errors at the different cutoff levels
error_COR_df%>%
  ggplot(aes(x  = Vars, y = error))+
  theme_bw()+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Model errors after pair-wise correlations are removed")

ggsave("Model_errors_landcover_allcat.jpg")

#------------------------------ Select Important variables -----------------------------------
# Creates a dataframe with all variables and their importance
ImportantVarsFrame<-enframe(Modslist_cor[[1]]$variable.importance, 
                            name="predictor", value="importance")

# Function Creates a plot of the 35 most important vars
ImportantVarsFrame35<-ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:35,]

# Lets R respect the order in data.frame.
ImportantVarsFrame35$predictor <- factor(ImportantVarsFrame35$predictor,
                                         levels = ImportantVarsFrame35$predictor
                                         [order(ImportantVarsFrame35$importance)])

# Creates a plot of the 35 most important variables
ImportantVarsFrame35%>%
  ggplot(aes(x  = predictor, y = importance))+
  theme_bw()+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("35 Most Important Varibles")

ggsave("Important_Variables_landcover_allcat.jpg")


# Creates corelation matrix
CorrelationMatrix<-cor(SpecLib[-1])
CorrelationMatrix[is.na(CorrelationMatrix)] <- 0

# Select most correlated variables based on the correlation assesment above (lines 60 - 88)
caret_findCorr<-findCorrelation(CorrelationMatrix, cutoff = 0.99, names = T)

# Remove correlated vars
predictor_df_reduced<-SpecLib %>%
  dplyr::select(-caret_findCorr)

# Rebuild Model on removal of intercorrelated variables at best cutoff value
rf_mod_rang2<-ranger(Classes ~ .,data = predictor_df_reduced,
                     num.trees = 10000,
                     importance = "impurity_corrected",
                     local.importance = TRUE) # OOB prediction error (MSE):  0.2676 % , R2: 0.7809

# Creates a sequence of numbers that represents the number of variables to choose to build models
NoofVars<-seq(5,50,by = 5)

# List of models
Modslist<-lapply(1:length(NoofVars),function(x){
  
  # Creates a dataframe with all variables and their imoportance
  ImportantVarsFrame<-enframe(rf_mod_rang2$variable.importance, 
                              name="predictor", value="importance")
  
  # Function selects the most important variables
  Imp_Vars<-ImportantVarsFrame[order(ImportantVarsFrame$importance,decreasing = TRUE),][1:NoofVars[x],]
  
  # Grabs the names of the 50 most important variables from predictors dataframe  
  ImpVars_names<-unique(Imp_Vars$predictor)%>%
    as.character()
  
  # Creates a new model built on important variables
  New_Speclib<-predictor_df_reduced%>%
    dplyr::select(Classes,ImpVars_names)
  
  # Build ne model
  rfNew<-ranger(Classes ~ .,data = New_Speclib,
                num.trees =1000,
                local.importance = TRUE,
                classification = TRUE)
  return(rfNew)
  
})%>%
  setNames(paste("B",NoofVars,"Most_ImpVariables",sep="_"))

# We can print the prediction error for each model, see below
listofmoderors<-lapply(Modslist,function(x)
  return(x$prediction.error))

#$A_5_Most_ImpVariables
#[1] 0.2080392
#
#$A_10_Most_ImpVariables
#[1] 0.1305287
#
#$A_15_Most_ImpVariables
#[1] 0.1060366
#
#$A_20_Most_ImpVariables
#[1] 0.08889209
#
#$A_25_Most_ImpVariables
#[1] 0.08442588
#
#$A_30_Most_ImpVariables
#[1] 0.08745138
#
#$A_35_Most_ImpVariables
#[1] 0.08370552
#
#$A_40_Most_ImpVariables
#[1] 0.08068002
#
#$A_45_Most_ImpVariables
#[1] 0.0822648
#
#$A_50_Most_ImpVariables
#[1] 0.07995966


# Creates a dataframe with errors for each model
error_df<-do.call("rbind",listofmoderors)%>%
  as.data.frame()

# Adds another column
error_df$Vars<-rownames(error_df)

# Changes column names
names(error_df)<-c("error","Vars")

# Lets R respect the order in data.frame.
error_df$Vars <- factor(error_df$Var,
                        levels = error_df$Var
                        [order(error_df$error)])

# Creates a plot of the model errors at each number of most important variables
error_df%>%
  ggplot(aes(x  = Vars, y = error))+
  theme_bw()+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Model errors after selecting the most important variables")

ggsave("Model_errors_VarImp_landcover_allcat.jpg")

list2env(Modslist ,.GlobalEnv)

# saves the model with the lowest error
save(B_25_Most_ImpVariables, file = "Best_Model25vars_landcover_allcat.rda")


