
# Use predict to make predictions on spectral classifier ranger model using validation data


#fit_ranger <- train(Classes ~ .,
#                    data = Spectral_Library,
#                    method = "rf", 
#                    metric = "accuracy",
#                    nodesize =1,
#                    num.trees = 10000,
#                    importance = "impurity_corrected",
#                    local.importance = TRUE)

outputs_folder<-"/Forests/ROutputs/"
input_folder  <-"/Forests/ROutputs/Predictors/"

names_SpecLibPreds_val = list.files(input_folder, pattern="ash_health_validation_SpecLib_Derivs.csv",full.names = T)

remove_names<-c("ScanID","Class1","Class2","Class4","Area","Class2_Freq"
                ,"Class3_Freq","Class4_Freq","Tree_numbe","x","y")

# Unit test PASSES
# Spectral_Library<-read.csv(names_SpecLibPreds) %>% dim() #6506 by 268
# Reads in spectral library
Spectral_Library_val<-read.csv(names_SpecLibPreds_val)

# Unit test PASSES 
# Spectral_Library[remove_names] = NULL # dim( Spectral_Library) 6506 by 265 ... removes 3 columns
# Removes unwanted metadata from dataframe 
Spectral_Library_val[remove_names] = NULL

#retain_names <- c("Condition_", "Carter4" ,      "MCARI2OSAVI2",  "MCARI2",        "SR6" ,          "DDn",           "OSAVI2" ,       "DD",           
#                   "mSR705",        "MTCI",          "mND705",        "Maccioni",      "OSAVI",         "Vogelmann" ,    "CI2",          
#                 "Vogelmann2" ,   "Carter2",       "SumDr1" ,       "GDVI4",         "PSND" ,         "NDVI" ,         "EVI" ,         
#                   "PARS" ,         "CRI1" ,         "X677.593nm_10", "X407.593nm_10", "REPLi",         "MPRI",          "CRI4" ,        
#                   "Datt3" )

# Change column name with all the levels to "classes"
names(Spectral_Library_val)[1]<-"Classes"

##Try rerunning with the "Classes" being numeric or being an ordinal factor
Spectral_Library_val[, "Classes"] <- as.factor(Spectral_Library_val[, "Classes"])

prediction <- predict(A_20_Most_ImpVariables, data=Spectral_Library_val)   

predictedValues <- prediction$predictions
dataValues <- Spectral_Library_val$Classes

plotData <- cbind(predictedValues, dataValues) %>% as.data.frame()
#plot(plotData)

Confusion_matrix = table(plotData$dataValues, plotData$predictedValues)

# overall classification accuracy
accuracy = mean (plotData$dataValues == plotData$predictedValues)
error    = mean (plotData$dataValues != plotData$predictedValues)

tst<-plotData$dataValues != plotData$predictedValues #vector of t/f where f = obs not same as pred
tst<-as.data.frame(tst)
colnames(tst)<-"var"
tst_f<-tst %>% subset(var==T) %>% count() # count number of times where obs don't match pred
1-tst_f/nrow(tst)


str(plotData)
levels(as.factor(plotData$predictedValues))
levels(as.factor(plotData$dataValues))

