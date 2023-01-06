
### Removing intercorrelated variables from a dataset before applying ranger models

library(caret)
library(dplyr)

all_preds_combineClass <- read.csv("/Forests/ROutputs/Predictors/all_preds_combineClass_cal.csv") # rows: 6506 cols: 268
all_preds_only_combineClass <- all_preds_combineClass[ ,-c(1:4)]   #rows: 6506  cols: 264

# Creates correlation matrix of all predictors
corrMatrix <- all_preds_only_combineClass %>% cor() #rows: 264 cols: 264

# Creates list of intercorrelated variables at different cutoff levels
caret_findcorr.9 <- findCorrelation(corrMatrix, cutoff = 0.9, verbose=TRUE, names=TRUE, exact=TRUE) %>%
  as.data.frame()   # rows: 213
caret_findcorr.92 <- findCorrelation(corrMatrix, cutoff = 0.92, verbose=TRUE, names=TRUE, exact=TRUE) %>%
  as.data.frame()   # rows: 206
caret_findcorr.95 <- findCorrelation(corrMatrix, cutoff = 0.95, verbose=TRUE, names=TRUE, exact=TRUE) %>%
  as.data.frame()   # rows: 196
caret_findcorr.97 <- findCorrelation(corrMatrix, cutoff = 0.97, verbose=TRUE, names=TRUE, exact=TRUE) %>%
  as.data.frame()   # rows: 184
caret_findcorr.99 <- findCorrelation(corrMatrix, cutoff = 0.99, verbose=TRUE, names=TRUE, exact=TRUE) %>%
  as.data.frame()   # rows: 165

# Create new dfs of predictors by removing intercorrelated variables 
all_preds_uncorr.9 <- all_preds_combineClass[setdiff(names(all_preds_combineClass), caret_findcorr.9$.)] 
# cols: 55
all_preds_uncorr.92 <- all_preds_combineClass[setdiff(names(all_preds_combineClass), caret_findcorr.92$.)] 
# cols: 62
all_preds_uncorr.95 <- all_preds_combineClass[setdiff(names(all_preds_combineClass), caret_findcorr.95$.)] 
# cols: 72
all_preds_uncorr.97 <- all_preds_combineClass[setdiff(names(all_preds_combineClass), caret_findcorr.97$.)] 
# cols: 84
all_preds_uncorr.99 <- all_preds_combineClass[setdiff(names(all_preds_combineClass), caret_findcorr.99$.)] 
# cols: 103

# Test that all (or some) of the actual values from the intercorrelated list are removed from the original all_preds df
### view(caret_findcorr.97)
### "Carter5" %in% names(all_preds_uncorr.97)

# Save the new dfs of uncorrelated predictors as .csv files
write.csv(all_preds_uncorr.9,"E://Forests//ROutputs//Predictors//uncorr_preds_9.csv", row.names =FALSE)
write.csv(all_preds_uncorr.92,"E://Forests//ROutputs//Predictors//uncorr_preds_92.csv", row.names =FALSE)
write.csv(all_preds_uncorr.95,"E://Forests//ROutputs//Predictors//uncorr_preds_95.csv", row.names =FALSE)
write.csv(all_preds_uncorr.97,"E://Forests//ROutputs//Predictors//uncorr_preds_97.csv", row.names =FALSE)
write.csv(all_preds_uncorr.99,"E://Forests//ROutputs//Predictors//uncorr_preds_99.csv", row.names =FALSE)



# Validation dataset for predictions

all_preds_combineClass_v <- read.csv("/Forests/ROutputs/Predictors/all_preds_combineClass_validation.csv") # rows: 2291 cols: 268

# Create new validation df of predictors by removing intercorrelated variables
all_preds_v_uncorr.99 <- all_preds_combineClass_v[setdiff(names(all_preds_combineClass_v), caret_findcorr.99$.)] 
# cols: 103
all_preds_v_uncorr.97 <- all_preds_combineClass_v[setdiff(names(all_preds_combineClass_v), caret_findcorr.97$.)] 
# cols: 84

# Save the new validation df of uncorrelated predictors as .csv files
write.csv(all_preds_v_uncorr.99,"E://Forests//ROutputs//Predictors//uncorr_v_preds_99.csv", row.names =FALSE)
write.csv(all_preds_v_uncorr.97,"E://Forests//ROutputs//Predictors//uncorr_v_preds_97.csv", row.names =FALSE)



#LowCor_SpecLib<-New_Speclib %>%
#  dplyr::select(Classes,-caret_findcorr.97) 
## Build ne model
#rfNew<-ranger(Classes ~ .,data = LowCor_SpecLib,
#              num.trees =200,
#              importance = "impurity_corrected",
#              local.importance = TRUE)
#


#  rfLowCor<-ranger(Classes ~ .,data = LowCor_SpecLib,
#                num.trees =200,
#                importance = "impurity_corrected",
#                local.importance = TRUE)





### Intercorrelated variables Instructions:
# 2) use findCorrelation in caret package to remove intercorrelated variables at a few different levels 
# (eg. change cutoff to range between 0.9 and 0.99) and see how either 
# r.squared (regression)/oob_error(classification) and error changes. 

#Make a correlation matrix
#corrMatrix <- predictor_df %>% cor()

#Use function in caret to find correlations between predictors at a user
#specified threshold in the parameter cutoff
#caret_findCorr <- findCorrelation(corrMatrix, cutoff = 0.97, verbose=TRUE, names=TRUE, exact=TRUE)

