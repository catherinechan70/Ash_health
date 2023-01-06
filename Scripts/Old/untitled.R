ImportantVars<-function(x){
  
  # Creates Random forest Model
  set.seed(2017)
  rf_mod_rang<-ranger(Classes ~ .,data = x,
                      mtry = sqrt(ncol(x)),
                      num.trees = 1001,
                      importance = "impurity_corrected",
                      local.importance = TRUE)
  
  
  #rf_mod<-randomForest(Classes ~ .,data = x,
  #                     mtry = sqrt(ncol(x)),
  #                     ntree = 1001,
  #                     localImp = TRUE)
  #
  # Selects the 50 most important variables
  # This is achieved by calculating distribution of minimal depth 
  rf_mod_rang<-unlist(rf_mod_rang, recursive = F, use.names = T);
  ImportantVars<-enframe(rf_mod_rang$variable.importance, name="predictor", value="imprtance_air")
  #ImportantVars<-plot_min_depth_distribution(
  #  min_depth_distribution(rf_mod),
  #  min_no_of_trees = 200,
  #  mean_sample = "relevant_trees",
  #  k = 50)
  return(ImportantVars)
}
SpecLibs_Preds<-lapply(names_SpecLibPreds,ImportantVars)
