


library("dplyr")
library("caret")
library("xgboost")
library("parallel")
library("doParallel")
library("kernlab")
# install groupdata
#library("groupdata2")
# If the directory doesn't exist. You will need to make sure you have permissions to create the directory
# install and specify the created directory
#install.packages("groupdata2", lib="/mnt/data1/boreal/avirkkala/packages") 
# use library while adding path to the Rlib path.
library("groupdata2", lib.loc="/mnt/data1/boreal/avirkkala/packages")  


setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")



### Check variable distributions - this needs to be done with all subsets
hist(d$NEE_gC_m2)
hist(d$GPP_gC_m2)
hist(d$Reco_gC_m2)

### And counts in groups
unique(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
d %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# sites with NA?
d$Study_ID_Short[which(is.na(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged))] # quite important ones!

d %>% filter(!is.na(GPP_gC_m2)) %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short))) # nothing from barren!
d %>% filter(!is.na(Reco_gC_m2)) %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))


# # NOTE NOTE! Barren obs missing from GPP, and only one site in Reco - > merge with P
# d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- ifelse(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged==1, 31, d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)


d %>% group_by(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
d %>% filter(!is.na(GPP_gC_m2)) %>% group_by(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short))) 
d %>% filter(!is.na(Reco_gC_m2)) %>% group_by(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# not enough fire sites... merge 1-2 and 3-4!!
d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- ifelse(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned==2, 1, d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- ifelse(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned==4, 3, d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)


d %>% group_by(Number_of_days_since_fire_classes_gfed_monthly_calc) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short))) # ok!


d %>% group_by(TKWP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
d %>% filter(!is.na(GPP_gC_m2)) %>% group_by(TKWP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short))) 
d %>% filter(!is.na(Reco_gC_m2)) %>% group_by(TKWP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(TKWP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
unique(d$TKWP_Thermokarst)
d %>% group_by(TKHP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
d %>% filter(!is.na(GPP_gC_m2)) %>% group_by(TKHP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short))) 
d %>% filter(!is.na(Reco_gC_m2)) %>% group_by(TKHP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(TKHP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
unique(d$TKHP_Thermokarst) # huom very high puuttuu!! eli se on oikein
# enough classes!

# There are also some NAs in the variables. NAs are handled in different ways across ML methods: https://stats.stackexchange.com/questions/144922/r-caret-and-nas
length(which(is.na(d$tmean_terraclimate_sites)))
length(which(is.na(d$NDVI_whittaker_constant_monthly_mean))) # these are just pre-2000
length(which(is.na(d$LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality)))
length(which(is.na(d$Soil.temperature.level.1_era5_soilmoist_temp_snow)))
length(which(is.na(d$Snow.depth_era5_soilmoist_temp_snow)))
length(which(is.na(d$PFR_ESA_CCI_Permafrostv2))) # where is this??

# these 2 have too many NAs
length(which(is.na(d$ABoVE_fractional_water_ABoVE_fractional_water)))
length(which(is.na(d$AMSR6km_thaw_days_NTSG_FT_AMSR_6km)))


k <- subset(d, select=c(Study_ID, Meas_year, Interval, Soil.temperature.level.1_era5_soilmoist_temp_snow, Snow.depth_era5_soilmoist_temp_snow))


# ### Factor conversions - uncommented because not needed anymore! just need to code as.factor
# # note that randomForest and svmRadial can handle variables coded as "as.factor"
# # but xgboost cannot: https://github.com/dmlc/xgboost/issues/95
# # so we will need to do one-hot encoding
# 
# # Thermokarst
# d <- as.data.frame(d)
# d$Thermokarst <- factor(d$Thermokarst)
# onehot <- model.matrix(~0+d[, 'Thermokarst'])
# attr(onehot, "dimnames")[[2]] <- paste("Thermokarst", levels(d$Thermokarst), sep="_")
# d <- cbind(d, onehot)
# 
# # land cover - no NAs
# d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- factor(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
# onehot <- model.matrix(~0+d[, 'ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged'])
# attr(onehot, "dimnames")[[2]] <- paste("ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", levels(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged), sep="_")
# d <- cbind(d, onehot)
# 
# # Fire burn classes
# d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- factor(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
# onehot <- model.matrix(~0+d[, 'Number_of_days_since_fire_classes_MCD64A1_sites_cleaned'])
# attr(onehot, "dimnames")[[2]] <- paste("Number_of_days_since_fire_classes_MCD64A1_sites_cleaned", levels(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned), sep="_")
# d <- cbind(d, onehot)
# 
# 
# 
# d$Number_of_days_since_fire_classes_gfed_monthly_calc <- factor(d$Number_of_days_since_fire_classes_gfed_monthly_calc)
# onehot <- model.matrix(~0+d[, 'Number_of_days_since_fire_classes_gfed_monthly_calc'])
# attr(onehot, "dimnames")[[2]] <- paste("Number_of_days_since_fire_classes_gfed_monthly_calc", levels(d$Number_of_days_since_fire_classes_gfed_monthly_calc), sep="_")
# d <- cbind(d, onehot)



### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 


### Predictors
names(d)

## List predictors for the models (including the one-hot encoded factors)
# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "NDVI_whittaker_constant_monthly_mean", "Snow.cover_era5_soilmoist_temp_snow", "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged"
                      
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km



### Start looping through the response variables ###

for (flux in resp_vars) {
  
  print(flux)
  # flux <- "NEE_gC_m2"

  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) # only 86 obs
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  
  modeldata1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- factor(modeldata1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
  
  
  
  
  # Create a list of row indices for cross validation splits (for leave-one-fold out) 
  # leave one site out used because of the highly different number of observations in some factor data levels
  
  # 1 km
  set.seed(448) # so that folds will always be the same
  folds <- fold(data= modeldata1,
                k = length(unique(modeldata1$Study_ID_Short)),
                id_col = "Study_ID_Short")
  
  # merge
  folds_data <- subset(folds, select=c( ".folds", "id"))
  modeldata1 <- full_join(modeldata1, folds_data, by="id")
  
  # create a row ID
  modeldata1$samplerow <- seq(1, length(modeldata1$Study_ID_Short), by=1)
  
  indices1 <- list()
  indices_not1 <- list()
  
  for (k in unique(modeldata1$.folds)) {
    
    # k = 1
    subs <- subset(modeldata1, .folds==k)
    
    # list the row ids for the fold
    sites <- list(as.integer(subs$samplerow) )
    
    # list other row ids that don't belong to the k fold
    sites_not <- list(as.integer(modeldata1$samplerow[!(modeldata1$samplerow %in% unlist(sites))]))
    
    # check that all the rows are included
    length(sites_not[[1]]) + length(sites[[1]]) == length(modeldata1$Study_ID_Short)
    
    # append to list (caret wants these as lists)
    # row indices for each fold, used for model evaluation
    indices1 <- append(indices1, sites)
    # row indices for model training
    indices_not1 <- append(indices_not1, sites_not)
    
  }
  
  
  names(indices1) <- 1:length(indices1)
  names(indices_not1) <- 1:length(indices_not1)
  
  
  
  
  
  print("model tuning and feature selection starting")
  

  ### Uncommented in March, 2021 because this information will be in rfecontrol too
  # ## Model tuning parameters
  # # Includes some options about what kind of information is saved
  # tunecontrol1 <- trainControl(
  #   method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
  #   verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
  #   returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
  #   returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
  #   # p = 0.7, # For leave-group out cross-validation: the training percentage
  #   summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
  #   selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
  #   index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
  #   indexOut = indices1,
  #   allowParallel = TRUE, # parallel processing
  #   savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  # # Basically most of the key parameters are still coming from the rfeControl command which is a wrapper for train
  # # But if I'd like to set different tuning parameters in trainControl, that is still possible: https://stackoverflow.com/questions/51933704/feature-selection-with-caret-rfe-and-training-with-another-method
  # # indexFinal an optional vector of integers indicating which samples are used to fit the final model after resampling. If NULL, then entire data set is used. NULL is the default?
  # 
  # # Includes some options about what kind of information is saved
  # tunecontrol2 <- trainControl(
  #   method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
  #   verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
  #   returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
  #   returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
  #   # p = 0.7, # For leave-group out cross-validation: the training percentage
  #   summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
  #   selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
  #   index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
  #   indexOut = indices2,
  #   allowParallel = TRUE, # parallel processing
  #   savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  # # Basically most of the key parameters are still coming from the rfeControl command which is a wrapper for train
  # # But if I'd like to set different tuning parameters in trainControl, that is still possible: https://stackoverflow.com/questions/51933704/feature-selection-with-caret-rfe-and-training-with-another-method
  # # indexFinal an optional vector of integers indicating which samples are used to fit the final model after resampling. If NULL, then entire data set is used. NULL is the default?



  ### GBM


  ## Feature selection parameters
  # define the predictions using a rfe selection function
  # rfe= Recursive Feature Elimination.
  # Recursive feature elimination (RFE) is a feature selection method that fits a model and
  # removes the weakest feature (or features) until the specified number of features is reached.
  # In the current RFE algorithm, the training data is being used for at least three purposes: predictor selection, model fitting and performance evaluation.

  # Control parameters
  rfecontrol1 <- rfeControl(functions=treebagFuncs, #caret-specific rfe process functions - there are e.g. lmFuncs too that are specific to one model type
                            method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
                            # and no need to specify number or repeats either
                            returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
                            # p = 0.7, # For leave-group out cross-validation: the training percentage. No need to worry about this since we use pre-defined indices
                            #summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
                            #selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
                            index = indices_not1, # a list with elements for each external resampling iteration. Each list element is the sample rows used for training at that iteration.
                            indexOut = indices1, # a list (the same length as index) that dictates which sample are held-out for each resample.
                            allowParallel = TRUE,  # parallel processing
                            saveDetails =TRUE) # a logical to save the predictions and variable importances from the selection process



  # some useful web pages about the parameters:
  # https://stackoverflow.com/questions/24612824/r-caret-package-error-if-i-specified-index-for-both-rfe-control-and-train-contr
  # https://github.com/topepo/caret/issues/1019
  # https://stackoverflow.com/questions/37749009/custom-training-rows-when-using-caret-package
  # https://stats.stackexchange.com/questions/109340/leave-one-subject-out-cross-validation-in-caret
  # https://stats.stackexchange.com/questions/214387/results-from-rfe-function-caret-to-compute-average-metrics-r
  # https://stats.stackexchange.com/questions/323356/index-argument-with-createfolds-in-traincontrol-caret-package


  ### run the RFE and model tuning algorithm with 1 km predictors
  # note that no data not accepted for the response, and with svm and rf in predictors
  set.seed(448)
  rfe_fit = rfe(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                sizes = c(5, 4, 3), # a numeric vector of integers corresponding to the number of features that should be retained
                rfeControl = rfecontrol1, # rfe parameters
                method="gbm") # modeling method
                #trControl=tunecontrol1) # tuning parameters


  print("1 km rfe and tuning done")


  ### Write the model out
  saveRDS(rfe_fit, paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(flux, "1km_gbm_loocv_simpletestmodel2",  sep="_"), ".rds"))


  ### RF


  ## Feature selection parameters
  # define the predictions using a rfe selection function
  # rfe= Recursive Feature Elimination.
  # Recursive feature elimination (RFE) is a feature selection method that fits a model and
  # removes the weakest feature (or features) until the specified number of features is reached.
  # In the current RFE algorithm, the training data is being used for at least three purposes: predictor selection, model fitting and performance evaluation.

  # Control parameters
  rfecontrol1 <- rfeControl(functions=rfFuncs, #caret-specific rfe process functions - there are e.g. lmFuncs too that are specific to one model type
                            method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
                            # and no need to specify number or repeats either
                            returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
                            # p = 0.7, # For leave-group out cross-validation: the training percentage. No need to worry about this since we use pre-defined indices
                            #summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
                            #selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
                            index = indices_not1, # a list with elements for each external resampling iteration. Each list element is the sample rows used for training at that iteration.
                            indexOut = indices1, # a list (the same length as index) that dictates which sample are held-out for each resample.
                            allowParallel = TRUE,  # parallel processing
                            saveDetails =TRUE) # a logical to save the predictions and variable importances from the selection process


  # some useful web pages about the parameters:
  # https://stackoverflow.com/questions/24612824/r-caret-package-error-if-i-specified-index-for-both-rfe-control-and-train-contr
  # https://github.com/topepo/caret/issues/1019
  # https://stackoverflow.com/questions/37749009/custom-training-rows-when-using-caret-package
  # https://stats.stackexchange.com/questions/109340/leave-one-subject-out-cross-validation-in-caret
  # https://stats.stackexchange.com/questions/214387/results-from-rfe-function-caret-to-compute-average-metrics-r
  # https://stats.stackexchange.com/questions/323356/index-argument-with-createfolds-in-traincontrol-caret-package

  ### run the RFE and model tuning algorithm with 1 km predictors
  # note that no data not accepted for the response, and with svm and rf in predictors
  set.seed(448)
  rfe_fit = rfe(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                sizes = c(5, 4, 3), # a numeric vector of integers corresponding to the number of features that should be retained
                rfeControl = rfecontrol1, # rfe parameters
                method="ranger") # modeling method
  #trControl=tunecontrol1) # tuning parameters


  print("1 km rfe and tuning done")

  ### Write the model out
  saveRDS(rfe_fit, paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(flux, "1km_rf_loocv_simpletestmodel2",  sep="_"), ".rds"))

 
 
  
  
  ### SVM
  
  
  ## Feature selection parameters
  # define the predictions using a rfe selection function
  # rfe= Recursive Feature Elimination. 
  # Recursive feature elimination (RFE) is a feature selection method that fits a model and 
  # removes the weakest feature (or features) until the specified number of features is reached.
  # In the current RFE algorithm, the training data is being used for at least three purposes: predictor selection, model fitting and performance evaluation. 
  
  # Control parameters
  rfecontrol1 <- rfeControl(functions=caretFuncs, #caret-specific rfe process functions - there are e.g. lmFuncs too that are specific to one model type
                            method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
                            # and no need to specify number or repeats either
                            returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
                            # p = 0.7, # For leave-group out cross-validation: the training percentage. No need to worry about this since we use pre-defined indices
                            #summaryFunction = defaultSummary, # a function to compute performance metrics across resamples. 
                            #selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
                            index = indices_not1, # a list with elements for each external resampling iteration. Each list element is the sample rows used for training at that iteration.
                            indexOut = indices1, # a list (the same length as index) that dictates which sample are held-out for each resample.
                            allowParallel = TRUE,  # parallel processing
                            saveDetails =TRUE) # a logical to save the predictions and variable importances from the selection process
  
  # caretFuncs should work, see here: https://rdrr.io/cran/caret/man/rfe.html
  
  
  # some useful web pages about the parameters:
  # https://stackoverflow.com/questions/24612824/r-caret-package-error-if-i-specified-index-for-both-rfe-control-and-train-contr
  # https://github.com/topepo/caret/issues/1019
  # https://stackoverflow.com/questions/37749009/custom-training-rows-when-using-caret-package
  # https://stats.stackexchange.com/questions/109340/leave-one-subject-out-cross-validation-in-caret
  # https://stats.stackexchange.com/questions/214387/results-from-rfe-function-caret-to-compute-average-metrics-r
  # https://stats.stackexchange.com/questions/323356/index-argument-with-createfolds-in-traincontrol-caret-package
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  # note that no data not accepted for the response, and with svm and rf in predictors
  set.seed(448)
  rfe_fit = rfe(as.formula(paste(flux, "~", paste(Baseline_vars_1km, collapse="+"))),
                  sizes = c(5, 4, 3), # a numeric vector of integers corresponding to the number of features that should be retained
                  rfeControl = rfecontrol1, # rfe parameters
                  method="svmRadial", data=modeldata1)
  
  # modeling method - tried svmradial here, and tried using only  cv in rfe and tunecontrol (i.e. no indices)
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(flux, "1km_svm_loocv_simpletestmodel2",  sep="_"), ".rds"))
  
 
  
  
}


### SIMPLETESTMODEL ekassa versiossa oli vain srad, vpd, ja esa

# SIMPLETESTMODEL2 näihin lisättiin ndvi ja snowcover

