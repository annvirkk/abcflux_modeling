


library("dplyr")
library("caret")
library("xgboost")
library("parallel")
library("doParallel")
library("kernlab")
library("groupdata2", lib.loc="/mnt/data1/boreal/avirkkala/packages")  


### Load modeling data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")



### Check variable distributions - this needs to be done with all subsets
hist(d$NEE_gC_m2)
hist(d$GPP_gC_m2)
hist(d$Reco_gC_m2)



### And number of sites and obs in the different levels of the categorical variables
unique(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
d %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# sites with NA?
d$Study_ID_Short[which(is.na(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged))] 

d %>% filter(!is.na(GPP_gC_m2)) %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short))) # nothing from barren!
d %>% filter(!is.na(Reco_gC_m2)) %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))


# # NOTE NOTE! Barren obs used to be missing from GPP, and only one site in Reco but not anymore
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



d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(forest_age_class_forest_age_sites) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
d %>% filter(!is.na(Reco_gC_m2)) %>% group_by(forest_age_class_forest_age_sites) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
d %>% filter(!is.na(GPP_gC_m2)) %>% group_by(forest_age_class_forest_age_sites) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))



### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 


### Predictors
names(d)

## List predictors for the models
# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "swe_terraclimate_sites", # met
                       
                       "tmean_TerraClimate_averages", "ppt_TerraClimate_averages", # climate
                       
                       "trend_20yrprior_terra_change_id",  "terra_trend_19601990", # temperature change - note that the naming convention changed a bit...
                       
                       "ndvi_trend_10yrprior_ndvi_change_id",  "ndvi_trend_19812010", # ndvi change trend 
                       
                       "Barrow_CO2_conc_Barrow_CO2conc", # atmos CO2 conc
                       
                       "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                       
                       "NDVI_whittaker_constant_monthly_mean", # Optical RS, dropped several because highly correlated
                       
                       "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", # surface temp
                       
                       "water_ground_MCD43A4_annual_water_ground_sites_low_quality", "water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality", # wetness
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # topo
                       
                       "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass", # c stocks
                       
                       "Percent_NonTree_Vegetation_MOD44B_sites", "Percent_NonVegetated_MOD44B_sites", "Percent_Tree_Cover_MOD44B_sites", # veg cover
                       
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", # veg type
                       
                       "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                       "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", # soil variables
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", # Permafrost (static)
                       
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned",   # time since fire

                       "TKWP_Thermokarst", "TKHP_Thermokarst", # themokarst vulnerability 
                       
                       "forest_age_class_forest_age_sites" # forest age
                      
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km




# Variables used in 20 km spatial resolution models
Baseline_vars_20km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites", "swe_terraclimate_sites", # tmean included because don't have LST
                        
                        "tmean_TerraClimate_averages", "ppt_TerraClimate_averages", 
                        
                        "trend_20yrprior_terra_change_id",  "terra_trend_19601990", 
                        
                        "ndvi_trend_19812010", # note cannot have ndvi prior to 10 yrs because no data from 1979
                        
                        "Barrow_CO2_conc_Barrow_CO2conc",
                        
                        "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", 
                        
                        "ndvi3g_lowest_mean_GIMMS3g_NDVI_sites_high_and_low_quality",  ############### VAIHDA NIMI
                        
                        "SMMR_SSMIS_thaw_days_NTSG_FT_SMMR_SSMIS_25km", # microwave 
                      
                        "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", 
                        
                        "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                        
                        "Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Percent_TreeCover_AVHRR_VCF5KYR", "Percent_NonVegetated_AVHRR_VCF5KYR", # equivalent to MOD tree cover product
                        
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", 
                        
                        "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                        "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent",  
                        
                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", 
                        
                        "Number_of_days_since_fire_classes_gfed_monthly_calc",  
                        
                        "TKWP_Thermokarst", "TKHP_Thermokarst", 
                        
                        "forest_age_class_forest_age_sites"
)

# check that the columns exist
Baseline_vars_20km %in% colnames(d)

# variables as factors
d$TKWP_Thermokarst <- as.factor(d$TKWP_Thermokarst)
d$TKHP_Thermokarst <- as.factor(d$TKHP_Thermokarst)
d$Number_of_days_since_fire_classes_gfed_monthly_calc <- as.factor(d$Number_of_days_since_fire_classes_gfed_monthly_calc)
d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- as.factor(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- as.factor(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
d$forest_age_class_forest_age_sites <- as.factor(d$forest_age_class_forest_age_sites)
# (note that if you would use dummy variables those would not need to be factors)









### Set up clusters for parallel processing 
cluster <- makeCluster(detectCores()) # keep all - Kubernetes uses 7 cores per run
registerDoParallel(cluster)
# see the tutorial here: https://rpubs.com/lgreski/improvingCaretPerformance


### Start looping through the response variables ###

for (flux in resp_vars) {
  
  print(flux)
  # flux <- "NEE_gC_m2"

  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  
  
  modeldata2 <- d[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata2)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  
  
  
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
  
  
  
  # 20 km
  set.seed(448)
  folds <- groupdata2::fold(data= modeldata2,
                            k = length(unique(modeldata2$Study_ID_Short)), id_col = "Study_ID_Short")
  
  # merge
  folds_data <- subset(folds, select=c( ".folds", "id"))
  modeldata2 <- full_join(modeldata2, folds_data, by="id")
  
  # create a row ID
  modeldata2$samplerow <- seq(1, length(modeldata2$Study_ID_Short), by=1)
  
  indices2 <- list()
  indices_not2 <- list()
  
  for (k in unique(modeldata2$.folds)) {
    
    # k = 1
    subs <- subset(modeldata2, .folds==k)
    
    # list the row ids for the fold
    sites <- list(as.integer(subs$samplerow) )
    
    # list other row ids that don't belong to the k fold
    sites_not <- list(as.integer(modeldata2$samplerow[!(modeldata2$samplerow %in% unlist(sites))]))
    
    # check that all the rows are included
    length(sites_not[[1]]) + length(sites[[1]]) == length(modeldata2$Study_ID_Short)
    
    # append to list (caret wants these as lists)
    # row indices for each fold, used for model evaluation
    indices2 <- append(indices2, sites)
    # row indices for model training
    indices_not2 <- append(indices_not2, sites_not)
    
  }
  
  
  names(indices2) <- 1:length(indices2)
  names(indices_not2) <- 1:length(indices2)
  
  
  
  # Check factor levels (thermokarst, fire, veg type) in different subsets
  for (ii in 1:10) {
    
    # ii <- 7
    # subset to training and test data
    train1 <- modeldata1[indices_not1[[ii]] ,]
    test1 <- modeldata1[indices1[[ii]] ,]
    
    ### Check that the values in test data are also in training data
    # Thermokarst
    print(unique(test1$Thermokarst) %in% unique(train1$Thermokarst))
    
    
    # Fire
    print(unique(test1$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %in% unique(train1$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned))
    
    
    # ESA CCI veg type
    print(unique(test1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %in% unique(train1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged))
    
    
    ### And same for modeldata2 and other two variables....
    # subset to training and test data
    train1 <- modeldata2[indices_not1[[ii]] ,]
    test1 <- modeldata2[indices1[[ii]] ,]
    
    
    ### Check that the values in test data are also in training data
    # Thermokarst
    print(unique(test1$Thermokarst) %in% unique(train1$Thermokarst))
    
    # Fire
    print(unique(test1$Number_of_days_since_fire_classes_gfed_monthly_calc) %in% unique(train1$Number_of_days_since_fire_classes_gfed_monthly_calc))
    
    
    # ESA CCI veg type
    print(unique(test1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %in% unique(train1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged))
    
  }
  
  
  
  
  print("model tuning and feature selection starting")
  
  
  # Recursive feature elimination (RFE) is a feature selection method that fits a model and
  # removes the weakest feature (or features) until the specified number of features is reached.
  # In the current RFE algorithm, the training data is being used for at least three purposes: predictor selection, model fitting and performance evaluation.
  # We will give 40 and less predictor options for 1 km models and 35 and less predictor options for 20 km models (just because the 20 km model has a lower number of predictors)


  ### GBM


  # Control parameters
  # 1 km model
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
  # tune length is per default set to 3 - the model will try three different estimates for each parameter
  # rfeControl is a wrapper for trainControl. trainControl has slightly different options and names for those options but the basic idea is similar. We don't need to define trainControl now since we have rfeControl


  # 20 km model
  rfecontrol2 <- rfeControl(functions=treebagFuncs, #caret-specific rfe process functions - there are e.g. lmFuncs too that are specific to one model type
                            method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
                            # and no need to specify number or repeats either
                            returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
                            # p = 0.7, # For leave-group out cross-validation: the training percentage. No need to worry about this since we use pre-defined indices
                            #summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
                            #selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
                            index = indices_not2, # a list with elements for each external resampling iteration. Each list element is the sample rows used for training at that iteration.
                            indexOut = indices2, # a list (the same length as index) that dictates which sample are held-out for each resample.
                            allowParallel = TRUE,  # parallel processing
                            saveDetails =TRUE) # a logical to save the predictions and variable importances from the selection process

  # some useful web pages about the parameters:
  # https://stackoverflow.com/questions/24612824/r-caret-package-error-if-i-specified-index-for-both-rfe-control-and-train-contr
  # https://github.com/topepo/caret/issues/1019
  # https://stackoverflow.com/questions/37749009/custom-training-rows-when-using-caret-package
  # https://stats.stackexchange.com/questions/109340/leave-one-subject-out-cross-validation-in-caret
  # https://stats.stackexchange.com/questions/214387/results-from-rfe-function-caret-to-compute-average-metrics-r
  # https://stats.stackexchange.com/questions/323356/index-argument-with-createfolds-in-traincontrol-caret-package


  
  # Note that it's important to differentiate between formula and non-formula formulations: 
  # https://stackoverflow.com/questions/22200923/different-results-with-formula-and-non-formula-for-caret-training?fbclid=IwAR0d1bTb07FZzVojdlClemMmgMUhJk02Z_8uGRyKTYM8BBHGLtstJfuicVE
  # For gbm and rf I'm using the non-formula method (without ~) where dummy variables for factors are not created (because trees can handle these in their own way)
  # But for svm I'm using the formula method, so categorical variables are transformed to dummies
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = rfe(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                sizes = c(40, 35, 30, 25, 20, 15), # a numeric vector of integers corresponding to the number of features that should be retained
                rfeControl = rfecontrol1, # rfe parameters
                method="gbm") # modeling method
                #trControl=tunecontrol1) # tuning parameters - not needed


  print("1 km rfe and tuning done")

  ### Write the model out
  saveRDS(rfe_fit, paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(flux, "1km_gbm_loocv",  sep="_"), ".rds"))

  
  # # TEMPORARY
  # ### run the RFE and model tuning algorithm with 20 km predictors
  # set.seed(448)
  # rfe_fit2 = rfe(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
  #               sizes = c(35, 30, 25, 20, 15),
  #               rfeControl = rfecontrol2,
  #               method="gbm")
  # 
  # 
  # print("20 km rfe and tuning done")
  # 
  # ### Write the model out
  # saveRDS(rfe_fit2, paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(flux, "20km_gbm_loocv", sep="_"), ".rds"))

  print("gbm done, moving on to rf")



  ### RF

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


  rfecontrol2 <- rfeControl(functions=rfFuncs, #caret-specific rfe process functions - there are e.g. lmFuncs too that are specific to one model type
                            method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
                            # and no need to specify number or repeats either
                            returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
                            # p = 0.7, # For leave-group out cross-validation: the training percentage. No need to worry about this since we use pre-defined indices
                            #summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
                            #selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
                            index = indices_not2, # a list with elements for each external resampling iteration. Each list element is the sample rows used for training at that iteration.
                            indexOut = indices2, # a list (the same length as index) that dictates which sample are held-out for each resample.
                            allowParallel = TRUE,  # parallel processing
                            saveDetails =TRUE) # a logical to save the predictions and variable importances from the selection process



  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = rfe(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                sizes = c(40, 35, 30, 25, 20, 15), # a numeric vector of integers corresponding to the number of features that should be retained
                rfeControl = rfecontrol1, # rfe parameters
                method="ranger") # modeling method


  print("1 km rfe and tuning done")

  ### Write the model out
  saveRDS(rfe_fit, paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(flux, "1km_rf_loocv",  sep="_"), ".rds"))

  
# # TEMPORARY
# ### run the RFE and model tuning algorithm with 20 km predictors
# set.seed(448)
# rfe_fit2 = rfe(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
#                sizes = c(35, 30, 25, 20, 15),
#                rfeControl = rfecontrol2,
#                method="ranger")
# 
# 
#   print("20 km rfe and tuning done")
# 
#   ### Write the model out
#   saveRDS(rfe_fit2, paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(flux, "20km_rf_loocv", sep="_"), ".rds"))

  
  print("rf done, moving on to svm")
  
  
  
  ### SVM - I had some issues with the model formulations but these should work now

  
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
  
  rfecontrol2 <- rfeControl(functions=caretFuncs, #caret-specific rfe process functions - there are e.g. lmFuncs too that are specific to one model type
                            method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
                            # and no need to specify number or repeats either
                            returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
                            # p = 0.7, # For leave-group out cross-validation: the training percentage. No need to worry about this since we use pre-defined indices
                            #summaryFunction = defaultSummary, # a function to compute performance metrics across resamples. 
                            #selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
                            index = indices_not2, # a list with elements for each external resampling iteration. Each list element is the sample rows used for training at that iteration.
                            indexOut = indices2, # a list (the same length as index) that dictates which sample are held-out for each resample.
                            allowParallel = TRUE,  # parallel processing
                            saveDetails =TRUE) # a logical to save the predictions and variable importances from the selection process
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = rfe(as.formula(paste(flux, "~", paste(Baseline_vars_1km, collapse="+"))),
                  sizes = c(40, 35, 30, 25, 20, 15), # a numeric vector of integers corresponding to the number of features that should be retained
                  rfeControl = rfecontrol1, # rfe parameters
                  method="svmRadial", data=modeldata1) # tested several other svm approaches but this one seemed to be the best
  
  # note: used the formula (y~k+s...) instead of non-formula structure (data[y], data[k, s]) for svm because the non-formula resulted in errors. 
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(flux, "1km_svm_loocv",  sep="_"), ".rds"))
  
  
  # # TEMPORARY
  # ### run the RFE and model tuning algorithm with 20 km predictors
  # set.seed(448)
  # rfe_fit2 = rfe(as.formula(paste(flux, "~", paste(Baseline_vars_20km, collapse="+"))),
  #                 sizes = c(35, 30, 25, 20, 15), # a numeric vector of integers corresponding to the number of features that should be retained
  #                 rfeControl = rfecontrol2, # rfe parameters
  #                 method="svmRadial", data=modeldata2)
  # 
  # 
  # print("20 km rfe and tuning done")
  # 
  # ### Write the model out
  # saveRDS(rfe_fit2, paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(flux, "20km_svm_loocv", sep="_"), ".rds"))
  # 
  
}



