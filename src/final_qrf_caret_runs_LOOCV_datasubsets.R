


### Note -changed this code now so that in each subset, kobbefjord is being removed
.libPaths("/home/master/R/x86_64-pc-linux-gnu-library/4.2")


library("dplyr")
library("caret")
library("parallel")
library("doParallel")
library("randomForest")
#install.packages("quantregForest", lib="D:/packages")  
library("quantregForest")  
library("groupdata2")  


### Load modeling data
setwd("/home/master/abcflux_modeling/src")
d <- read.csv("/home/master/flux_upscaling_data/results/final/modeldata_avg.csv")



### Check variable distributions - this needs to be done with all subsets
hist(d$NEE_gC_m2)
hist(d$GPP_gC_m2)
hist(d$Reco_gC_m2)


## First key subset: run model without larval outbreaks: removing only Lund_Kobbefjord_Ch because of extremely high uptake values. 
# Would like to keep L\xf3pez-Blanco_GL-NuF_tower1 because the flux values seem to make more sense (landscape average) but it does not have MODIS NDVI data -> will be dropped in 1 km model tuning too 
# Run this with the full list of predictors first

d <- subset(d, Study_ID_Short!="Lund_Kobbefjord_Ch")


### Response variables
resp_vars <- c("GPP_gC_m2", "Reco_gC_m2", "NEE_gC_m2") 


### Predictors

## List predictors for the models
# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", # met
                       
                       "tmean_TerraClimate_averages", "ppt_TerraClimate_averages", # climate
                       
                       "trend_20yrprior_terra_change_id",  "terra_trend_19601990", # temperature change - note that the naming convention changed a bit...
                       
                       "ndvi_trend_19812010", # ndvi change trend 
                       
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
                       
                       "TKWP_Thermokarst", "TKHP_Thermokarst", # themokarst vulnerability 
                       
                       "forest_age_class_forest_age_sites" # forest age
                       
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km




# Variables used in 20 km spatial resolution models
Baseline_vars_20km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites",  # tmean included because don't have LST
                        
                        "tmean_TerraClimate_averages", "ppt_TerraClimate_averages", 
                        
                        "trend_20yrprior_terra_change_id",  "terra_trend_19601990", 
                        
                        "ndvi_trend_19812010", # note cannot have ndvi prior to 10 yrs because no data from 1979
                        
                        "Barrow_CO2_conc_Barrow_CO2conc",
                        
                        "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", 
                        
                        "ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled",  
                        
                        "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", 
                        
                        "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                        
                        "Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Percent_TreeCover_AVHRR_VCF5KYR", "Percent_NonVegetated_AVHRR_VCF5KYR", # equivalent to MOD tree cover product
                        
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", 
                        
                        "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                        "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent",  
                        
                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", 
                        
                        "TKWP_Thermokarst", "TKHP_Thermokarst", 
                        
                        "forest_age_class_forest_age_sites"
)

# check that the columns exist
Baseline_vars_20km %in% colnames(d)



### Factor edits for the non-larval outbreak subset

# not enough cavm observations for GPP and Reco... merge class 1 (barren) and 31 (prostrate shrub)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- ifelse(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged==1, 31, d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)

# not enough tkwp sites for class 3 in 20 km models... merge 3 and 4
d$TKWP_Thermokarst <- ifelse(d$TKWP_Thermokarst==4, 3, d$TKWP_Thermokarst)




# variables as factors
d$TKWP_Thermokarst <- as.factor(d$TKWP_Thermokarst)
d$TKHP_Thermokarst <- as.factor(d$TKHP_Thermokarst)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- as.factor(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
d$forest_age_class_forest_age_sites <- as.factor(d$forest_age_class_forest_age_sites)
d$Study_ID_Short <- as.factor(d$Study_ID_Short)
# (note that if you would use dummy variables those would not need to be factors)





### Set up clusters for parallel processing
cluster <- makeCluster(detectCores()-7) # -7 for other models
registerDoParallel(cluster)
# see the tutorial here: https://rpubs.com/lgreski/improvingCaretPerformance


# TEMPORARILY UNCOMMENTED - DONE

# 
# ### Start looping through the response variables ###
# 
# for (flux in resp_vars) {
#   
#   print(flux)
#   # flux <- "NEE_gC_m2"
#   
#   
#   ### Model parameter inputs for feature (variable) selection and model tuning
#   
#   ### remove NA across columns for 1 and 20 km datasets
#   modeldata2 <- d[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
#   modeldata1 <- na.omit(modeldata2) 
#   sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
#   print("1 km data set:")
#   print(nrow(modeldata1))
#   #check that factors are truly factors
#   print(str(modeldata1))
#   
#   # which sites are left out?
#   dropped <- modeldata2[!(complete.cases(modeldata2)), ]
#   year <- subset(d, select=c(id, Meas_year))
#   dropped <- merge(dropped, year, by="id")
#   print("dropped data from the following sites:")
#   print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
#   
#   
#   modeldata3 <- d[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
#   modeldata2 <- na.omit(modeldata3)
#   sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
#   print("20 km data set:")
#   print(nrow(modeldata2))
#   #check that factors are truly factors
#   print(str(modeldata2))
#   
#   # which sites are left out?
#   dropped <- modeldata3[!(complete.cases(modeldata3)), ]
#   year <- subset(d, select=c(id, Meas_year))
#   dropped <- merge(dropped, year, by="id")
#   print("dropped data from the following sites:")
#   print(unique(dropped$Study_ID_Short))
#   
#   
#   
#   # Create a list of row indices for cross validation splits (for leave-one-fold out) 
#   # leave one site out used because of the highly different number of observations in some factor data levels
#   
#   # 1 km
#   set.seed(448) # so that folds will always be the same
#   folds <- fold(data= modeldata1,
#                 k = length(unique(modeldata1$Study_ID_Short)),
#                 id_col = "Study_ID_Short")
#   
#   # merge
#   folds_data <- subset(folds, select=c( ".folds", "id"))
#   modeldata1 <- full_join(modeldata1, folds_data, by="id")
#   
#   # create a row ID
#   modeldata1$samplerow <- seq(1, length(modeldata1$Study_ID_Short), by=1)
#   
#   indices1 <- list()
#   indices_not1 <- list()
#   
#   for (k in unique(modeldata1$.folds)) {
#     
#     # k = 1
#     subs <- subset(modeldata1, .folds==k)
#     
#     # list the row ids for the fold
#     sites <- list(as.integer(subs$samplerow) )
#     
#     # list other row ids that don't belong to the k fold
#     sites_not <- list(as.integer(modeldata1$samplerow[!(modeldata1$samplerow %in% unlist(sites))]))
#     
#     # check that all the rows are included
#     length(sites_not[[1]]) + length(sites[[1]]) == length(modeldata1$Study_ID_Short)
#     
#     # append to list (caret wants these as lists)
#     # row indices for each fold, used for model evaluation
#     indices1 <- append(indices1, sites)
#     # row indices for model training
#     indices_not1 <- append(indices_not1, sites_not)
#     
#   }
#   
#   
#   names(indices1) <- 1:length(indices1)
#   names(indices_not1) <- 1:length(indices_not1)
#   
#   
#   
#   # 20 km
#   set.seed(448)
#   folds <- groupdata2::fold(data= modeldata2,
#                             k = length(unique(modeldata2$Study_ID_Short)), id_col = "Study_ID_Short")
#   
#   # merge
#   folds_data <- subset(folds, select=c( ".folds", "id"))
#   modeldata2 <- full_join(modeldata2, folds_data, by="id")
#   
#   # create a row ID
#   modeldata2$samplerow <- seq(1, length(modeldata2$Study_ID_Short), by=1)
#   
#   indices2 <- list()
#   indices_not2 <- list()
#   
#   for (k in unique(modeldata2$.folds)) {
#     
#     # k = 1
#     subs <- subset(modeldata2, .folds==k)
#     
#     # list the row ids for the fold
#     sites <- list(as.integer(subs$samplerow) )
#     
#     # list other row ids that don't belong to the k fold
#     sites_not <- list(as.integer(modeldata2$samplerow[!(modeldata2$samplerow %in% unlist(sites))]))
#     
#     # check that all the rows are included
#     length(sites_not[[1]]) + length(sites[[1]]) == length(modeldata2$Study_ID_Short)
#     
#     # append to list (caret wants these as lists)
#     # row indices for each fold, used for model evaluation
#     indices2 <- append(indices2, sites)
#     # row indices for model training
#     indices_not2 <- append(indices_not2, sites_not)
#     
#   }
#   
#   
#   names(indices2) <- 1:length(indices2)
#   names(indices_not2) <- 1:length(indices2)
#   
#   
#   
#   print("model tuning and feature selection starting")
#   
#   
#   # Recursive feature elimination (RFE) is a feature selection method that fits a model and
#   # removes the weakest feature (or features) until the specified number of features is reached.
#   # In the current RFE algorithm, the training data is being used for at least three purposes: predictor selection, model fitting and performance evaluation.
#   # We will give 40 and less predictor options for 1 km models and 35 and less predictor options for 20 km models (just because the 20 km model has a lower number of predictors)
#   
#   
#   # some useful web pages about the parameters:
#   # https://stackoverflow.com/questions/24612824/r-caret-package-error-if-i-specified-index-for-both-rfe-control-and-train-contr
#   # https://github.com/topepo/caret/issues/1019
#   # https://stackoverflow.com/questions/37749009/custom-training-rows-when-using-caret-package
#   # https://stats.stackexchange.com/questions/109340/leave-one-subject-out-cross-validation-in-caret
#   # https://stats.stackexchange.com/questions/214387/results-from-rfe-function-caret-to-compute-average-metrics-r
#   # https://stats.stackexchange.com/questions/323356/index-argument-with-createfolds-in-traincontrol-caret-package
#   
#   # Note that it's important to differentiate between formula and non-formula formulations:
#   # https://stackoverflow.com/questions/22200923/different-results-with-formula-and-non-formula-for-caret-training?fbclid=IwAR0d1bTb07FZzVojdlClemMmgMUhJk02Z_8uGRyKTYM8BBHGLtstJfuicVE
#   # For gbm and rf I'm using the non-formula method without ~) where dummy variables for factors are not created (because trees can handle these in their own way)
#   # But for svm I'm using the formula method, so categorical variables are transformed to dummies
#   
#   
#   
#   
#   
#   ### RF
#   
#   # Control parameters
#   tunecontrol1 <- trainControl(
#     method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
#     verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
#     returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
#     returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
#     # p = 0.7, # For leave-group out cross-validation: the training percentage
#     summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
#     selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
#     index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
#     indexOut = indices1,
#     allowParallel = TRUE, # parallel processing
#     savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
#   # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
#   # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
#   
#   
#   # Includes some options about what kind of information is saved
#   tunecontrol2 <- trainControl(
#     method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
#     verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
#     returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
#     returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
#     # p = 0.7, # For leave-group out cross-validation: the training percentage
#     summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
#     selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
#     index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
#     indexOut = indices2,
#     allowParallel = TRUE, # parallel processing
#     savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
#   
#   
#   
#   ### run the RFE and model tuning algorithm with 1 km predictors
#   set.seed(448)
#   rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
#                   trControl = tunecontrol1, # tuning parameters
#                   method="qrf", importance=TRUE) # modeling method
#   
#   
#   print("1 km rfe and tuning done")
#   
#   ### Write the model out
#   saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_full_model_without_larvaloutbreak",  sep="_"), ".rds"))
#   
#   
#   
#   ### run the RFE and model tuning algorithm with 20 km predictors
#   set.seed(448)
#   rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
#                    trControl = tunecontrol2, # tuning parameters
#                    method="qrf", importance=TRUE) # modeling method
#   
#   
#   
#   print("20 km rfe and tuning done")
#   
#   ### Write the model out
#   saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_full_model_without_larvaloutbreak", sep="_"), ".rds"))
#   
#   
#   print("moving on to next variable")
#   
#   
#   
#   
#   
# }


#stopCluster(cluster)






### Second key models: full dataset without veg type, solar radiation, or only with the top 10 key predictors



## List predictors for the models
# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", # met
                       
                       "tmean_TerraClimate_averages", "ppt_TerraClimate_averages", # climate
                       
                       "trend_20yrprior_terra_change_id",  "terra_trend_19601990", # temperature change - note that the naming convention changed a bit...
                       
                       "ndvi_trend_19812010", # ndvi change trend 
                       
                       "Barrow_CO2_conc_Barrow_CO2conc", # atmos CO2 conc
                       
                       "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                       
                       "NDVI_whittaker_constant_monthly_mean", # Optical RS, dropped several because highly correlated
                       
                       "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", # surface temp
                       
                       "water_ground_MCD43A4_annual_water_ground_sites_low_quality", "water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality", # wetness
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # topo
                       
                       "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass", # c stocks
                       
                       "Percent_NonTree_Vegetation_MOD44B_sites", "Percent_NonVegetated_MOD44B_sites", "Percent_Tree_Cover_MOD44B_sites", # veg cover
                       
                       "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                       "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", # soil variables
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", # Permafrost (static)
                       
                       "TKWP_Thermokarst", "TKHP_Thermokarst", # themokarst vulnerability 
                       
                       "forest_age_class_forest_age_sites" # forest age
                       
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km




# Variables used in 20 km spatial resolution models
Baseline_vars_20km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites",  # tmean included because don't have LST
                        
                        "tmean_TerraClimate_averages", "ppt_TerraClimate_averages", 
                        
                        "trend_20yrprior_terra_change_id",  "terra_trend_19601990", 
                        
                        "ndvi_trend_19812010", # note cannot have ndvi prior to 10 yrs because no data from 1979
                        
                        "Barrow_CO2_conc_Barrow_CO2conc",
                        
                        "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", 
                        
                        "ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled",  
                        
                        "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", 
                        
                        "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                        
                        "Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Percent_TreeCover_AVHRR_VCF5KYR", "Percent_NonVegetated_AVHRR_VCF5KYR", # equivalent to MOD tree cover product
                        
                        "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                        "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent",  
                        
                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", 
                        
                        "TKWP_Thermokarst", "TKHP_Thermokarst", 
                        
                        "forest_age_class_forest_age_sites"
)

# check that the columns exist
Baseline_vars_20km %in% colnames(d)



### Set up clusters for parallel processing
cluster <- makeCluster(detectCores()-7) # -7 for other models
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
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  # Recursive feature elimination (RFE) is a feature selection method that fits a model and
  # removes the weakest feature (or features) until the specified number of features is reached.
  # In the current RFE algorithm, the training data is being used for at least three purposes: predictor selection, model fitting and performance evaluation.
  # We will give 40 and less predictor options for 1 km models and 35 and less predictor options for 20 km models (just because the 20 km model has a lower number of predictors)
  
  
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
  
  
  
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_full_model_without_vegtype",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_full_model_without_vegtype", sep="_"), ".rds"))
  
  
  print("moving on to next model")
  
  
  
  
  
  
  
  
  
  
  ### Same model but without solar radiation
  
  
  Baseline_vars_1km_edited <- c("vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", # met
                         
                         "tmean_TerraClimate_averages", "ppt_TerraClimate_averages", # climate
                         
                         "trend_20yrprior_terra_change_id",  "terra_trend_19601990", # temperature change - note that the naming convention changed a bit...
                         
                         "ndvi_trend_19812010", # ndvi change trend 
                         
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
                         
                         "TKWP_Thermokarst", "TKHP_Thermokarst", # themokarst vulnerability 
                         
                         "forest_age_class_forest_age_sites" # forest age
                         
                         
  )
  
  
  
  
  # Variables used in 20 km spatial resolution models
  Baseline_vars_20km_edited <- c("vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites",  # tmean included because don't have LST
                          
                          "tmean_TerraClimate_averages", "ppt_TerraClimate_averages", 
                          
                          "trend_20yrprior_terra_change_id",  "terra_trend_19601990", 
                          
                          "ndvi_trend_19812010", # note cannot have ndvi prior to 10 yrs because no data from 1979
                          
                          "Barrow_CO2_conc_Barrow_CO2conc",
                          
                          "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", 
                          
                          "ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled",  
                          
                          "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", 
                          
                          "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                          
                          "Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Percent_TreeCover_AVHRR_VCF5KYR", "Percent_NonVegetated_AVHRR_VCF5KYR", # equivalent to MOD tree cover product
                          
                          "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", 
                          
                          "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                          "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent",  
                          
                          "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", 
                          
                          "TKWP_Thermokarst", "TKHP_Thermokarst", 
                          
                          "forest_age_class_forest_age_sites"
  )
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d[,c("Study_ID_Short", "id", flux, Baseline_vars_1km_edited)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d[,c("Study_ID_Short", "id", flux, Baseline_vars_20km_edited)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  # Recursive feature elimination (RFE) is a feature selection method that fits a model and
  # removes the weakest feature (or features) until the specified number of features is reached.
  # In the current RFE algorithm, the training data is being used for at least three purposes: predictor selection, model fitting and performance evaluation.
  # We will give 40 and less predictor options for 1 km models and 35 and less predictor options for 20 km models (just because the 20 km model has a lower number of predictors)
  
  
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
  
  
  
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km_edited], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_full_model_without_srad",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km_edited], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_full_model_without_srad", sep="_"), ".rds"))
  
  
  
  
  
  
  ### Same model but with only 10 key predictors
  
  
  Baseline_vars_1km_edited <- c("vpd_terraclimate_sites",  # met
                                
                                "trend_20yrprior_terra_change_id",  "terra_trend_19601990", # temperature change - note that the naming convention changed a bit...
                                
                                "ndvi_trend_19812010", # ndvi change trend 
                                
                                "Barrow_CO2_conc_Barrow_CO2conc", # atmos CO2 conc
                                
                                "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                                
                                "NDVI_whittaker_constant_monthly_mean", # Optical RS, dropped several because highly correlated
                                
                                "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", # surface temp
                                
                                "water_ground_MCD43A4_annual_water_ground_sites_low_quality", "water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality", # wetness
                                
                                "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # topo
                                
                                "aboveground_biomass_carbon_2010_Above_belowground_biomass",  # c stocks
                                
                                "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", # veg type
                                
                                "PHIHOX_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                                "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent" # soil variables
                               
                                
  )
  
  
  
  
  # Variables used in 20 km spatial resolution models
  Baseline_vars_20km_edited <- c("vpd_terraclimate_sites",  "tmean_terraclimate_sites",  # tmean included because don't have LST
            
                                 
                                 "trend_20yrprior_terra_change_id",  "terra_trend_19601990", 
                                 
                                 "ndvi_trend_19812010", # note cannot have ndvi prior to 10 yrs because no data from 1979
                                 
                                 "Barrow_CO2_conc_Barrow_CO2conc",
                                 
                                  "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", 
                                 
                                 "ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled",  
                                 
                                 "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", 
                                 
                                 "aboveground_biomass_carbon_2010_Above_belowground_biomass", 
                                 
                                 "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", 
                                 
                                 "PHIHOX_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                                 "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent"  

  )
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d[,c("Study_ID_Short", "id", flux, Baseline_vars_1km_edited)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d[,c("Study_ID_Short", "id", flux, Baseline_vars_20km_edited)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  # Recursive feature elimination (RFE) is a feature selection method that fits a model and
  # removes the weakest feature (or features) until the specified number of features is reached.
  # In the current RFE algorithm, the training data is being used for at least three purposes: predictor selection, model fitting and performance evaluation.
  # We will give 40 and less predictor options for 1 km models and 35 and less predictor options for 20 km models (just because the 20 km model has a lower number of predictors)
  
  
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
  
  
  
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km_edited], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_full_model_predictor_subset",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km_edited], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_full_model_predictor_subset", sep="_"), ".rds"))
  
  
  
  
  
}


#stopCluster(cluster)













### Remaining data subsets


# data subsets: model training data 
#  factor variables as predictors in these data subsets are tricky because each time some of the class levels will be lost -> better to run these without any categorical data
## DISTURBANCE SUBSETS
# 1) without larval outbreaks: removing only Lund_Kobbefjord_Ch because of extremely high uptake values. 
# Would like to keep L\xf3pez-Blanco_GL-NuF_tower1 because the flux values seem to make more sense (landscape average) but it does not have MODIS NDVI data -> will be dropped in 1 km model tuning too 
# Run this with and without the categorical data
# 2) without any disturbed sites - maybe doesn't make sense to add disturbance predictors then either
# 3) undisturbed + fire
# 4) undisturbed + harvest
# 5) undisturbed + thermokarst
# no need to consider drought (observations will not be included in the model) or drainage (old effect, not recent permafrost thaw related or something similar) here

## METHODOLOGICAL SUBSETS
# 1) remove daily chamber measurements because sites that have only daytime measurements have larger sink strength (note that this removes also Lund_Kobbefjord)
# 2) eddy covariance data only
# 3) eddy covariance data partitioned using daytime partitioning
# 4) same but with night-time
# 5) Keep only rows that have measurements of all three fluxes (FLUXCOM)


### An example of the different missing levels
### number of sites and obs in the different levels of the categorical variables

## No disturbances
d2 <- subset(d, Disturbance=="No" | is.na(Disturbance))

## 1 km models
# NEE
modeldata1 <- d2[,c("Study_ID_Short", "id", "NEE_gC_m2", Baseline_vars_1km)]
modeldata1 <- na.omit(modeldata1) 
unique(modeldata1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
unique(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
modeldata1 %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# not enough data for veg type 30 or 90

unique(modeldata1$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
modeldata1 %>% group_by(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# no data for fire class 1

unique(modeldata1$TKWP_Thermokarst)
modeldata1 %>% group_by(TKWP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# only one site for class 3


unique(modeldata1$TKHP_Thermokarst)
modeldata1 %>% group_by(TKHP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$forest_age_class_forest_age_sites)
modeldata1 %>% group_by(forest_age_class_forest_age_sites) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# only one site for class 1


# GPP
modeldata1 <- d2[,c("Study_ID_Short", "id", "GPP_gC_m2", Baseline_vars_1km)]
modeldata1 <- na.omit(modeldata1) 
unique(modeldata1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
unique(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) 
modeldata1 %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# no data from 1 and 30 

unique(modeldata1$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
modeldata1 %>% group_by(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# class 1 missing

unique(modeldata1$TKWP_Thermokarst)
modeldata1 %>% group_by(TKWP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# class 3 missing 

unique(modeldata1$TKHP_Thermokarst)
modeldata1 %>% group_by(TKHP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$forest_age_class_forest_age_sites)
modeldata1 %>% group_by(forest_age_class_forest_age_sites) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# only one site for class 1






# Reco
modeldata1 <- d2[,c("Study_ID_Short", "id", "Reco_gC_m2", Baseline_vars_1km)]
modeldata1 <- na.omit(modeldata1) 
unique(modeldata1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) 
unique(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) 
modeldata1 %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# barren group missing!!

unique(modeldata1$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
modeldata1 %>% group_by(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$TKWP_Thermokarst)
modeldata1 %>% group_by(TKWP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$TKHP_Thermokarst)
modeldata1 %>% group_by(TKHP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$forest_age_class_forest_age_sites)
modeldata1 %>% group_by(forest_age_class_forest_age_sites) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))







## 20 km models
# NEE
modeldata1 <- d2[,c("Study_ID_Short", "id", "NEE_gC_m2", Baseline_vars_20km)]
modeldata1 <- na.omit(modeldata1) 
unique(modeldata1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
unique(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
modeldata1 %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))


unique(modeldata1$Number_of_days_since_fire_classes_gfed_monthly_calc)
modeldata1 %>% group_by(Number_of_days_since_fire_classes_gfed_monthly_calc) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$TKWP_Thermokarst)
modeldata1 %>% group_by(TKWP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# only one site in class 3

unique(modeldata1$TKHP_Thermokarst)
modeldata1 %>% group_by(TKHP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$forest_age_class_forest_age_sites)
modeldata1 %>% group_by(forest_age_class_forest_age_sites) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))


# GPP
modeldata1 <- d2[,c("Study_ID_Short", "id", "GPP_gC_m2", Baseline_vars_20km)]
modeldata1 <- na.omit(modeldata1) 
unique(modeldata1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
unique(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) 
modeldata1 %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# barren group missing!!

unique(modeldata1$Number_of_days_since_fire_classes_gfed_monthly_calc)
modeldata1 %>% group_by(Number_of_days_since_fire_classes_gfed_monthly_calc) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$TKWP_Thermokarst)
modeldata1 %>% group_by(TKWP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# nothing from group 3

unique(modeldata1$TKHP_Thermokarst)
modeldata1 %>% group_by(TKHP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$forest_age_class_forest_age_sites)
modeldata1 %>% group_by(forest_age_class_forest_age_sites) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))






# Reco
modeldata1 <- d2[,c("Study_ID_Short", "id", "Reco_gC_m2", Baseline_vars_20km)]
modeldata1 <- na.omit(modeldata1) 
unique(modeldata1$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) 
unique(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) 
modeldata1 %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))
# barren group missing!!

unique(modeldata1$Number_of_days_since_fire_classes_gfed_monthly_calc)
modeldata1 %>% group_by(Number_of_days_since_fire_classes_gfed_monthly_calc) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$TKWP_Thermokarst)
modeldata1 %>% group_by(TKWP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$TKHP_Thermokarst)
modeldata1 %>% group_by(TKHP_Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))

unique(modeldata1$forest_age_class_forest_age_sites)
modeldata1 %>% group_by(forest_age_class_forest_age_sites) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Short)))





### Data subsets
# 1) without larval outbreaks: removing only Lund_Kobbefjord_Ch because of extremely high uptake values. 
# Would like to keep L\xf3pez-Blanco_GL-NuF_tower1 because the flux values seem to make more sense (landscape average) but it does not have MODIS NDVI data -> will be dropped in 1 km model tuning too 
d1 <- subset(d, Study_ID_Short!="Lund_Kobbefjord_Ch")


# 2) without any disturbed sites - maybe doesn't make sense to add disturbance predictors then either? So remove these:
# "trend_20yrprior_terra_change_id",  "terra_trend_19601990", "ndvi_trend_19812010", 
# "Number_of_days_since_fire_classes_gfed_monthly_calc", "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned",  "TKWP_Thermokarst", "TKHP_Thermokarst", 
d2 <- subset(d, Disturbance=="No" | is.na(Disturbance))

# 3) undisturbed + fire
d3 <- subset(d, Disturbance=="No" | is.na(Disturbance) | Disturbance=="Fire")

# 4) undisturbed + harvest
d4 <- subset(d, Disturbance=="No" | is.na(Disturbance) | Disturbance=="Harvest")


# 5) undisturbed + thermokarst
d5 <- subset(d, Disturbance=="No" | is.na(Disturbance) | Disturbance=="Thermokarst")

# no need to consider drought (observations will not be included in the model) or drainage (old effect, not recent permafrost thaw related or something similar) here

## METHODOLOGICAL SUBSETS
# 1) remove daily chamber measurements because sites that have only daytime measurements have larger sink strength (note that this removes also Lund_Kobbefjord)
dm1 <- subset(d, Diurnal_coverage=="Day and Night")

# 2) eddy covariance data only
dm2 <- subset(d, Flux_method=="EC")

# 3) ec partitioning but with night-time Reichstein method - doesn't make sense to use LAsslop because only 189 observations
dm3 <- subset(d, Partition_method=="Reichstein")

# 4) Keep only rows that have measurements of all three fluxes (FLUXCOM)
dm4 <- subset(d, !is.na(NEE_gC_m2) &  !is.na(GPP_gC_m2) & !is.na(Reco_gC_m2))

# 5) Remove larval outbreak, and remove srad from predictors in the code
dm5 <- subset(d, Study_ID_Short!="Lund_Kobbefjord_Ch")

## FULL DATA WITHOUT FACTOR VARIABLES
data.list <- list(d1, d2, d3, d4, d5, dm1, dm2, dm3, dm4)





## List predictors for the models
# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", # met
                       
                       "tmean_TerraClimate_averages", "ppt_TerraClimate_averages", # climate
                       
                       "trend_20yrprior_terra_change_id",  "terra_trend_19601990", # temperature change - note that the naming convention changed a bit...
                       
                       "ndvi_trend_19812010", # ndvi change trend 
                       
                       "Barrow_CO2_conc_Barrow_CO2conc", # atmos CO2 conc
                       
                       "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                       
                       "NDVI_whittaker_constant_monthly_mean", # Optical RS, dropped several because highly correlated
                       
                       "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", # surface temp
                       
                       "water_ground_MCD43A4_annual_water_ground_sites_low_quality", "water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality", # wetness
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # topo
                       
                       "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass", # c stocks
                       
                       "Percent_NonTree_Vegetation_MOD44B_sites", "Percent_NonVegetated_MOD44B_sites", "Percent_Tree_Cover_MOD44B_sites", # veg cover
                       
                       "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                       "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", # soil variables
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH" # Permafrost (static)
                       
                       
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km




# Variables used in 20 km spatial resolution models
Baseline_vars_20km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites",  # tmean included because don't have LST
                        
                        "tmean_TerraClimate_averages", "ppt_TerraClimate_averages", 
                        
                        "trend_20yrprior_terra_change_id",  "terra_trend_19601990", 
                        
                        "ndvi_trend_19812010", # note cannot have ndvi prior to 10 yrs because no data from 1979
                        
                        "Barrow_CO2_conc_Barrow_CO2conc",
                        
                        "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", 
                        
                        "ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled",  
                        
                        "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", 
                        
                        "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                        
                        "Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Percent_TreeCover_AVHRR_VCF5KYR", "Percent_NonVegetated_AVHRR_VCF5KYR", # equivalent to MOD tree cover product
                        
                        "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                        "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent",  
                        
                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH" 
                        
)

# check that the columns exist
Baseline_vars_20km %in% colnames(d)




# ### Set up clusters for parallel processing
# cluster <- makeCluster(detectCores()-7) # -7 for other models
# registerDoParallel(cluster)
# # see the tutorial here: https://rpubs.com/lgreski/improvingCaretPerformance



# Study_ID_Short as factor
d$Study_ID_Short <- factor(d$Study_ID_Short)


### Start looping through the response variables ###

for (flux in resp_vars) {
  
  print(flux)
  # flux <- "NEE_gC_m2"
  
  ### SUBSET 1: without larval outbreaks: removing only Lund_Kobbefjord_Ch because of extremely high uptake values.  ###
  ##############
  # this is now removed from all the other data subsets too
  
  d2 <- subset(d, Study_ID_Short!="Lund_Kobbefjord_Ch")
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  # Recursive feature elimination (RFE) is a feature selection method that fits a model and
  # removes the weakest feature (or features) until the specified number of features is reached.
  # In the current RFE algorithm, the training data is being used for at least three purposes: predictor selection, model fitting and performance evaluation.
  # We will give 40 and less predictor options for 1 km models and 35 and less predictor options for 20 km models (just because the 20 km model has a lower number of predictors)
  
  
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
  
  
  
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_nofactors_model_without_larvaloutbreak",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_nofactors_model_without_larvaloutbreak", sep="_"), ".rds"))
  
  
  print("moving on to next subset")
  
  
  
  
  
  
  
  
  ### SUBSET 2: without any disturbed sites   ###
  ##############
  
  d2 <- subset(d, Disturbance=="No" | is.na(Disturbance))
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_nofactors_model_without_disturbedsites",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_nofactors_model_without_disturbedsites", sep="_"), ".rds"))
  
  
  print("moving on to next subset")
  
  
  
  
  
  
  ### SUBSET 3: undisturbed+fire   ###
  ##############
  
  d2 <- subset(d, Disturbance=="No" | is.na(Disturbance) | Disturbance=="Fire")
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_nofactors_model_without_disturbedsites_plusfire",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_nofactors_model_without_disturbedsites_plusfire", sep="_"), ".rds"))
  
  
  print("moving on to next subset")
  
  
  
  
  
  
  ### SUBSET 4: undisturbed+harvest   ###
  ##############
  
  d2 <- subset(d, Disturbance=="No" | is.na(Disturbance) | Disturbance=="Harvest")
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_nofactors_model_without_disturbedsites_plusharvest",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_nofactors_model_without_disturbedsites_plusharvest", sep="_"), ".rds"))
  
  
  print("moving on to next subset")
  
  
  
  
  
  
  
  ### SUBSET 4: undisturbed+thermokarst   ###
  ##############
  
  d2 <- subset(d, Disturbance=="No" | is.na(Disturbance) | Disturbance=="Thermokarst")
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_nofactors_model_without_disturbedsites_plusthermokarst",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_nofactors_model_without_disturbedsites_plusthermokarst", sep="_"), ".rds"))
  
  
  print("moving on to next subset")
  
  
  
  
  
  
  ### SUBSET METHOD 1: remove daily chamber measurements because sites that have only daytime measurements have larger sink strength (note that this removes also Lund_Kobbefjord)   ###
  ##############
  
  d2 <- subset(d, Diurnal_coverage=="Day and Night")
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_nofactors_model_without_daytimechambermeas",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_nofactors_model_without_daytimechambermeas", sep="_"), ".rds"))
  
  
  print("moving on to next subset")
  
  
  
  
  
  
  
  
  
  ### SUBSET METHOD 2: rwith eddy covariance data only   ###
  ##############
  
  d2 <- subset(d, Flux_method=="EC")
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_nofactors_model_with_econly",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_nofactors_model_with_econly", sep="_"), ".rds"))
  
  
  print("moving on to next subset")
  
  
  
  
  
  
  
  
  
  ### SUBSET METHOD 3: with eddy covariance data partitioned with Reichstein only   ###
  ##############
  
  d2 <- subset(d, Partition_method=="Reichstein")
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_nofactors_model_with_reichsteinonly",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_nofactors_model_with_reichsteinonly", sep="_"), ".rds"))
  
  
  print("moving on to next subset")
  
  
  
  
  
  
  
  ### SUBSET METHOD 4: with rows that have all GPP, Reco, and NEE observations   ###
  ##############
  
  d2 <- subset(d, !is.na(NEE_gC_m2) &  !is.na(GPP_gC_m2) & !is.na(Reco_gC_m2))
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) 
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d2[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d2, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_nofactors_model_with_datafromallfluxes",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_nofactors_model_datafromallfluxes", sep="_"), ".rds"))
  
  
  print("moving on to next subset")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}


#stopCluster(cluster)





### Another subset: include key predictor variables except veg type and aggregate those and fluxes to annual averages (predictors) and annual cumulative fluxes (response)



### Load modeling data
setwd("/home/master/abcflux_modeling/src")
d <- read.csv("/home/master/flux_upscaling_data/results/final/modeldata_avg.csv")


# remove lund
d <- subset(d, Study_ID_Short!="Lund_Kobbefjord_Ch")

d <- subset(d, Flux_method=="EC")


Baseline_vars_1km <- c("vpd_terraclimate_sites",  # met
                       
                       "trend_20yrprior_terra_change_id",  "terra_trend_19601990", # temperature change - note that the naming convention changed a bit...
                       
                       "ndvi_trend_19812010", # ndvi change trend 
                       
                       "Barrow_CO2_conc_Barrow_CO2conc", # atmos CO2 conc
                       
                       "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                       
                       "NDVI_whittaker_constant_monthly_mean", # Optical RS, dropped several because highly correlated
                       
                       "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", # surface temp
                       
                       "water_ground_MCD43A4_annual_water_ground_sites_low_quality", "water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality", # wetness
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # topo
                       
                       "aboveground_biomass_carbon_2010_Above_belowground_biomass",  # c stocks
                       
                       "PHIHOX_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                       "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent" # soil variables
                              
                              
                              
                              
)




# Variables used in 20 km spatial resolution models
Baseline_vars_20km <- c("vpd_terraclimate_sites",  "tmean_terraclimate_sites",  # tmean included because don't have LST
                               
                               
                               "trend_20yrprior_terra_change_id",  "terra_trend_19601990", 
                               
                               "ndvi_trend_19812010", # note cannot have ndvi prior to 10 yrs because no data from 1979
                               
                               "Barrow_CO2_conc_Barrow_CO2conc",
                               
                               "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", 
                               
                               "ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled",  
                               
                               "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", 
                               
                               "aboveground_biomass_carbon_2010_Above_belowground_biomass", 
                               
                               "PHIHOX_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                               "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent"  
                               
)



d <- d %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2),
                                                            vpd_terraclimate_sites=mean(vpd_terraclimate_sites),
                                                            trend_20yrprior_terra_change_id=mean(trend_20yrprior_terra_change_id),
                                                            terra_trend_19601990=mean(terra_trend_19601990),
                                                            ndvi_trend_19812010=mean(ndvi_trend_19812010),
                                                             tmean_terraclimate_sites=mean(tmean_terraclimate_sites), Barrow_CO2_conc_Barrow_CO2conc=mean(Barrow_CO2_conc_Barrow_CO2conc),
                                                            Soil.temperature.level.1_era5_soilmoist_temp_snow=mean(Soil.temperature.level.1_era5_soilmoist_temp_snow),
                                                             Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow=mean(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow), NDVI_whittaker_constant_monthly_mean=mean(NDVI_whittaker_constant_monthly_mean),
                                                             ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled=mean(ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled),
                                                             dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m=unique(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m), dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m=unique(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m), aboveground_biomass_carbon_2010_Above_belowground_biomass=unique(aboveground_biomass_carbon_2010_Above_belowground_biomass),
                                                             
                                                             SoilGrids_SOC_SoilGrids_SOCstock=mean(SoilGrids_SOC_SoilGrids_SOCstock), PHIHOX_M_sl1_250m_ll_SoilGrids=mean(PHIHOX_M_sl1_250m_ll_SoilGrids) , sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent=mean(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)) %>% filter(n==12)

d$id <- seq(1, nrow(d), by=1)





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
  #check that factors are truly factors
  print(str(modeldata1))
  
  # which sites are left out?
  dropped <- modeldata2[!(complete.cases(modeldata2)), ]
  year <- subset(d, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short)) # related to NDVI data, soil data, MOD44b data, and ERA5 land data gaps
  
  
  modeldata3 <- d[,c("Study_ID_Short", "id", flux, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata3)
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  #check that factors are truly factors
  print(str(modeldata2))
  
  # which sites are left out?
  dropped <- modeldata3[!(complete.cases(modeldata3)), ]
  year <- subset(d, select=c(id, Meas_year))
  dropped <- merge(dropped, year, by="id")
  print("dropped data from the following sites:")
  print(unique(dropped$Study_ID_Short))
  
  
  
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
  
  
  
  print("model tuning and feature selection starting")
  
  
  # Recursive feature elimination (RFE) is a feature selection method that fits a model and
  # removes the weakest feature (or features) until the specified number of features is reached.
  # In the current RFE algorithm, the training data is being used for at least three purposes: predictor selection, model fitting and performance evaluation.
  # We will give 40 and less predictor options for 1 km models and 35 and less predictor options for 20 km models (just because the 20 km model has a lower number of predictors)
  
  
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
  
  
  
  
  
  ### RF
  
  # Control parameters
  tunecontrol1 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not1, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices1,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  set.seed(448)
  rfe_fit = train(modeldata1[,Baseline_vars_1km], modeldata1[,flux],
                  trControl = tunecontrol1, # tuning parameters
                  method="qrf", importance=TRUE) # modeling method
  
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/home/master/abcflux_modeling/results/", paste(flux, "1km_qrf_train_loocv_keypredictors_annual",  sep="_"), ".rds"))
  
  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                   trControl = tunecontrol2, # tuning parameters
                   method="qrf", importance=TRUE) # modeling method
  
  
  
  print("20 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_keypredictors_annual", sep="_"), ".rds"))
  
  
  print("moving on to next variable")
  
  
  
  
  
}


#stopCluster(cluster)


