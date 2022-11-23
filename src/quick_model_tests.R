




############### ADD LONGITUDE



.libPaths("/home/master/R/x86_64-pc-linux-gnu-library/4.2")


library("dplyr")
library("caret")
library("parallel")
library("doParallel")
library("randomForest")
#install.packages("quantregForest", lib="D:/packages")  
library("quantregForest")  
library("groupdata2")  
library("terra")


### Load modeling data
d <- read.csv("/home/master/flux_upscaling_data/results/final/modeldata_avg.csv")



### Check variable distributions - this needs to be done with all subsets
hist(d$NEE_gC_m2)
hist(d$GPP_gC_m2)
hist(d$Reco_gC_m2)



### Extract longitude
sp <- vect(d, geom=c("Longitude", "Latitude"), "+proj=longlat +datum=WGS84")
crs(sp)
r <- rast("/home/master/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif")
sp <- project(sp, r)
test <- as.data.frame(sp[,2], geom="XY")

d <- data.frame(cbind(d, test[,2:3]))
# y the correct one

# # just in case, extract 
# rr <- rast("/home/master/predictors_8km/latitude.tif")
# check <- extract(rr, sp)
# plot(check$V3, d$y)


### Response variables
resp_vars <- c("NEE_gC_m2") 


### Predictors


## List predictors for the models


# Variables used in 20 km spatial resolution models
Baseline_vars_20km <- c("y", "srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites",  # tmean included because don't have LST
                        
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










### Factor edits

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











# ### Set up clusters for parallel processing
cluster <- makeCluster(detectCores()-7) # -7 for other models
registerDoParallel(cluster)
# see the tutorial here: https://rpubs.com/lgreski/improvingCaretPerformance



### Start looping through the response variables ###

for (flux in resp_vars) {
  
  print(flux)
  # flux <- "NEE_gC_m2"
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  
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
    # row indices for model caret::training
    indices_not2 <- append(indices_not2, sites_not)
    
  }
  
  
  names(indices2) <- 1:length(indices2)
  names(indices_not2) <- 1:length(indices2)
  
  
  
  print("model tuning starting")
  
  
  
  ### RF
  
  # Control parameters
  
  # Includes some options about what kind of information is saved
  tunecontrol2 <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does leave-site/group-out CV
    verboseIter = FALSE,  # A logical for printing a caret::training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the caret::training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples.
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not2, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices2,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none".
  
  
  
  
  

  
  
  ### run the RFE and model tuning algorithm with 20 km predictors
  set.seed(448)
  rfe_fit2 = caret::train(modeldata2[,Baseline_vars_20km], modeldata2[,flux],
                          trControl = tunecontrol2, # tuning parameters
                          method="qrf",importance = TRUE) # modeling method
  
  
  print("20 km caret::train and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit2, paste0("/home/master/abcflux_modeling/results/", paste(flux, "20km_qrf_train_loocv_with_lat", sep="_"), ".rds"))
  
  
  print("moving on to next variable")
  
  
  
  
  
}


stopCluster(cluster)