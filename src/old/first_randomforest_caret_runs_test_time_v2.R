

### This script trains the model for one flux response variable, 
# and is based on xgboost, 50 model training observations and 3 different predictor amounts in feature selection scheme (the best predictors for models with 70, 60, and 50 predictors estimated)
# to check how much it takes to run it
# then the final model runs can be muliplied by ~200 (number of additional splits in CV) and number of different predictor selections (~20)


library("dplyr")
library("caret")
library("xgboost")
library("parallel")
library("doParallel")
library("ranger")

# install groupdata
#library("groupdata2")
# If the directory doesn't exist. You will need to make sure you have permissions to create the directory
# install and specify the created directory
install.packages("groupdata2", lib="/mnt/data1/boreal/avirkkala/packages") 
# use library while adding path to the Rlib path.
library("groupdata2", lib.loc="/mnt/data1/boreal/avirkkala/packages")  


setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")

### Check variable distributions - this needs to be done with all subsets
hist(d$NEE_gC_m2)
hist(d$GPP_gC_m2)
hist(d$Reco_gC_m2)

### And counts in groups
unique(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged)
d %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean)))
d %>% filter(!is.na(GPP_gC_m2)) %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean))) # nothing from barren!
d %>% filter(!is.na(Reco_gC_m2)) %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean)))
d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean)))


# NOTE NOTE! Barren obs missing from GPP, and only one site in Reco - > merge with P
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged <- ifelse(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged==1, 31, d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged)


d %>% group_by(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean)))
d %>% filter(!is.na(GPP_gC_m2)) %>% group_by(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean))) 
d %>% filter(!is.na(Reco_gC_m2)) %>% group_by(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean)))
d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean)))
# enough classes!

d %>% group_by(Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean)))
d %>% filter(!is.na(GPP_gC_m2)) %>% group_by(Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean))) 
d %>% filter(!is.na(Reco_gC_m2)) %>% group_by(Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean)))
d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(Thermokarst) %>% dplyr::summarize(n=n(), nsite=length(unique(Study_ID_Clean)))
unique(d$Thermokarst)
# enough classes!

# There are also some NAs in the variables. NAs are handled in different ways across ML methods: https://stats.stackexchange.com/questions/144922/r-caret-and-nas
# but xgboost can handle NAs - I tested this - so no need to worry about this for now


### Factor conversions
# note that randomForest and svmRadial can handle variables coded as "as.factor"
# but xgboost cannot: https://github.com/dmlc/xgboost/issues/95
# so we will need to do one-hot encoding

# Thermokarst
d <- as.data.frame(d)
d$Thermokarst <- factor(d$Thermokarst)
onehot <- model.matrix(~0+d[, 'Thermokarst'])
attr(onehot, "dimnames")[[2]] <- paste("Thermokarst", levels(d$Thermokarst), sep="_")
d <- cbind(d, onehot)

# land cover
# first change NA in ESA CCI to something else
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged <- ifelse(is.na(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged), "0000", d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged <- factor(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged)
onehot <- model.matrix(~0+d[, 'ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged'])
attr(onehot, "dimnames")[[2]] <- paste("ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged", levels(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged), sep="_")
d <- cbind(d, onehot)

# Fire burn classes
d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- factor(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
onehot <- model.matrix(~0+d[, 'Number_of_days_since_fire_classes_MCD64A1_sites_cleaned'])
attr(onehot, "dimnames")[[2]] <- paste("Number_of_days_since_fire_classes_MCD64A1_sites_cleaned", levels(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned), sep="_")
d <- cbind(d, onehot)


### Response variables
resp_vars <- c("NEE_gC_m2") 


### Predictors
names(d)

## List predictors for the models (including the one-hot encoded factors)
# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites", "swe_terraclimate_sites", # climate
                       
                       "tmean_TerraClimate_averages", "ppt_TerraClimate_averages",  # don't have all of these:  "swe_TerraClimate_averages", "srad_TerraClimate_averages", "vpd_TerraClimate_averages", "pdsi_TerraClimate_averages", 
                       
                       "trend_20yrprior_terra_change_id", # temperature change trend
 
                       "value_era5_soilmoisture", # soil moist
                       
                       "NDVI_MOD13A2v006_NDVI_EVI_sites_low_quality", "EVI_MOD13A2v006_NDVI_EVI_sites_low_quality", "Fpar_MCD15A3Hv006_FPAR_LAI_sites_low_quality", "Lai_MCD15A3Hv006_FPAR_LAI_sites_low_quality",# Optical RS
                       
                       "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", "LST_Night_1km_MOD11A2v006_LST_Night_sites_low_quality", "NDSI_Snow_Cover_MOD10A1_SnowCover_sites_lower_quality",
                       
                       "Albedo_WSA_shortwave_MCD43A3v006_Albedo_WSA_shortwave_sites_low_quality", 
                       
                       "Nadir_Reflectance_Band1_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band2_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band3_MCD43A4_reflectance_sites_low_quality",
                       "Nadir_Reflectance_Band4_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band5_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band6_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band7_MCD43A4_reflectance_sites_low_quality", 
                       
                       "brightness_MCD43A4_tasseledcap_sites_low_quality","greenness_MCD43A4_tasseledcap_sites_low_quality","wetness_MCD43A4_tasseledcap_sites_low_quality",
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
                       
                       "ABoVE_fractional_water_ABoVE_fractional_water", "AMSR6km_thaw_days_NTSG_FT_AMSR_6km", #microwave
                       
                       "Percent_Tree_Cover_MOD44B_sites", "Percent_NonVegetated_MOD44B_sites", #vegetation type/fraction
                       
                       
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_0000", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_120", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_160", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_21", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_21", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_30", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_31", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_33", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_41", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_60", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_70", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_80", 
                       
                       "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                       
                       "BLDFIE_M_250m_ll_30_agg_SoilGrids", "PHIHOX_M_250m_ll_30_agg_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", "wtd_Water_table_depth", 
                       "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent_30_agg", # check soilgrids water and water balance and water table depth
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", "PFR_ESA_CCI_Permafrostv2", "ALT_ESA_CCI_Permafrostv2", # Permafrost
                       
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_0",   # Disturbance
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_1",
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_2",
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_2",
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_3",
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_4",
                       
                       
                       "Thermokarst_0", # Disturbance 
                       "Thermokarst_10",
                       "Thermokarst_11",
                       "Thermokarst_12",
                       "Thermokarst_5",
                       "Thermokarst_6",
                       "Thermokarst_7",
                       "Thermokarst_8",
                       "Thermokarst_9"
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km



# Variables used in 20 km spatial resolution models
Baseline_vars_20km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites", "swe_terraclimate_sites", # climate
                       
                       "tmean_TerraClimate_averages", "ppt_TerraClimate_averages",  # don't have all of these! "swe_TerraClimate_averages", "srad_TerraClimate_averages", "vpd_TerraClimate_averages", "pdsi_TerraClimate_averages", 
                       
                       "trend_20yrprior_terra_change_id", # temperature change trend
                       
                       #ERA5 soil moist
                       "value_era5_soilmoisture",
                       
                       "ndvi3g_mean_GIMMS3g_NDVI_sites_high_and_low_quality", "lai3g_mean_LAI3g_sites_high_and_low_quality", "fpar3g_mean_FPAR3g_sites_high_and_low_quality",# Optical RS
                       
                       "vod_kuband_mean_vod_kuband_sites",
                       
                       "GlobSnow3_SWE_GlobSnow3_snowcover",
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
                       
                      "AMSR_thaw_days_NTSG_FT_AMSR_25km", #microwave
                       
                       "Percent_TreeCover_AVHRR_VCF5KYR", "Percent_NonVegetated_AVHRR_VCF5KYR", #vegetation type/fraction
                       
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_0000", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_120", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_160", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_21", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_21", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_30", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_31", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_33", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_41", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_60", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_70", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_80", 
                       
                       "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                       
                       "BLDFIE_M_250m_ll_30_agg_SoilGrids", "PHIHOX_M_250m_ll_30_agg_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", "wtd_Water_table_depth", 
                       "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent_30_agg", # check soilgrids water and water balance and water table depth
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", "PFR_ESA_CCI_Permafrostv2", "ALT_ESA_CCI_Permafrostv2", # Permafrost
                       
                       # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_0",   # Disturbance
                       # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_1",
                       # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_2",
                       # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_2",
                       # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_3",
                       # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_4",
                       
                       
                       "Thermokarst_0", # Disturbance 
                       "Thermokarst_10",
                       "Thermokarst_11",
                       "Thermokarst_12",
                       "Thermokarst_5",
                       "Thermokarst_6",
                       "Thermokarst_7",
                       "Thermokarst_8",
                       "Thermokarst_9"
                       
)

# check that the columns exist
Baseline_vars_20km %in% colnames(d)




# # List of predictors where factors not one-hot encoded (just traditional as.factor variables)
# Baseline_vars_1km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites", "swe_terraclimate_sites", # climate
#                        
#                        "tmean_TerraClimate_averages", "ppt_TerraClimate_averages",  # don't have all of these! "swe_TerraClimate_averages", "srad_TerraClimate_averages", "vpd_TerraClimate_averages", "pdsi_TerraClimate_averages", 
#                        
#                        "trend_20yrprior_terra_change_id", # temperature change trend
#                        
#                        #ERA5 soil moist
#                        "value_era5_soilmoisture",
#                        
#                        "NDVI_MOD13A2v006_NDVI_EVI_sites_low_quality", "EVI_MOD13A2v006_NDVI_EVI_sites_low_quality", "Fpar_MCD15A3Hv006_FPAR_LAI_sites_low_quality", "Lai_MCD15A3Hv006_FPAR_LAI_sites_low_quality",# Optical RS
#                        
#                        "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", "LST_Night_1km_MOD11A2v006_LST_Night_sites_low_quality", "NDSI_Snow_Cover_MOD10A1_SnowCover_sites_lower_quality",
#                        
#                        "Albedo_WSA_shortwave_MCD43A3v006_Albedo_WSA_shortwave_sites_low_quality", 
#                        
#                        "Nadir_Reflectance_Band1_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band2_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band3_MCD43A4_reflectance_sites_low_quality",
#                        "Nadir_Reflectance_Band4_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band5_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band6_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band7_MCD43A4_reflectance_sites_low_quality", 
#                        
#                        "brightness_MCD43A4_tasseledcap_sites_low_quality","greenness_MCD43A4_tasseledcap_sites_low_quality","wetness_MCD43A4_tasseledcap_sites_low_quality",
#                        
#                        "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
#                        
#                        "ABoVE_fractional_water_ABoVE_fractional_water", "AMSR6km_thaw_days_NTSG_FT_AMSR_6km", #microwave
#                        
#                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged", "Percent_Tree_Cover_MOD44B_sites", "Percent_NonVegetated_MOD44B_sites", #vegetation type/fraction
#                        
#                        "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
#                        
#                        "BLDFIE_M_250m_ll_30_agg_SoilGrids", "PHIHOX_M_250m_ll_30_agg_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", "wtd_Water_table_depth", 
#                        "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent_30_agg", # check soilgrids water and water balance and water table depth
#                        
#                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", "PFR_ESA_CCI_Permafrostv2", "ALT_ESA_CCI_Permafrostv2", # Permafrost
#                        
#                        "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned", "Thermokarst" # Disturbance 
#                        
# )
# 
# # check that the columns exist
# Baseline_vars_1km %in% colnames(d)
# Baseline_vars_1km


### Set up clusters for parallel processing 
cluster <- makeCluster(detectCores()) ### IN RSTUDIO USE detectCores() - 4 THIS, but in Kubernetes you can just use all the 7 cores that you can
registerDoParallel(cluster)
# see the tutorial here: https://rpubs.com/lgreski/improvingCaretPerformance


### Start looping through the response variables ###

for (flux in resp_vars) {
  
  print(flux)
  # flux <- "GPP_gC_m2"
  
  # subset data so that the response variables does not have NA
  modeldata <- subset(d, !is.na(d[flux]))
  
  
  ### Model parameter inputs for feature (variable) selection and model tuning
  
  ### Take a smaller subset!! this needs to be done first
  modeldata <- modeldata[1:2000, ]
  
  
  ### With randomforest, let's do this
  modeldata2 <- modeldata[,c("Study_ID_Clean", "id", flux, Baseline_vars_1km)]
  modeldata <- na.omit(modeldata2) # only 86 obs
  sapply(modeldata, function(x) sum(is.na(x))) # no missing data
  
  # Create a list of row indices for cross validation splits (for leave-one-fold out) 
  # 10-fold cross validation strategy similar to FLUXCOM: "The training data sets were stratified into
  # 10 folds, each containing ca. 10 % of the data. Entire sites
  # were assigned to each fold (Jung et al., 2011)"
  
  folds <- fold(data= modeldata,
    k = 10,
    id_col = "Study_ID_Clean")
  
  # merge
  folds_data <- subset(folds, select=c( ".folds", "id"))
  modeldata <- full_join(modeldata, folds_data, by="id")
  
  # create a row ID
  modeldata$samplerow <- seq(1, length(modeldata$Study_ID_Clean), by=1)
  
  indices <- list()
  indices_not <- list()
  
  for (k in unique(modeldata$.folds)) {
    
    # k = 1
    subs <- subset(modeldata, .folds==k)
    
    # list the row ids for the fold
    sites <- list(as.integer(subs$samplerow) )
    
    # list other row ids that don't belong to the k fold
    sites_not <- list(as.integer(modeldata$samplerow[!(modeldata$samplerow %in% unlist(sites))]))
    
    # check that all the rows are included
    length(sites_not[[1]]) + length(sites[[1]]) == length(modeldata$Study_ID_Clean)
    
    # append to list (caret wants these as lists)
    # row indices for each fold, used for model evaluation
    indices <- append(indices, sites)
    # row indices for model training
    indices_not <- append(indices_not, sites_not)
    
  }

  
  names(indices) <- 1:length(indices)
  names(indices_not) <- 1:length(indices)
  


  print("model tuning and feature selection starting")
  
  ## Feature selection parameters
  # define the predictions using a rfe selection function
  # rfe= Recursive Feature Elimination. 
  # Recursive feature elimination (RFE) is a feature selection method that fits a model and 
  # removes the weakest feature (or features) until the specified number of features is reached.
  # In the current RFE algorithm, the training data is being used for at least three purposes: predictor selection, model fitting and performance evaluation. 
  
  # Control parameters
  rfecontrol <- rfeControl(functions=rfFuncs, #caret-specific rfe process functions - there are e.g. lmFuncs too that are specific to one model type
                           method = "cv",  # No need to specify this because we use index column which automatically does k-fold cv. 
                           # and no need to specify number or repeats either
                           returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
                           # p = 0.7, # For leave-group out cross-validation: the training percentage. No need to worry about this since we use pre-defined indices
                           #summaryFunction = defaultSummary, # a function to compute performance metrics across resamples. 
                           #selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
                           index = indices_not, # a list with elements for each external resampling iteration. Each list element is the sample rows used for training at that iteration.
                           indexOut = indices, # a list (the same length as index) that dictates which sample are held-out for each resample.
                           allowParallel = TRUE,  # parallel processing
                           saveDetails =TRUE) # a logical to save the predictions and variable importances from the selection process
  
  # some useful web pages about the parameters:
  # https://stackoverflow.com/questions/24612824/r-caret-package-error-if-i-specified-index-for-both-rfe-control-and-train-contr
  # https://github.com/topepo/caret/issues/1019
  # https://stackoverflow.com/questions/37749009/custom-training-rows-when-using-caret-package
  # https://stats.stackexchange.com/questions/109340/leave-one-subject-out-cross-validation-in-caret
  # https://stats.stackexchange.com/questions/214387/results-from-rfe-function-caret-to-compute-average-metrics-r
  # https://stats.stackexchange.com/questions/323356/index-argument-with-createfolds-in-traincontrol-caret-package
  
  
  ## Model tuning parameters
  # Includes some options about what kind of information is saved
  tunecontrol <- trainControl(
    method = "cv",  # No need to specify this because we use index column which automatically does k-fold cv
    verboseIter = FALSE,  # A logical for printing a training log. This could be set to FALSE in the final model runs
    returnData = FALSE, # A logical for saving the data. This could be set to FALSE in the final model runs
    returnResamp = "final", # A character string indicating how much of the resampled summary metrics should be saved. We only save the final model
    # p = 0.7, # For leave-group out cross-validation: the training percentage
    summaryFunction = defaultSummary, # a function to compute performance metrics across resamples. 
    selectionFunction = "best", #  chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
    index = indices_not, # a list with elements for each resampling iteration.  needs to be integer
    indexOut = indices,
    allowParallel = TRUE, # parallel processing
    savePredictions="final") # an indicator of how much of the hold-out predictions for each resample should be saved. Values can be either "all", "final", or "none". 
  # A logical value can also be used that convert to "all" (for true) or "none" (for false). "final" saves the predictions for the optimal tuning parameters.
  # tuneLength = 3 is already included - this means that the model will try three different parameter estimates
  # Basically most of the key parameters are still coming from the rfeControl command which is a wrapper for train
  # But if I'd like to set different tuning parameters in trainControl, that is still possible: https://stackoverflow.com/questions/51933704/feature-selection-with-caret-rfe-and-training-with-another-method
  
  
  
  ### run the RFE and model tuning algorithm with 1 km predictors
  # note that no data not accepted for the response, and with svm and rf in predictors
  set.seed(448)
  rfe_fit = rfe(x=modeldata[,Baseline_vars_1km], y=modeldata[,flux],
                sizes = c(40, 30, 20), # a numeric vector of integers corresponding to the number of features that should be retained
                rfeControl = rfecontrol, # rfe parameters
                method="ranger", # modeling method
                trControl=tunecontrol) # tuning parameters) 

  # na.action="na.pass" means we're allowing NAs in the predictors - xgboost handles them quite nicely by splitting them to the two nodes based on NA data distribution
  # objective = "reg:squarederror" needed for xgbTree
  # Added that new command because of an error: https://github.com/topepo/caret/issues/1160
  
  print("1 km rfe and tuning done")
  
  ### Write the model out
  saveRDS(rfe_fit, paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(flux, "1km_ranger_test_speed2", sep="_"), ".rds"))
  

  
}



