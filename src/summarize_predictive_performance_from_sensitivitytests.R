


# Packages
library("caret")
library("dplyr")
library("purrr")
library("raster")
#install.packages('terra', repos='https://rspatial.r-universe.dev', lib="/mnt/data1/boreal/avirkkala/packages")
library(stringr)



### Load the model training data just in case
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv") 


# Reclassify veg type and tkwp
# not enough cavm observations for GPP and Reco... merge class 1 (barren) and 31 (prostrate shrub)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- ifelse(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged==1, 31, d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
# not enough tkwp sites for class 3 in 20 km models... merge 3 and 4
d$TKWP_Thermokarst <- ifelse(d$TKWP_Thermokarst==4, 3, d$TKWP_Thermokarst)

## List predictors for the models
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



### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 

### Models
models <- c( "qrf") 

### Kilometers
km <- c("20km")


### Model versions
versions <- c( "full_model_without_larvaloutbreak", "full_model_without_vegtype", "nofactors_model_with_econly", 
              "nofactors_model_with_reichsteinonly", "nofactors_model_without_daytimechambermeas", "nofactors_model_datafromallfluxes",
              "nofactors_model_without_disturbedsites", "nofactors_model_without_larvaloutbreak", 
              "nofactors_model_without_disturbedsites_plusharvest", "nofactors_model_without_disturbedsites_plusthermokarst",
              "nofactors_model_without_disturbedsites_plusfire")





### Loop over the models

for (i in resp_vars) {
  
  #i <- "NEE_gC_m2"
  
  print("looping through resp vars")
  print(i)
  
  
  # load the model training data: we'll use this to finalize model prediction data
  modeldata2 <- d[,c("Study_ID_Short", "id", i, Baseline_vars_20km)]
  modeldata1 <- na.omit(modeldata2) 
  
  # print(nrow(modeldata1))
  # print(length(unique(modeldata1$Study_ID_Short)))
  
  
  
  # loop through the models - this loop is also unnecessary now since we only have one model
  
  for (m in models) {
    
    # m <- "qrf"
    
    for (v in versions) {
      
      print(i)
      print(v)
      
      # v <- "full_model_without_larvaloutbreak"
      
      # Load model files
      mod <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i,  km, m, "train_loocv", v, sep="_"), ".rds"))
      
      # Performance
      paste(i,  km, m,  v, sep="_")
      df <- (mod$results[which.min(mod$results[, "RMSE"]),])
      print(round(df, digits=2))
      print(nrow(mod$pred))
    
      
      print("next")
      
      
    }
    
    
    
  } # model loop
  
  print("model loop done")
  
} # resp var loop

print("resp loop done")


      
      
      

# Load model files
i <- "NEE_gC_m2"
mod <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i,  km, m, "train_loocv", sep="_"), ".rds"))

# Performance
paste(i,  km, m,  v, sep="_")
df <- (mod$results[which.min(mod$results[, "RMSE"]),])
print(round(df, digits=2))
print(nrow(mod$pred))

  
  
  
  
  

