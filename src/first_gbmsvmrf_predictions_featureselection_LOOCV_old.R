


########### ANNA MUSITA TÄMÄ https://stackoverflow.com/questions/25121725/error-in-predicting-raster-with-randomforest-caret-and-factor-variables

# Packages
library("caret")
library("vip")
library("pdp")
library("ggplot2")
library("viridis")
library("dplyr")
library("terra")
library(stringr)

terraOptions(memfrac=0.95, tempdir = "/mnt/data1/boreal/avirkkala/Temp") # testing a different style


### Data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")


### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 


### Predictors
names(d)

## List predictors for the models (including the one-hot encoded factors)
# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "swe_terraclimate_sites", # climate "tmean_terraclimate_sites" correlates with LST
                       
                       "tmean_TerraClimate_averages", "ppt_TerraClimate_averages",  # don't have all of these:  "swe_TerraClimate_averages", "srad_TerraClimate_averages", "vpd_TerraClimate_averages", "pdsi_TerraClimate_averages", 
                       
                       "trend_20yrprior_terra_change_id", "terra_trend_10yrprior_terra_change_id", "terra_trend_19601990", "terra_trend_19812010",# temperature change - note that the naming convention changed a bit...
                       
                       "ndvi_trend_10yrprior_ndvi_change_id",  "ndvi_trend_19812010", # ndvi change trend - not including permafrost for now
                       
                       "Barrow_CO2_conc_Barrow_CO2conc",
                       
                       "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                       
                       "NDVI_whittaker_constant_monthly_mean", # "EVI_whittaker_constant_monthly_mean", "EVI_MOD13A1v006_NDVI_EVI_sites_low_quality_merge_whittaker",  "NDVI_MOD13A1v006_NDVI_EVI_sites_low_quality_merge_whittaker", # Optical RS, dropped several because highly correlated
                       
                       "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", # "LST_Night_1km_MOD11A2v006_LST_Night_sites_low_quality", # highly correlated
                       
                       "water_ground_MCD43A4_annual_water_ground_sites_low_quality", "water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality", 
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
                       
                       "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                       
                       "Percent_NonTree_Vegetation_MOD44B_sites", "Percent_NonVegetated_MOD44B_sites", "Percent_Tree_Cover_MOD44B_sites",
                       
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", 
                       
                       "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                       "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", "wtd_Water_table_depth", 
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", # Permafrost
                       
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned",   # Disturbance
                       
                       "TKWP_Thermokarst", "TKHP_Thermokarst" # Disturbance 
                       
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km



# Variables used in 20 km spatial resolution models
Baseline_vars_20km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites", "swe_terraclimate_sites", # climate
                        
                        "tmean_TerraClimate_averages", "ppt_TerraClimate_averages",  # don't have all of these:  "swe_TerraClimate_averages", "srad_TerraClimate_averages", "vpd_TerraClimate_averages", "pdsi_TerraClimate_averages", 
                        
                        "trend_20yrprior_terra_change_id", "terra_trend_10yrprior_terra_change_id", "terra_trend_19601990", "terra_trend_19812010",# temperature change - note that the naming convention changed a bit...
                        
                        "ndvi_trend_10yrprior_ndvi_change_id",  "ndvi_trend_19812010", # ndvi change trend - not including permafrost for now
                        
                        "Barrow_CO2_conc_Barrow_CO2conc",
                        
                        "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                        
                        "ndvi3g_lower_mean_GIMMS3g_NDVI_sites_high_and_low_quality", # Optical RS
                        
                        "SMMR_SSMIS_thaw_days_NTSG_FT_SMMR_SSMIS_25km", "SMMR_SSMIS_transitional_days_NTSG_FT_SMMR_SSMIS_25km",#microwave 
                        
                        "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
                        
                        "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                        
                        "Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Percent_Tree_Cover_MOD44B_sites", "Percent_NonVegetated_AVHRR_VCF5KYR", 
                        
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", 
                        
                        "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                        "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", "wtd_Water_table_depth", 
                        
                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", # Permafrost
                        
                        "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned",   # Disturbance
                        
                        "TKWP_Thermokarst", "TKHP_Thermokarst" # Disturbance 
                        
                        
)


# check that the columns exist
Baseline_vars_20km %in% colnames(d)



# variables as factors
d$TKWP_Thermokarst <- as.factor(d$TKWP_Thermokarst)
d$TKHP_Thermokarst <- as.factor(d$TKHP_Thermokarst)
d$Number_of_days_since_fire_classes_gfed_monthly_calc <- as.factor(d$Number_of_days_since_fire_classes_gfed_monthly_calc)
d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- as.factor(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- as.factor(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)



### Response variables
resp_vars <- c("NEE_gC_m2") 

### Models
models <- c("rf")

### Kilometers
kms <- c("1km")


### Time periods
time <- seq(as.Date("2001/01/01"), as.Date("2020/12/31"), "months")
time <- substr(time, 1, 7)
time <- sub("-", "_", sub("_", "", time, fixed=TRUE), fixed=TRUE)
time_alt <- gsub("_0", "_", time)


### Cropping extents
crops <- rbind(c(-4834843, 4834843, -4834843, 0),  c(-4834843, 4834843,  0, 4834843))

#test <- crop(pred_rast_static[[1]], ext(crops[1,]))


### Set folder for results
setwd("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/figures")



# For figures

theme_pub <- theme_bw() + theme(panel.border=element_rect(size=1, colour="black"),
                                axis.text=element_text(size=14, face="bold"),
                                plot.title = element_text(size = 14, face = "bold"),
                                axis.title=element_text(size=14, face="bold"), 
                                plot.subtitle=element_text(size=14, face="bold", color="black"), 
                                strip.text.x = element_text(size = 14, face="bold"),
                                legend.text=element_text(size=14, face="bold"), legend.title=element_text(size=14))



for (i in resp_vars) {
  
  #i <- "NEE_gC_m2"

  
  # Add data
  # remove NA across columns for 1 and 20 km datasets
  modeldata2 <- d[,c("Study_ID_Short", "id", i, Baseline_vars_1km)]
  modeldata1 <- na.omit(modeldata2) # only 86 obs
  sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
  print("1 km data set:")
  print(nrow(modeldata1))
  
  modeldata2 <- d[,c("Study_ID_Short", "id", i, Baseline_vars_20km)]
  modeldata2 <- na.omit(modeldata2) # only 86 obs
  sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
  print("20 km data set:")
  print(nrow(modeldata2))
  
  
  
  # create a row ID
  modeldata1$samplerow <- seq(1, length(modeldata1$Study_ID_Short), by=1)
  
  # create a row ID
  modeldata2$samplerow <- seq(1, length(modeldata2$Study_ID_Short), by=1)
  
  
  # merge back other information
  modeldata11 <- merge(modeldata1[ , !(names(modeldata1) %in%  c("Study_ID_Short",  i, Baseline_vars_1km))], d, by="id")
  modeldata22 <- merge(modeldata2[ , !(names(modeldata2) %in%  c("Study_ID_Short",  i, Baseline_vars_20km))], d, by="id")
  

  
  
  
  for (km in kms) {
    
  # km <- "1km"
  # i <- "NEE_gC_m2"

  
  
  # Loop through the models 
  
  for (m in models) {
    
    # m <- "rf"
    
    # Load model files
    mod <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i,  km, m, "loocv", sep="_"), ".rds"))
    
    # # Print the best variables
    # print("Best variables are:")
    mod$optVariables
    
    
    
    # creating predictions in two parts: NA and Siberia
    
    for (c in 1:2) {
      
      
      
      ### Load static vars (only once)
      setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_1km")
      
      ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")
      
      aboveground_biomass_carbon_2010_Above_belowground_biomass <- rast("abovegroundbiomass.tif")
      plot(aboveground_biomass_carbon_2010_Above_belowground_biomass)
      aboveground_biomass_carbon_2010_Above_belowground_biomass
      summary(d$aboveground_biomass_carbon_2010_Above_belowground_biomass)
      aboveground_biomass_carbon_2010_Above_belowground_biomass <- aboveground_biomass_carbon_2010_Above_belowground_biomass/100
      aboveground_biomass_carbon_2010_Above_belowground_biomass <- crop(aboveground_biomass_carbon_2010_Above_belowground_biomass, ext(crops[c,]))
      
      
      tmean_TerraClimate_averages <-  rast("Terraclimate_averages_tmean.tif")
      plot(tmean_TerraClimate_averages)
      tmean_TerraClimate_averages
      summary(d$tmean_TerraClimate_averages)
      tmean_TerraClimate_averages <- tmean_TerraClimate_averages/1000
      
      SoilGrids_SOC_SoilGrids_SOCstock <-  rast("soc.tif")
      plot(SoilGrids_SOC_SoilGrids_SOCstock)
      SoilGrids_SOC_SoilGrids_SOCstock
      summary(d$SoilGrids_SOC_SoilGrids_SOCstock)
      SoilGrids_SOC_SoilGrids_SOCstock <- SoilGrids_SOC_SoilGrids_SOCstock/100
      
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("roughscale.tif")
      plot(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
      summary(d$dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m/100
      
      terra_trend_19812010<- rast("tmean_trend_19812010.tif")
      plot(terra_trend_19812010)
      terra_trend_19812010
      summary(d$terra_trend_19812010)
      terra_trend_19812010 <- terra_trend_19812010/100 ### EI TOIMI
      
      wtd_Water_table_depth <- rast("wtd.tif") ## EI TOIMI; PALJON TYGJÄÄ!!!
      plot(wtd_Water_table_depth)
      wtd_Water_table_depth
      summary(d$wtd_Water_table_depth)
      wtd_Water_table_depth <- wtd_Water_table_depth/100  ############## TÄMÄ IHAN KUMMALLINEN - ARVOT PITÄISI OLLA NEGATIIVISIA?????????
      
      belowground_biomass_carbon_2010_Above_belowground_biomass <- rast("belowgroundbiomass.tif") ## ONKO TÄMä UNCERTAINTY??
      plot(belowground_biomass_carbon_2010_Above_belowground_biomass)
      belowground_biomass_carbon_2010_Above_belowground_biomass
      summary(d$belowground_biomass_carbon_2010_Above_belowground_biomass)
      belowground_biomass_carbon_2010_Above_belowground_biomass <- belowground_biomass_carbon_2010_Above_belowground_biomass/100 
      
      BLDFIE_M_sl1_250m_ll_SoilGrids <- rast("bulkdensity.tif")
      plot(BLDFIE_M_sl1_250m_ll_SoilGrids)
      BLDFIE_M_sl1_250m_ll_SoilGrids
      summary(d$BLDFIE_M_sl1_250m_ll_SoilGrids)
      BLDFIE_M_sl1_250m_ll_SoilGrids <- BLDFIE_M_sl1_250m_ll_SoilGrids/100 
      
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("cti.tif")
      plot(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
      summary(d$dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m/100 
      
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- rast("sol_watercontent.tif")
      plot(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent
      summary(d$sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent/100 
      
      ppt_TerraClimate_averages <- rast("Terraclimate_averages_ppt.tif")
      plot(ppt_TerraClimate_averages)
      ppt_TerraClimate_averages
      summary(d$ppt_TerraClimate_averages)
      ppt_TerraClimate_averages <- ppt_TerraClimate_averages/100 
      
      ndvi_trend_19812010 <-  rast("ndvi_trend_19812010.tif") 
      plot(ndvi_trend_19812010)
      ndvi_trend_19812010
      summary(d$ndvi_trend_19812010)
      ndvi_trend_19812010 <- ndvi_trend_19812010/100000 ##################### MAYBE CORRECT????
      
      PHIHOX_M_sl1_250m_ll_SoilGrids <- rast("ph.tif")
      plot(PHIHOX_M_sl1_250m_ll_SoilGrids)
      PHIHOX_M_sl1_250m_ll_SoilGrids
      summary(d$PHIHOX_M_sl1_250m_ll_SoilGrids)
      PHIHOX_M_sl1_250m_ll_SoilGrids <- PHIHOX_M_sl1_250m_ll_SoilGrids/100 
      
      
      terra_trend_19601990 <- rast("tmean_trend_19601990.tif")
      plot(terra_trend_19601990)
      terra_trend_19601990
      summary(d$terra_trend_19601990)
      terra_trend_19601990 <- terra_trend_19601990/100 #### TÄMä NÄYTTÄISI SAMALTA KUIN TERRA_TREND 1981....??????
      
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- rast("UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif")
      plot(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH
      summary(d$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH/100 
      
      TKWP_Thermokarst <- rast("Circumpolar_Thermokarst_Landscapes_TKWP.tif")
      plot(TKWP_Thermokarst)
      summary(d$TKWP_Thermokarst)
      
      
      # stack in pieces
      # create a stack
      pred_rast1 <- c(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, aboveground_biomass_carbon_2010_Above_belowground_biomass,
                      tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock,
                      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, terra_trend_19812010)
      
      
      # modify names to match with the dataframe
      names(pred_rast1) <- c("ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", "aboveground_biomass_carbon_2010_Above_belowground_biomass",
                             "tmean_TerraClimate_averages", "SoilGrids_SOC_SoilGrids_SOCstock",
                             "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "terra_trend_19812010")
      is.factor(pred_rast1)
      
      # remove the individual layers - they are just taking space
      rm(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
      rm(aboveground_biomass_carbon_2010_Above_belowground_biomass)
      rm(tmean_TerraClimate_averages)
      rm(SoilGrids_SOC_SoilGrids_SOCstock)
      rm(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      rm(terra_trend_19812010)
      
      # rm(list=c(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, aboveground_biomass_carbon_2010_Above_belowground_biomass,
      #           tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock, 
      #           dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, terra_trend_19812010)) ## NOT WORKING???
      
      # rasters 2
      pred_rast2 <- c(wtd_Water_table_depth, belowground_biomass_carbon_2010_Above_belowground_biomass,
                      BLDFIE_M_sl1_250m_ll_SoilGrids, dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m,
                      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent, ppt_TerraClimate_averages)
      
      # names(pred_rast1) <- c("wtd_Water_table_depth", "belowground_biomass_carbon_2010_Above_belowground_biomass",
      #                        "tmean_TerraClimate_averages", "SoilGrids_SOC_SoilGrids_SOCstock",
      #                        "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "terra_trend_19812010")
      
      rm(wtd_Water_table_depth)
      rm(belowground_biomass_carbon_2010_Above_belowground_biomass)
      rm(BLDFIE_M_sl1_250m_ll_SoilGrids)
      rm(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
      rm(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      rm(ppt_TerraClimate_averages)
      
      
      
      pred_rast3 <- c(PHIHOX_M_sl1_250m_ll_SoilGrids, terra_trend_19601990,
                      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, TKWP_Thermokarst, ndvi_trend_19812010)
      
      rm(PHIHOX_M_sl1_250m_ll_SoilGrids)
      rm(terra_trend_19601990)
      rm(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      rm(TKWP_Thermokarst)
      rm(ndvi_trend_19812010)
      
      # NIMET
      
      gc()
      
      ### Combine
      pred_rast <- c(pred_rast1, pred_rast2)
      rm(pred_rast1)
      rm(pred_rast2)
      gc()
      
      pred_rast_static <- c(pred_rast, pred_rast3)
      
      rm(pred_rast)
      rm(pred_rast3)
      gc()
      
      
      # ### FULL !
      # # not enough memory!
      # pred_rast1 <- c(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, aboveground_biomass_carbon_2010_Above_belowground_biomass,
      #                 tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock,
      #                 dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, terra_trend_19812010,
      #                 wtd_Water_table_depth, belowground_biomass_carbon_2010_Above_belowground_biomass,
      #                 BLDFIE_M_sl1_250m_ll_SoilGrids, dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m,
      #                 sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent, ppt_TerraClimate_averages,
      #                 PHIHOX_M_sl1_250m_ll_SoilGrids, terra_trend_19601990,
      #                 UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, TKWP_Thermokarst, ndvi_trend_19812010)
      # 
      # # modify names to match with the dataframe
      # print(names(pred_rast1))
      # name <- sapply(strsplit(sources(pred_rast1)$source, split= "/", fixed = TRUE), tail, 1L)
      # name <- str_sub(name, 1, str_length(name)-4)
      # names(pred_rast1) <- name
      # 
      # 
      # ### EI TOIMI KOSKA ERI MÄÄRÄ NA:ta
      # pred_data <- cbind(terra::as.data.frame(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, xy=TRUE, cells=FALSE),
      #                    terra::as.data.frame(aboveground_biomass_carbon_2010_Above_belowground_biomass,  cells=FALSE),
      #                    terra::as.data.frame(tmean_TerraClimate_averages,  cells=FALSE),
      #                    terra::as.data.frame(SoilGrids_SOC_SoilGrids_SOCstock,  cells=FALSE),
      #                    terra::as.data.frame(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m,  cells=FALSE),
      #                    terra::as.data.frame(terra_trend_19812010,  cells=FALSE),
      #                    terra::as.data.frame(wtd_Water_table_depth,  cells=FALSE),
      #                    terra::as.data.frame(belowground_biomass_carbon_2010_Above_belowground_biomass,  cells=FALSE),
      #                    terra::as.data.frame(BLDFIE_M_sl1_250m_ll_SoilGrids,  cells=FALSE),
      #                    terra::as.data.frame(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m,  cells=FALSE),
      #                    terra::as.data.frame(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent,  cells=FALSE),
      #                    terra::as.data.frame(ppt_TerraClimate_averages,  cells=FALSE),
      #                    terra::as.data.frame(PHIHOX_M_sl1_250m_ll_SoilGrids,  cells=FALSE),
      #                    terra::as.data.frame(terra_trend_19601990,  cells=FALSE),
      #                    terra::as.data.frame(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH,  cells=FALSE),
      #                    terra::as.data.frame(TKWP_Thermokarst,  cells=FALSE),
      #                    terra::as.data.frame(ndvi_trend_19812010,  cells=FALSE)
      #                    
      #                    )
      # 
      # # remove the individual layers - they are just taking space
      # rm(list=c(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, aboveground_biomass_carbon_2010_Above_belowground_biomass,
      #           tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock, 
      #           dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, terra_trend_19812010, 
      #           wtd_Water_table_depth, belowground_biomass_carbon_2010_Above_belowground_biomass, 
      #           BLDFIE_M_sl1_250m_ll_SoilGrids, dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, 
      #           sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent, ppt_TerraClimate_averages,
      #           PHIHOX_M_sl1_250m_ll_SoilGrids, terra_trend_19601990, 
      #           UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, TKWP_Thermokarst))
      # 
      
      
      
      
      ### Loop over times and load dynamic vars
      for (t in 1:length(time)) {
        
        # t <- 1
        srad_terraclimate_sites <- rast(paste0("srad_", time_alt[t], ".tif"))
        plot(srad_terraclimate_sites)
        srad_terraclimate_sites
        summary(d$srad_terraclimate_sites)
        #srad_terraclimate_sites <- srad_terraclimate_sites/100  ### NO CONVERSION NEEDED?
        
        Barrow_CO2_conc_Barrow_CO2conc <- rast(paste0("co2_", time_alt[t], ".tif"))
        plot(Barrow_CO2_conc_Barrow_CO2conc)
        Barrow_CO2_conc_Barrow_CO2conc
        summary(d$Barrow_CO2_conc_Barrow_CO2conc)
        Barrow_CO2_conc_Barrow_CO2conc <- Barrow_CO2_conc_Barrow_CO2conc/1000  
        
        NDVI_whittaker_constant_monthly_mean <- rast(paste0("ndvi_", time_alt[t], ".tif")) ## EI ARVOJA plotissa mutta valuesin kautta tulee??? mutta on arvoja raster-komennon kautta plotissa???
        plot(NDVI_whittaker_constant_monthly_mean)
        NDVI_whittaker_constant_monthly_mean
        summary(d$NDVI_whittaker_constant_monthly_mean)
        NDVI_whittaker_constant_monthly_mean <- NDVI_whittaker_constant_monthly_mean/1000
        
        Soil.temperature.level.1_era5_soilmoist_temp_snow <- rast(paste0("soiltemplevel1_", time[t], ".tif"))
        plot(Soil.temperature.level.1_era5_soilmoist_temp_snow)
        Soil.temperature.level.1_era5_soilmoist_temp_snow
        summary(d$Soil.temperature.level.1_era5_soilmoist_temp_snow)
        Soil.temperature.level.1_era5_soilmoist_temp_snow <- Soil.temperature.level.1_era5_soilmoist_temp_snow/100
        
        water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality <- rast(paste0("ndii_", substr(time[t],1, 4), ".tif"))
        plot(water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality)
        water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality
        summary(d$water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality)
        water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality <- water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality/100
        
        water_ground_MCD43A4_annual_water_ground_sites_low_quality <- rast(paste0("ndwi_", substr(time[t],1, 4), ".tif"))
        plot(water_ground_MCD43A4_annual_water_ground_sites_low_quality)
        water_ground_MCD43A4_annual_water_ground_sites_low_quality
        summary(d$water_ground_MCD43A4_annual_water_ground_sites_low_quality)
        water_ground_MCD43A4_annual_water_ground_sites_low_quality <- water_ground_MCD43A4_annual_water_ground_sites_low_quality/100
        
        LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality <- rast(paste0("lst_", time_alt[t], ".tif"))
        plot(LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality)
        LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality
        summary(d$LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality)
        LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality <- LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality/100
        
        vpd_terraclimate_sites <- rast(paste0("vpd_", time_alt[t], ".tif"))
        plot(vpd_terraclimate_sites)
        vpd_terraclimate_sites
        summary(d$vpd_terraclimate_sites)
        #vpd_terraclimate_sites <- vpd_terraclimate_sites/100  ### NO CONVERSION NEEDED?
        
        Snow.cover_era5_soilmoist_temp_snow <- rast(paste0("snowcover_", time[t], ".tif"))
        plot(Snow.cover_era5_soilmoist_temp_snow)
        Snow.cover_era5_soilmoist_temp_snow
        summary(d$Snow.cover_era5_soilmoist_temp_snow)
        Snow.cover_era5_soilmoist_temp_snow <- Snow.cover_era5_soilmoist_temp_snow/100
        
        Percent_Tree_Cover_MOD44B_sites <- raster::stack(paste0("Percent_Tree_Cover_mod44b", substr(time[t],1, 4), ".tif")) # THE FIRST LAYER!!!!
        Percent_Tree_Cover_MOD44B_sites <- rast(Percent_Tree_Cover_MOD44B_sites[[1]])
        plot(Percent_Tree_Cover_MOD44B_sites)
        Percent_Tree_Cover_MOD44B_sites
        summary(d$Percent_Tree_Cover_MOD44B_sites)
        
        Percent_NonTree_Vegetation_MOD44B_sites <- raster::stack(paste0("Percent_NonTree_Vegetation_mod44b", substr(time[t],1, 4), ".tif"))  # THE SECOND LAYER!!!! (non-vegetated is the third)
        Percent_NonTree_Vegetation_MOD44B_sites <- rast(Percent_NonTree_Vegetation_MOD44B_sites[[2]])
        plot(Percent_Tree_Cover_MOD44B_sites)
        Percent_NonTree_Vegetation_MOD44B_sites
        summary(d$Percent_NonTree_Vegetation_MOD44B_sites)
        
        terra_trend_10yrprior_terra_change_id <- rast(paste0("tmean10yrprior_trend_", substr(time[t],1, 4), ".tif"))
        plot(terra_trend_10yrprior_terra_change_id)
        terra_trend_10yrprior_terra_change_id
        summary(d$terra_trend_10yrprior_terra_change_id)
        terra_trend_10yrprior_terra_change_id <- terra_trend_10yrprior_terra_change_id/100 ########### TÄSSÄ JOTAIN HÄMÄRÄÄÄ
        
        trend_20yrprior_terra_change_id <- rast(paste0("tmean20yrprior_trend_", substr(time[t],1, 4), ".tif"))
        plot(trend_20yrprior_terra_change_id)
        trend_20yrprior_terra_change_id
        summary(d$trend_20yrprior_terra_change_id)
        trend_20yrprior_terra_change_id <- trend_20yrprior_terra_change_id/100  ########### TÄSSÄ JOTAIN HÄMÄRÄÄÄ
        
        ndvi_trend_10yrprior_ndvi_change_id <- rast(paste0("ndvi10yrprior_trend_", substr(time[t],1, 4), ".tif"))
        plot(ndvi_trend_10yrprior_ndvi_change_id)
        ndvi_trend_10yrprior_ndvi_change_id
        summary(d$ndvi_trend_10yrprior_ndvi_change_id)
        ndvi_trend_10yrprior_ndvi_change_id <- ndvi_trend_10yrprior_ndvi_change_id/100 ########### TÄSSÄ JOTAIN HÄMÄRÄÄÄ
        
        Snow.depth_era5_soilmoist_temp_snow <- rast(paste0("snowdepth_", time[t], ".tif"))
        plot(Snow.depth_era5_soilmoist_temp_snow)
        Snow.depth_era5_soilmoist_temp_snow
        summary(d$Snow.depth_era5_soilmoist_temp_snow)
        Snow.depth_era5_soilmoist_temp_snow <- Snow.depth_era5_soilmoist_temp_snow/100
        
        pr_terraclimate_sites <- rast(paste0("ppt_", time_alt[t], ".tif"))
        plot(pr_terraclimate_sites)
        pr_terraclimate_sites
        summary(d$pr_terraclimate_sites)
        pr_terraclimate_sites <- pr_terraclimate_sites/100
        
        pdsi_terraclimate_sites <- rast(paste0("pdsi_", time_alt[t], ".tif"))
        plot(pdsi_terraclimate_sites)
        pdsi_terraclimate_sites
        summary(d$pdsi_terraclimate_sites)
        #pdsi_terraclimate_sites <- pdsi_terraclimate_sites/100 # not needed???
        
        
        
        # stack
        
        pred_rast1 <- c(srad_terraclimate_sites, Barrow_CO2_conc_Barrow_CO2conc,
                        NDVI_whittaker_constant_monthly_mean, Soil.temperature.level.1_era5_soilmoist_temp_snow,
                        water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality)
        
        rm(srad_terraclimate_sites)
        rm(Barrow_CO2_conc_Barrow_CO2conc)
        rm(NDVI_whittaker_constant_monthly_mean)
        rm(Soil.temperature.level.1_era5_soilmoist_temp_snow)
        rm(water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality)
        
        gc()
        
        
        pred_rast2 <- c(water_ground_MCD43A4_annual_water_ground_sites_low_quality,
                        LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality, vpd_terraclimate_sites, 
                        Snow.cover_era5_soilmoist_temp_snow, terra_trend_10yrprior_terra_change_id)
        
        rm(water_ground_MCD43A4_annual_water_ground_sites_low_quality)
        rm(LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality)
        rm(vpd_terraclimate_sites)
        rm(Snow.cover_era5_soilmoist_temp_snow)
        rm(terra_trend_10yrprior_terra_change_id)
        
        gc()
        
        pred_rast3 <- c(trend_20yrprior_terra_change_id, Snow.depth_era5_soilmoist_temp_snow, 
                        pr_terraclimate_sites, pdsi_terraclimate_sites)
        
        rm(trend_20yrprior_terra_change_id)
        rm(Snow.depth_era5_soilmoist_temp_snow)
        rm(pr_terraclimate_sites)
        rm(pdsi_terraclimate_sites)
        
        
        gc()
        
        
        
        pred_rast4 <- c(ndvi_trend_10yrprior_ndvi_change_id, 
                        Percent_Tree_Cover_MOD44B_sites, Percent_NonTree_Vegetation_MOD44B_sites)
        rm(ndvi_trend_10yrprior_ndvi_change_id)
        rm(Percent_Tree_Cover_MOD44B_sites)
        rm(Percent_NonTree_Vegetation_MOD44B_sites)
        
        gc()
        
        
        
        ### Combine
        pred_rast <- c(pred_rast1, pred_rast2)
        rm(pred_rast1)
        rm(pred_rast2)
        gc()
        
        pred_rast_dynamic <- c(pred_rast, pred_rast3)
        
        rm(pred_rast)
        rm(pred_rast3)
        gc()
        
        pred_rast_dynamic <- c(pred_rast_dynamic, pred_rast4)
        rm(pred_rast4)
        gc()
        
        
        
        ### combine all
        pred
        
        # pred_rast1 <- c(srad_terraclimate_sites, Barrow_CO2_conc_Barrow_CO2conc,
        #                NDVI_whittaker_constant_monthly_mean, Soil.temperature.level.1_era5_soilmoist_temp_snow,
        #                water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality, water_ground_MCD43A4_annual_water_ground_sites_low_quality,
        #                LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality, vpd_terraclimate_sites, 
        #                Snow.cover_era5_soilmoist_temp_snow, terra_trend_10yrprior_terra_change_id, 
        #                trend_20yrprior_terra_change_id, Snow.depth_era5_soilmoist_temp_snow, 
        #                pr_terraclimate_sites, pdsi_terraclimate_sites, ndvi_trend_10yrprior_ndvi_change_id
        #                ) # Percent_Tree_Cover_MOD44B_sites mISSING, 
        # 
        # name <- sapply(strsplit(sources(pred_rast1)$source, split= "/", fixed = TRUE), tail, 1L)
        # name <- str_sub(name, 1, str_length(name)-4)
        # names(pred_rast1) <- name
        
        
        pred_rast <- c(pred_rast1, pred_rast2)
        
        pred_data <- c(pred_data1, pred_data2)
        
        pred <- predict(pred_data, mod, na.rm=TRUE, cores=7, cpkgs=c("ranger", "caret"), filename="temp_pred.tif", overwrite=TRUE)
        
        #pred <- terra::predict(pred_rast, mod, na.rm=TRUE, cores=7, cpkgs=c("ranger", "caret"), filename="temp_pred.tif", overwrite=TRUE)
        
      } # time period loop
      
      
      
      
    } # crop loop done
    
   
    

    


    
    

  } # model loop done
  
  print("model loop done")
  
  
  
  } # km loop done
  
  print("km loop done")
  
  
}




