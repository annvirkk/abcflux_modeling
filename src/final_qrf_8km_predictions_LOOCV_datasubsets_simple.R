


# Packages

.libPaths("/home/master/R/x86_64-pc-linux-gnu-library/4.2")


# Use these for running through terminal
#install.packages("ggplot2")
library("sp", lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
library("ggplot2", lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
library("caret", lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
library("dplyr", lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
library("purrr", lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
library("raster", lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
library("terra", lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
#install.packages('terra', repos='https://rspatial.r-universe.dev', lib="/mnt/data1/boreal/avirkkala/packages")
library(stringr, lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
library(googleCloudStorageR, lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
library(purrr, lib.loc="R/x86_64-pc-linux-gnu-library/4.2")
library("quantregForest", lib.loc="R/x86_64-pc-linux-gnu-library/4.2")


# Use these for running in Rstudio
library("sp")
library("ggplot2")
library("caret")
library("dplyr")
library("purrr")
library("raster")
library("terra")
#install.packages('terra', repos='https://rspatial.r-universe.dev', lib="/mnt/data1/boreal/avirkkala/packages")
library(stringr)
library(googleCloudStorageR)
library(purrr)
library("quantregForest")



# Terra settings
terraOptions(memfrac=0.9, tempdir = "/home/master/temp/") 
options(digits=20) # this is important, otherwise we will lose some digits with the cropped extents


# Google cloud settings
my_project_id <- "top-operand-328213"
gcs_list_buckets(my_project_id)
gcs_global_bucket("abcflux_modeling_files")
contents <- gcs_list_objects()
gcs_upload_set_limit(50000000L) # increasing data size limit for transferring data to google cloud


### Load the model training data just in case
d <- read.csv("/home/master/flux_upscaling_data/results/final/modeldata_avg.csv") 


# Reclassify veg type and tkwp
# not enough cavm observations for GPP and Reco... merge class 1 (barren) and 31 (prostrate shrub)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- ifelse(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged==1, 31, d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
# not enough tkwp sites for class 3 in 20 km models... merge 3 and 4
d$TKWP_Thermokarst <- ifelse(d$TKWP_Thermokarst==4, 3, d$TKWP_Thermokarst)


## List predictors for the models
# Variables used in 20 km spatial resolution models
# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", 
                       
                       "Barrow_CO2_conc_Barrow_CO2conc", # atmos CO2 conc
                       
                       "Snow.cover_era5_soilmoist_temp_snow", 
                       
                       "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                       
                       "NDVI_whittaker_constant_monthly_mean", # Optical RS, dropped several because highly correlated
                       
                       "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", # surface temp
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", 
                       
                       "Percent_NonTree_Vegetation_MOD44B_sites", "Percent_Tree_Cover_MOD44B_sites", # veg cover
                       
                       "PHIHOX_M_sl1_250m_ll_SoilGrids",  "SoilGrids_SOC_SoilGrids_SOCstock", 
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH"
                       
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km




# Variables used in 20 km spatial resolution models
Baseline_vars_20km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites",  "tmean_terraclimate_sites",  # tmean included because don't have LST
                        
                        "Barrow_CO2_conc_Barrow_CO2conc",
                        
                        "Snow.cover_era5_soilmoist_temp_snow", 
                        
                        "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", 
                        
                        "ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled",  
                        
                        "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m",
                        
                        "Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Percent_TreeCover_AVHRR_VCF5KYR", # equivalent to MOD tree cover product
                        
                        "PHIHOX_M_sl1_250m_ll_SoilGrids",  "SoilGrids_SOC_SoilGrids_SOCstock", 
                        
                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH" 
                        
                        
)

# check that the columns exist
Baseline_vars_20km %in% colnames(d)



### Response variables
resp_vars <- c("GPP_gC_m2", "Reco_gC_m2", "NEE_gC_m2") 

### Models
models <- c( "qrf") 

### Kilometers
kms <- c("20km")


### Model versions

versions <- c("full_model_simple")
# versions <- c("full_model_without_vegtype", "full_model_without_srad") # ,
# "full_model_predictor_subset"

#
# versions <- c("full_model_without_vegtype", "nofactors_model_with_econly", 
#               "nofactors_model_with_reichsteinonly", "nofactors_model_without_daytimechambermeas", "nofactors_model_datafromallfluxes",
#               "nofactors_model_without_disturbedsites", "nofactors_model_without_larvaloutbreak", 
#               "nofactors_model_without_disturbedsites_plusharvest", "nofactors_model_without_disturbedsites_plusthermokarst",
#               "nofactors_model_without_disturbedsites_plusfire")


### Time periods for the monthly predictions
# Loop through average monthly rasters
time <- seq(as.Date("1990/01/01"), as.Date("2016/12/31"), "months") 
time <- substr(time, 1, 7)
time <- sub("-", "_", sub("_", "", time, fixed=TRUE), fixed=TRUE)
time_alt <- gsub("_0", "_", time)

time <- time[1:12]
time_alt <- time_alt[1:12]
time <- substr(time, 6,7)
time_alt <- substr(time_alt, 6,7)






### 8 km predictions will not need to be cropped to several domains as the 1 km predictions so we'll just crop the rasters to their original extent
setwd("/home/master/") 
gcs_get_object("masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif", saveToDisk = "masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif",overwrite=TRUE)
extent <- ext(rast("masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif")) %>% as.vector()
crops <- NA
crops <- rbind(crops, extent) %>% data.frame()
crops <- crops[2, ]

### Loop over cropped extents, times and load predictors
### Then loop over response variables and predict


print("starting to make predictions in a loop")


for (c in 1:nrow(crops)) { 
  # c <- 1
  print(c)
  
  # get the cropped extent
  cropped_exent <- as.numeric(crops[c,])
  
  # loop over the model spatial resolution - this is actually an unnecessary step now since each script only focuses on one spatial resolution
  for (km in kms) {
    
    print(km)
    # km <- "20km"
    
    
    if (km=="20km") {
      
      
      ### Load static vars (only once)
      setwd("/home/master/") 
      
      gcs_get_object("predictors_8km/soc.tif", saveToDisk = "predictors_8km/soc.tif", overwrite=TRUE)
      SoilGrids_SOC_SoilGrids_SOCstock <-  rast("predictors_8km/soc.tif")
      #plot(SoilGrids_SOC_SoilGrids_SOCstock)
      SoilGrids_SOC_SoilGrids_SOCstock
      summary(d$SoilGrids_SOC_SoilGrids_SOCstock)
      SoilGrids_SOC_SoilGrids_SOCstock <- SoilGrids_SOC_SoilGrids_SOCstock/100
      #SoilGrids_SOC_SoilGrids_SOCstock <- as.data.frame(SoilGrids_SOC_SoilGrids_SOCstock, xy=TRUE)
      # Unit tonnes per ha
      
      
      gcs_get_object("predictors_8km/cti.tif", saveToDisk = "predictors_8km/cti.tif", overwrite=TRUE)
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("predictors_8km/cti.tif")
      #plot(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
      summary(d$dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m/100 
      
      #dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- as.data.frame(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, xy=TRUE)
      # Compound topographic index, high value= high topographic moisture
      
      
      
      gcs_get_object("predictors_8km/ph.tif", saveToDisk = "predictors_8km/ph.tif", overwrite=TRUE)
      PHIHOX_M_sl1_250m_ll_SoilGrids <- rast("predictors_8km/ph.tif")
      #plot(PHIHOX_M_sl1_250m_ll_SoilGrids)
      PHIHOX_M_sl1_250m_ll_SoilGrids
      summary(d$PHIHOX_M_sl1_250m_ll_SoilGrids)
      PHIHOX_M_sl1_250m_ll_SoilGrids <- PHIHOX_M_sl1_250m_ll_SoilGrids/100 
      #PHIHOX_M_sl1_250m_ll_SoilGrids <- as.data.frame(PHIHOX_M_sl1_250m_ll_SoilGrids, xy=TRUE)
      
      
      
      gcs_get_object("predictors_8km/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif", saveToDisk = "predictors_8km/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif", overwrite=TRUE)
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- rast("predictors_8km/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif")
      #plot(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH
      summary(d$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH/100 
      #UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- as.data.frame(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, xy=TRUE)
      # Permafrost probability (fraction 0-1)
      
      
      
      gcs_get_object("predictors_8km/latitude.tif", saveToDisk = "predictors_8km/latitude.tif", overwrite=TRUE)
      y <- rast("predictors_8km/latitude.tif")
      
      
      gcs_get_object("predictors_8km/longitude.tif", saveToDisk = "predictors_8km/longitude.tif", overwrite=TRUE)
      x <- rast("predictors_8km/longitude.tif")
      
      
      
      # raster merging
      pred_rast_static <- c( SoilGrids_SOC_SoilGrids_SOCstock,
                             dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m,
                             PHIHOX_M_sl1_250m_ll_SoilGrids, 
                             UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, x, y)
      
      # change raster names so that they are exactly the same as in the model file
      names(pred_rast_static) <- c( "SoilGrids_SOC_SoilGrids_SOCstock",
                                    "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m",
                                    "PHIHOX_M_sl1_250m_ll_SoilGrids", 
                                    "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", "xpred", "ypred")
      
      # convert to a data frame
      pred_rast_static_df <- as.data.frame(pred_rast_static, xy=TRUE)
      
      # remove if any of the variables have NA in the pixel
      pred_rast_static_na <- na.omit(pred_rast_static_df)
      str(pred_rast_static_na)
      
      
      
      print("static vars loaded")
      
      
    
      # loop through the time periods and load dynamic data rasters
      for (t in 1:length(time)) {
        
        # t <- 7 
        setwd("/home/master/") 
        gcs_get_object(paste0("predictors_8km_mean/", "srad_mean_2002_2016_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km_mean/", "srad_mean_2002_2016_", time_alt[t], ".tif"), overwrite=TRUE)
        srad_terraclimate_sites <- rast(paste0("predictors_8km_mean/", "srad_mean_2002_2016_", time_alt[t], ".tif"))
        #plot(srad_terraclimate_sites)
        srad_terraclimate_sites
        summary(d$srad_terraclimate_sites)
        #srad_terraclimate_sites <- srad_terraclimate_sites/10  # this was changed
        #srad_terraclimate_sites <- as.data.frame(srad_terraclimate_sites, xy=TRUE)
        # Downward surface shortwave radiation. Unit W/m2. Both need to be divided by 10 to get to the original scale
        
        
        gcs_get_object(paste0("predictors_8km_mean/", "co2_mean_2002_2016_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km_mean/", "co2_mean_2002_2016_", time_alt[t], ".tif"), overwrite=TRUE)
        Barrow_CO2_conc_Barrow_CO2conc <- rast(paste0("predictors_8km_mean/", "co2_mean_2002_2016_", time_alt[t], ".tif"))
        #plot(Barrow_CO2_conc_Barrow_CO2conc)
        Barrow_CO2_conc_Barrow_CO2conc
        summary(d$Barrow_CO2_conc_Barrow_CO2conc)
        #Barrow_CO2_conc_Barrow_CO2conc <- Barrow_CO2_conc_Barrow_CO2conc/1000  
        #Barrow_CO2_conc_Barrow_CO2conc <- as.data.frame(Barrow_CO2_conc_Barrow_CO2conc, xy=TRUE)
        # atm CO2 concentrations in ppm
        
        gcs_get_object(paste0("predictors_8km_mean/", "ndvi_gimms_mean_2002_2016_", time[t], ".tif"), saveToDisk = paste0("predictors_8km_mean/", "ndvi_gimms_mean_2002_2016_", time[t], ".tif"), overwrite=TRUE)
        ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled <- rast(paste0("predictors_8km_mean/", "ndvi_gimms_mean_2002_2016_", time[t], ".tif")) 
        #plot(NDVI_whittaker_constant_monthly_mean)
        ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled
        summary(d$ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled)
        #ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled <- ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled/10000 
        #NDVI_whittaker_constant_monthly_mean <- as.data.frame(NDVI_whittaker_constant_monthly_mean, xy=TRUE)
        
        gcs_get_object(paste0("predictors_8km_mean/", "soiltemplevel1_mean_2002_2016_", time[t], ".tif"), saveToDisk = paste0("predictors_8km_mean/", "soiltemplevel1_mean_2002_2016_", time[t], ".tif"), overwrite=TRUE)
        Soil.temperature.level.1_era5_soilmoist_temp_snow <- rast(paste0("predictors_8km_mean/", "soiltemplevel1_mean_2002_2016_", time[t], ".tif"))
        #plot(Soil.temperature.level.1_era5_soilmoist_temp_snow)
        Soil.temperature.level.1_era5_soilmoist_temp_snow
        summary(d$Soil.temperature.level.1_era5_soilmoist_temp_snow)
        #Soil.temperature.level.1_era5_soilmoist_temp_snow <- Soil.temperature.level.1_era5_soilmoist_temp_snow/100
        #Soil.temperature.level.1_era5_soilmoist_temp_snow <- as.data.frame(Soil.temperature.level.1_era5_soilmoist_temp_snow, xy=TRUE)
        # Topsoil temp. Temperature measured in kelvin can be converted to degrees Celsius (Â°C) by subtracting 273.15.
        
        gcs_get_object(paste0("predictors_8km_mean/", "tmean_mean_2002_2016_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km_mean/", "tmean_mean_2002_2016_", time_alt[t], ".tif"), overwrite=TRUE)
        tmean_terraclimate_sites <- rast(paste0("predictors_8km_mean/", "tmean_mean_2002_2016_", time_alt[t], ".tif"))
        #plot(vpd_terraclimate_sites)
        tmean_terraclimate_sites
        summary(d$tmean_terraclimate_sites)
        # tmean_terraclimate_sites <- tmean_terraclimate_sites/10 # changed  
        #tmean_terraclimate_sites <- as.data.frame(tmean_terraclimate_sites, xy=TRUE)
        #  Mean annual air temperature C degrees
        
        gcs_get_object(paste0("predictors_8km_mean/", "vpd_mean_2002_2016_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km_mean/", "vpd_mean_2002_2016_", time_alt[t], ".tif"), overwrite=TRUE)
        vpd_terraclimate_sites <- rast(paste0("predictors_8km_mean/", "vpd_mean_2002_2016_", time_alt[t], ".tif"))
        #plot(vpd_terraclimate_sites)
        vpd_terraclimate_sites
        summary(d$vpd_terraclimate_sites)
        #vpd_terraclimate_sites <- vpd_terraclimate_sites/100  ### NO CONVERSION NEEDED - checked by comparing to new extractions too
        #vpd_terraclimate_sites <- as.data.frame(vpd_terraclimate_sites, xy=TRUE)
        #  Vapor pressure deficit kpa, both have a scale factor of   0.01 so values are really -0.001-1.4 kPA
        
        
        
        # raster merge
        pred_rast_dynamic1 <- c(srad_terraclimate_sites, Barrow_CO2_conc_Barrow_CO2conc,
                                ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled, Soil.temperature.level.1_era5_soilmoist_temp_snow, vpd_terraclimate_sites,
                                tmean_terraclimate_sites) 
        
        names(pred_rast_dynamic1) <- c("srad_terraclimate_sites", "Barrow_CO2_conc_Barrow_CO2conc",
                                       "ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "vpd_terraclimate_sites",
                                       "tmean_terraclimate_sites")
        
        
        
        pred_rast_dynamic1_df <- as.data.frame(pred_rast_dynamic1, xy=TRUE)
        
        pred_rast_dynamic1_na <- na.omit(pred_rast_dynamic1_df)
        str(pred_rast_dynamic1_na)
        
        rm(srad_terraclimate_sites)
        rm(Barrow_CO2_conc_Barrow_CO2conc)
        rm(ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled)
        rm(Soil.temperature.level.1_era5_soilmoist_temp_snow)
        rm(tmean_terraclimate_sites)
        rm(vpd_terraclimate_sites)
        gc()
        
        
        
        print("done")
        
        # continue with the rest of dynamic rasters...
        gcs_get_object(paste0("predictors_8km_mean/", "snowcover_mean_2002_2016_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km_mean/", "snowcover_mean_2002_2016_", time_alt[t], ".tif"), overwrite=TRUE)
        Snow.cover_era5_soilmoist_temp_snow <- rast(paste0("predictors_8km_mean/", "snowcover_mean_2002_2016_", time_alt[t], ".tif"))
        #plot(Snow.cover_era5_soilmoist_temp_snow)
        Snow.cover_era5_soilmoist_temp_snow
        summary(d$Snow.cover_era5_soilmoist_temp_snow)
        #Snow.cover_era5_soilmoist_temp_snow <- Snow.cover_era5_soilmoist_temp_snow/100
        #Snow.cover_era5_soilmoist_temp_snow <- as.data.frame(Snow.cover_era5_soilmoist_temp_snow, xy=TRUE)
        # Snow cover %
        
        gcs_get_object(paste0("predictors_8km_mean/", "soilmoistlevel1_mean_2002_2016_", time[t], ".tif"), saveToDisk = paste0("predictors_8km_mean/", "soilmoistlevel1_mean_2002_2016_", time[t], ".tif"), overwrite=TRUE)
        Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- rast(paste0("predictors_8km_mean/", "soilmoistlevel1_mean_2002_2016_", time[t], ".tif"))
        #plot(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
        Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow
        summary(d$Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
        #Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow/100
        #Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- as.data.frame(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow, xy=TRUE)
        # volumetric water content (0-1)
        
        
        
        
        gcs_get_object(paste0("predictors_8km_mean/", "Percent_TreeCover_VCF5KYR_annual_mean_2002_2016", ".tif"), saveToDisk = paste0("predictors_8km_mean/", "Percent_TreeCover_VCF5KYR_annual_mean_2002_2016", ".tif"), overwrite=TRUE)
        Percent_TreeCover_AVHRR_VCF5KYR <- rast(paste0("predictors_8km_mean/", "Percent_TreeCover_VCF5KYR_annual_mean_2002_2016", ".tif"))
        print(Percent_TreeCover_AVHRR_VCF5KYR)
        #plot(Percent_TreeCover_AVHRR_VCF5KYR)
        Percent_TreeCover_AVHRR_VCF5KYR
        summary(d$Percent_TreeCover_AVHRR_VCF5KYR)
        #Percent_TreeCover_AVHRR_VCF5KYR <- as.data.frame(Percent_TreeCover_AVHRR_VCF5KYR, xy=TRUE)
        
        gcs_get_object(paste0("predictors_8km_mean/", "Percent_NonTree_Vegetation_VCF5KYR_annual_mean_2002_2016",  ".tif"), saveToDisk = paste0("predictors_8km_mean/", "Percent_NonTree_Vegetation_VCF5KYR_annual_mean_2002_2016",  ".tif"), overwrite=TRUE)
        Percent_NonTree_Vegetation_AVHRR_VCF5KYR <- rast(paste0("predictors_8km_mean/", "Percent_NonTree_Vegetation_VCF5KYR_annual_mean_2002_2016",  ".tif"))
        print(Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
        #plot(Percent_TreeCover_AVHRR_VCF5KYR)
        Percent_NonTree_Vegetation_AVHRR_VCF5KYR
        summary(d$Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
        #Percent_TreeCover_AVHRR_VCF5KYR <- as.data.frame(Percent_TreeCover_AVHRR_VCF5KYR, xy=TRUE)
        
        
        
        gc()
        
        
        
        print("dynamic vars pt 2 merging")
        
        pred_rast_dynamic2 <- c(Snow.cover_era5_soilmoist_temp_snow, 
                                Percent_TreeCover_AVHRR_VCF5KYR, Percent_NonTree_Vegetation_AVHRR_VCF5KYR,  Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow) 
        
        names(pred_rast_dynamic2) <- c("Snow.cover_era5_soilmoist_temp_snow", 
                                       "Percent_TreeCover_AVHRR_VCF5KYR", "Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow")
        str(pred_rast_dynamic2)
        
        
        
        pred_rast_dynamic2_df <- as.data.frame(pred_rast_dynamic2, xy=TRUE)
        
        pred_rast_dynamic2_na <- na.omit(pred_rast_dynamic2_df)
        str(pred_rast_dynamic2_na)
        
        
        gc()
        
        
        
        ### combine all
        
        print("merge dynamic 1 and 2") 
        pred_rast_dynamic <- merge(pred_rast_dynamic1_na, pred_rast_dynamic2_na, by=c("x", "y"))
        print("merge all")
        str(pred_rast_dynamic)
        str(pred_rast_static_na)
        pred_rast <- merge(pred_rast_static_na, pred_rast_dynamic, by=c("x", "y")) # rows that have NA are skipped 
        
        
        print("prediction data done")
        
        
        
        for (i in resp_vars) {
          
          #i <- "NEE_gC_m2"
          
          print("looping through resp vars")
          print(i)
          
          
          # # load the model training data: we'll use this to finalize model prediction data
          # modeldata2 <- d[,c("Study_ID_Short", "id", i, Baseline_vars_20km)]
          # modeldata1 <- na.omit(modeldata2) 
          # 
          # 
          # editing the prediction raster based on this comment https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
          # because of an error in the random forest prediction: random forest needs all the factor levels used in model training also in the prediction data
          
          # structure the prediction data so that they have the same columns
          pred_rast_final <- pred_rast[, 3:ncol(pred_rast)]
          #pred_rast_final<-pred_rast_final[names(modeldata1)[4:35]]
          # 
          # # add modeldata to the prediction data temporarily so that all factor levels are included
          # pred_rast_final <- rbind(modeldata1[1:nrow(modeldata1),4:35 ] , pred_rast_final)
          # 
          # # then convert to factors, this is a must
          # pred_rast_final$TKWP_Thermokarst <- as.factor(pred_rast_final$TKWP_Thermokarst)
          # pred_rast_final$TKHP_Thermokarst <- as.factor(pred_rast_final$TKHP_Thermokarst)
          # pred_rast_final$forest_age_class_forest_age_sites <- as.factor(pred_rast_final$forest_age_class_forest_age_sites)
          # pred_rast_final$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- as.factor(pred_rast_final$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
          
          
          # loop through the models - this loop is also unnecessary now since we only have one model
          
          for (m in models) {
            
            # m <- "qrf"
            
            print(m)
            
            
            for (v in versions) {
              
              print(paste0("/home/master/abcflux_modeling/results/", paste(i,  km, m, "train_loocv", v, sep="_"), ".rds"))
              # Load model files
              mod <- readRDS(paste0("/home/master/abcflux_modeling/results/", paste(i,  km, m, "train_loocv", v, sep="_"), ".rds"))
              mod2 = mod$finalModel #pull out the quantile regression object
              
              print("prediction started")
              
              #quantiles to get for predictions
              quantiles = c(0.1, 0.5, 0.9)
              
              #library("quantregForest")
              pred <- predict(mod2, newdata=pred_rast_final, what = quantiles) #vector of confidence intervals to predict
              
              #loop through the quantiles
              for(q in 1:length(quantiles)){
                
                # q <- 1
                this_q = quantiles[[q]]
                
                
                #get outpath
                out_path = file.path("/home/master/predictions_8km/csv", this_q)
                
                #get the dimension of interest
                sub_pred = pred[, q]
                
                sub_pred <- unname(sub_pred)
                sub_pred <- sub_pred*1000 #multiply by 1000 so that can save in integers
                sub_pred <- round(sub_pred, digits = 0)
                
                
                # remove the predictions to the model training data which were just done to fix the random forest error
                #sub_pred <- sub_pred[(nrow(modeldata1)+1):length(sub_pred)]
                
                
                print("prediction to dataframe done")
                
                # add cell coordinates and write out
                pred_matrix  <- data.matrix(data.frame(cbind(pred_rast[,1:2], sub_pred)))
                write.csv(pred_matrix, file.path(out_path, paste(i,  km, m, time[t], "train_loocv", paste0( v, ".csv"), sep="_" )), row.names=FALSE) 
                
                
                # TEMPORARY
                # # Google cloud settings -recapping these here because of some errors (Error in googleAuthR::gar_cache_setup(invalid_func = function(req) { : 
                # #cannot open file 'R/x86_64-pc-linux-gnu-library/4.2/memoise/R/memoise.rdb': No such file or directory)
                # # Auto-refreshing stale OAuth token
                # my_project_id <- "top-operand-328213"
                # gcs_list_buckets(my_project_id)
                # gcs_global_bucket("abcflux_modeling_files")
                # contents <- gcs_list_objects()
                # gcs_upload_set_limit(50000000L) # increasing data size limit for transferring data to google cloud
                # gcs_upload(file=file.path(out_path, paste(i,  km, m, time[t], "train_loocv_full_model_without_larvaloutbreak.csv", sep="_")), name = paste0("predictions_8km/", paste0(quantiles[[q]], "/"), paste(i,  km, m, time[t], "train_loocv_full_model_without_larvaloutbreak.csv", sep="_")))
                # 
                # 
                print(paste("1 km prediction raster done and written out for ", i, km, m, time[t], c))
                
                rm(pred_matrix); gc()
                #TEMPORARY
                
                filename_in=file.path(out_path, paste(i,  km, m, time[t], "train_loocv", paste0(v, ".csv"), sep="_"))
                filename_out=paste0("gs://abcflux_modeling_files/predictions_8km/csv/", paste0(quantiles[[q]], "/"), paste(i,  km, m, time[t], "train_loocv", sep="_"), paste0("_",  v, ".csv"))
                system(paste("gsutil cp", filename_in, filename_out, sep=" ")) # unix commands. use gsutils
                file.remove(filename_in)
                
                
  
                
                
                
                
                
                
              } # quantile loop
              
            } # version loop
            
            
          } # model loop
          
          print("model loop done")
          rm(mod); rm(mod2)
          
        } # resp var loop
        
        print("resp loop done")
        
        
      } # time period loop done
      
      print("time loop done")
      
      rm(pred_rast)
      rm(pred_rast_final)
      
      
      
    } # km loop done
    
    
  } # true km loop done
    
    print("km loop done")
    
    
        
      
      
  
  
  
} # crop loop done



# # remove all predictors
# file.remove(list.files("/home/master/predictors_8km/", full.names=TRUE))




# r <- read.csv(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_8km_subsets/", paste(i,  km, m, time[t],c, "train_loocv", v, sep="_"), ".csv"))
# pred_rast_static_m  <- as.matrix(r[]) 
# r <- rast(pred_rast_static_m[], type="xyz")
# # plot(r)
