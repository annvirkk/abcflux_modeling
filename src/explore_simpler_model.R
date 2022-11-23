




.libPaths("/home/master/R/x86_64-pc-linux-gnu-library/4.2")


library("dplyr")
library("caret")
library("parallel")
library("doParallel")
library("randomForest")
#install.packages("quantregForest", lib="D:/packages")  
library("quantregForest")  
library("groupdata2")  
library(googleCloudStorageR)


###### Accessing data from the cloud
my_project_id <- "top-operand-328213"
gcs_list_buckets(my_project_id)
gcs_global_bucket("abcflux_modeling_files")
gcs_list_objects("abcflux_modeling_files")
gcs_list_objects("abcflux_modeling_files/predictors_8km") # not working

### Load modeling data
setwd("/home/master/abcflux_modeling/src")
d <- read.csv("/home/master/flux_upscaling_data/results/final/modeldata_avg.csv")



sp <- vect(d, geom=c("Longitude", "Latitude"), "+proj=longlat +datum=WGS84")
crs(sp)
r <- rast("/home/master/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif")
sp <- project(sp, r)
test <- as.data.frame(sp[,2], geom="XY")

d <- data.frame(cbind(d, test[,2:3]))


### Check variable distributions - this needs to be done with all subsets
hist(d$NEE_gC_m2)
hist(d$GPP_gC_m2)
hist(d$Reco_gC_m2)





## List predictors for the models
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
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", # Permafrost (static)
                      
                       "x", "y"
                       
                       
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
                        
                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH" ,
                        
                        "x", "y"
                        
                      
)

# check that the columns exist
Baseline_vars_20km %in% colnames(d)



d2 <- subset(d, select=c("NEE_gC_m2", Baseline_vars_20km))
# make a simple GAM without categorical!

d2 <- na.omit(d2)

names(d2)

m <- randomForest(as.formula(paste("NEE_gC_m2", paste(Baseline_vars_20km, collapse=" + "), sep=" ~ ")), d2)

varImpPlot(m)







d2 <- subset(d, select=c("GPP_gC_m2", Baseline_vars_20km))
# make a simple GAM without categorical!

d2 <- na.omit(d2)

names(d2)

m2 <- randomForest(as.formula(paste("GPP_gC_m2", paste(Baseline_vars_20km, collapse=" + "), sep=" ~ ")), d2)

varImpPlot(m2)










d2 <- subset(d, select=c("Reco_gC_m2", Baseline_vars_20km))
# make a simple GAM without categorical!

d2 <- na.omit(d2)

names(d2)

m3 <- randomForest(as.formula(paste("Reco_gC_m2", paste(Baseline_vars_20km, collapse=" + "), sep=" ~ ")), d2)

varImpPlot(m3)




d2 <- subset(d, select=c("Reco_gC_m2", "NEE_gC_m2", "Study_ID_Short", Baseline_vars_20km))
sort(unique(d2$Study_ID_Short))
# make a simple GAM without categorical!

d2 <- na.omit(d2)

m4 <- randomForest(as.formula(paste("NEE_gC_m2", paste(Baseline_vars_20km, collapse=" + "), sep=" ~ ")), d2)

varImpPlot(m4)


partialPlot(m4, d2, y)






# PREDICT WITH m4!!!!! to average datasets

# static

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


# Loop through average monthly rasters
time <- seq(as.Date("1990/01/01"), as.Date("2016/12/31"), "months") 
time <- substr(time, 1, 7)
time <- sub("-", "_", sub("_", "", time, fixed=TRUE), fixed=TRUE)
time_alt <- gsub("_0", "_", time)

time <- time[1:12]
time_alt <- time_alt[1:12]
time <- substr(time, 6,7)
time_alt <- substr(time_alt, 6,7)


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
  
  
  pred <- predict(m4, newdata=pred_rast) #vector of confidence intervals to predict
  
  pred_df <- data.frame(cbind(pred_rast$x, pred_rast$y, pred))
  predr <- rast(pred_df)
  
  writeRaster(predr, paste0("/home/master/predictions_8km_mean_datasubsets/", "NEE_simple_pluscoords", time_alt[t], ".tif"), overwrite=TRUE)

}


r <- rast(list.files("/home/master/predictions_8km_mean_datasubsets", full.names=TRUE))

sum <- sum(r)
plot(sum,  type="interval", breaks=c(-250, -50, 0, 20, 50, 150))











##### SAME BUT LARGER DATASET


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
  
  
  pred <- predict(m, newdata=pred_rast) #vector of confidence intervals to predict
  
  pred_df <- data.frame(cbind(pred_rast$x, pred_rast$y, pred))
  predr <- rast(pred_df)
  
  writeRaster(predr, paste0("/home/master/predictions_8km_mean_datasubsets/", "NEE_largestdata_pluscoords", time_alt[t], ".tif"), overwrite=TRUE)
  
}


r <- rast(list.files("/home/master/predictions_8km_mean_datasubsets", full.names=TRUE, pattern="NEE_largestdata"))

sum <- sum(r)
plot(sum,  type="interval", breaks=c(-250, -50, 0, 20, 50, 150))































########## DIFFERENT MODEL WITHOUT X AND Y

m5 <- randomForest(as.formula(paste("NEE_gC_m2", paste(Baseline_vars_20km[1:14], collapse=" + "), sep=" ~ ")), d2)

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
  
  
  pred <- predict(m5, newdata=pred_rast) #vector of confidence intervals to predict
  
  pred_df <- data.frame(cbind(pred_rast$x, pred_rast$y, pred))
  predr <- rast(pred_df)
  
  writeRaster(predr, paste0("/home/master/predictions_8km_mean_datasubsets/", "NEE_simple_nocoords", time_alt[t], ".tif"), overwrite=TRUE)
  
}


r <- rast(list.files("/home/master/predictions_8km_mean_datasubsets", full.names=TRUE, pattern="NEE_simple_nocoords"))

sum2 <- sum(r)
plot(sum2,  type="interval", breaks=c(-250, -50, 0, 20, 50, 150), col=rev(rainbow(5)))

     