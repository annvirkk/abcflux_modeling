


# Packages

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

# 
# # Use these for running in Rstudio
# library("sp")
# library("ggplot2")
# library("caret")
# library("dplyr")
# library("purrr")
# library("raster")
# library("terra")
# #install.packages('terra', repos='https://rspatial.r-universe.dev', lib="/mnt/data1/boreal/avirkkala/packages")
# library(stringr)
# library(googleCloudStorageR)
# library(purrr)
# library("quantregForest")



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
d <- read.csv("/home/master/cloud/flux_upscaling_data/results/final/modeldata_avg.csv") 


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
kms <- c("20km")


### Time periods for the monthly predictions
time <- seq(as.Date("1990/01/01"), as.Date("2016/12/31"), "months") 
time <- substr(time, 1, 7)
time <- sub("-", "_", sub("_", "", time, fixed=TRUE), fixed=TRUE)
time_alt <- gsub("_0", "_", time)
# note that 1994 and 2000  AVHRR vegetation fields are lacking data for that year: https://cmr.earthdata.nasa.gov/search/concepts/C1452975608-LPDAAC_ECS.html






### 8 km predictions will not need to be cropped to several domains as the 1 km predictions so we'll just crop the rasters to their original extent
extent <- ext(rast("/home/master/cloud/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif")) %>% as.vector()
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
      setwd("/home/master/cloud/predictors_8km") 
      
      ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- rast("/home/master/cloud/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif")
      ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- terra::crop(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, terra::ext(cropped_exent))
      #ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- as.data.frame(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, xy=TRUE)
      # 1 barren tundra, 21 graminoid tundra, 30 boreal mosaic vegetation, 31 prostrate shrub tundra, 33 low-shrub tundra, 41 tundra wetland,
      # 50 Tree cover, broadleaved, evergreen, 60 Tree cover, broadleaved, deciduous, 70 Tree cover, needleleaved, evergreen
      # 80 Tree cover, needleleaved, deciduous, 90 mixed tree cover, 120 Sparse vegetation (boreal), 160 Wetland (boreal)
      # note that older versions of terra package might not read this file correctly for some reason (instead of classes there are raster counts)
      
      
      aboveground_biomass_carbon_2010_Above_belowground_biomass <- rast("abovegroundbiomass.tif")
      #plot(aboveground_biomass_carbon_2010_Above_belowground_biomass)
      aboveground_biomass_carbon_2010_Above_belowground_biomass
      summary(d$aboveground_biomass_carbon_2010_Above_belowground_biomass)
      aboveground_biomass_carbon_2010_Above_belowground_biomass <- aboveground_biomass_carbon_2010_Above_belowground_biomass/100
      aboveground_biomass_carbon_2010_Above_belowground_biomass <- terra::crop(aboveground_biomass_carbon_2010_Above_belowground_biomass, ext(cropped_exent))
      #aboveground_biomass_carbon_2010_Above_belowground_biomass <- as.data.frame(aboveground_biomass_carbon_2010_Above_belowground_biomass, xy=TRUE)
      # Unit MgC/ha
      
      tmean_TerraClimate_averages <-  rast("Terraclimate_averages_tmean.tif")
      #plot(tmean_TerraClimate_averages)
      tmean_TerraClimate_averages
      summary(d$tmean_TerraClimate_averages)
      tmean_TerraClimate_averages <- tmean_TerraClimate_averages/1000
      tmean_TerraClimate_averages <- terra::crop(tmean_TerraClimate_averages, ext(cropped_exent))
      #tmean_TerraClimate_averages <- as.data.frame(tmean_TerraClimate_averages, xy=TRUE)
      # Unit C degrees
      
      SoilGrids_SOC_SoilGrids_SOCstock <-  rast("soc.tif")
      #plot(SoilGrids_SOC_SoilGrids_SOCstock)
      SoilGrids_SOC_SoilGrids_SOCstock
      summary(d$SoilGrids_SOC_SoilGrids_SOCstock)
      SoilGrids_SOC_SoilGrids_SOCstock <- SoilGrids_SOC_SoilGrids_SOCstock/100
      SoilGrids_SOC_SoilGrids_SOCstock <- terra::crop(SoilGrids_SOC_SoilGrids_SOCstock, ext(cropped_exent))
      #SoilGrids_SOC_SoilGrids_SOCstock <- as.data.frame(SoilGrids_SOC_SoilGrids_SOCstock, xy=TRUE)
      # Unit tonnes per ha
      
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("roughscale.tif")
      #plot(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
      summary(d$dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m/100
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- terra::crop(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, ext(cropped_exent))
      #dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- as.data.frame(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, xy=TRUE)
      # see paragraph Multiscale roughness here: https://www.nature.com/articles/s41597-020-0479-6
      # Scale of the maximum multiscale deviation
      
      
      belowground_biomass_carbon_2010_Above_belowground_biomass <- rast("belowgroundbiomass.tif") 
      #plot(belowground_biomass_carbon_2010_Above_belowground_biomass)
      belowground_biomass_carbon_2010_Above_belowground_biomass
      summary(d$belowground_biomass_carbon_2010_Above_belowground_biomass)
      belowground_biomass_carbon_2010_Above_belowground_biomass <- belowground_biomass_carbon_2010_Above_belowground_biomass/100 
      belowground_biomass_carbon_2010_Above_belowground_biomass <- terra::crop(belowground_biomass_carbon_2010_Above_belowground_biomass, ext(cropped_exent))
      #belowground_biomass_carbon_2010_Above_belowground_biomass <- as.data.frame(belowground_biomass_carbon_2010_Above_belowground_biomass, xy=TRUE)
      # Unit MgC/ha
      
      
      BLDFIE_M_sl1_250m_ll_SoilGrids <- rast("bulkdensity.tif")
      #plot(BLDFIE_M_sl1_250m_ll_SoilGrids)
      BLDFIE_M_sl1_250m_ll_SoilGrids
      summary(d$BLDFIE_M_sl1_250m_ll_SoilGrids)
      BLDFIE_M_sl1_250m_ll_SoilGrids <- BLDFIE_M_sl1_250m_ll_SoilGrids/100 
      BLDFIE_M_sl1_250m_ll_SoilGrids <- terra::crop(BLDFIE_M_sl1_250m_ll_SoilGrids, ext(cropped_exent))
      #BLDFIE_M_sl1_250m_ll_SoilGrids <- as.data.frame(BLDFIE_M_sl1_250m_ll_SoilGrids, xy=TRUE)
      # kg m-3
      
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("cti.tif")
      #plot(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
      summary(d$dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m/100 
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- terra::crop(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, ext(cropped_exent))
      #dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- as.data.frame(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, xy=TRUE)
      # Compound topographic index, high value= high topographic moisture
      
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- rast("sol_watercontent.tif")
      #plot(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent
      summary(d$sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent/100 
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- terra::crop(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent, ext(cropped_exent))
      #sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- as.data.frame(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent, xy=TRUE)
      # Soil water content (volumetric %) for 1500kPa suction
      
      ppt_TerraClimate_averages <- rast("Terraclimate_averages_ppt.tif")
      #plot(ppt_TerraClimate_averages)
      ppt_TerraClimate_averages
      summary(d$ppt_TerraClimate_averages)
      ppt_TerraClimate_averages <- ppt_TerraClimate_averages/1000 
      ppt_TerraClimate_averages <- terra::crop(ppt_TerraClimate_averages, ext(cropped_exent))
      #ppt_TerraClimate_averages <- as.data.frame(ppt_TerraClimate_averages, xy=TRUE)
      # cumulative precipitation (mm)
      
      ndvi_trend_19812010 <-  rast("ndvi_trend_19812010.tif") 
      #plot(ndvi_trend_19812010)
      ndvi_trend_19812010
      summary(d$ndvi_trend_19812010)
      ndvi_trend_19812010 <- ndvi_trend_19812010/10000000 # NDVI rasters multiplied by 10000, and the trend multiplied by 1000. This was checked by new extraction too
      ndvi_trend_19812010 <- terra::crop(ndvi_trend_19812010, ext(cropped_exent))
      #ndvi_trend_19812010 <- as.data.frame(ndvi_trend_19812010, xy=TRUE)
      # NDVI trend per year
      
      PHIHOX_M_sl1_250m_ll_SoilGrids <- rast("ph.tif")
      #plot(PHIHOX_M_sl1_250m_ll_SoilGrids)
      PHIHOX_M_sl1_250m_ll_SoilGrids
      summary(d$PHIHOX_M_sl1_250m_ll_SoilGrids)
      PHIHOX_M_sl1_250m_ll_SoilGrids <- PHIHOX_M_sl1_250m_ll_SoilGrids/100 
      PHIHOX_M_sl1_250m_ll_SoilGrids <- terra::crop(PHIHOX_M_sl1_250m_ll_SoilGrids, ext(cropped_exent))
      #PHIHOX_M_sl1_250m_ll_SoilGrids <- as.data.frame(PHIHOX_M_sl1_250m_ll_SoilGrids, xy=TRUE)
      
      
      terra_trend_19601990 <- rast("tmean_trend_19601990.tif")
      #plot(terra_trend_19601990)
      terra_trend_19601990
      summary(d$terra_trend_19601990)
      terra_trend_19601990 <- terra_trend_19601990/1000
      terra_trend_19601990 <- terra::crop(terra_trend_19601990, ext(cropped_exent))
      #terra_trend_19601990 <- as.data.frame(terra_trend_19601990, xy=TRUE)
      # Unit: temperature change per year (both flux data and raster data need to be divided by 10 to come to the original scale)
      
      
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- rast("UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif")
      #plot(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH
      summary(d$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH/100 
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- terra::crop(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, ext(cropped_exent))
      #UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- as.data.frame(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, xy=TRUE)
      # Permafrost probability (fraction 0-1)
      
      TKWP_Thermokarst <- rast("Circumpolar_Thermokarst_Landscapes_TKWP.tif")
      #plot(TKWP_Thermokarst)
      summary(d$TKWP_Thermokarst)
      TKWP_Thermokarst <- terra::crop(TKWP_Thermokarst, ext(cropped_exent))
      #TKWP_Thermokarst <- as.data.frame(TKWP_Thermokarst, xy=TRUE)
      # Themokarst wetland vulnerability, 0=none, 3=high
      
      
      TKHP_Thermokarst <- rast("Circumpolar_Thermokarst_Landscapes_TKHP.tif")
      #plot(TKHP_Thermokarst)
      summary(d$TKHP_Thermokarst)
      TKHP_Thermokarst <- terra::crop(TKHP_Thermokarst, ext(cropped_exent))
      #TKHP_Thermokarst <- as.data.frame(TKHP_Thermokarst, xy=TRUE)
      # Themokarst hillslope vulnerability, 0=none, 3=high
      
      
      
      
      forest_age_class_forest_age_sites <- rast("forest_age.tif")
      #NAflag(forest_age_class_forest_age_sites)<-0 # not working in kubernetes
      #plot(forest_age_class_forest_age_sites)
      forest_age_class_forest_age_sites 
      summary(d$forest_age_class_forest_age_sites)
      forest_age_class_forest_age_sites <- terra::crop(forest_age_class_forest_age_sites, ext(cropped_exent))
      #forest_age_class_forest_age_sites <- as.data.frame(forest_age_class_forest_age_sites, xy=TRUE)
      # forest age 1=0-60 yrs, 2=60-90, 3=90-120, 4=120-400, 5=>400 or tundra
      # unfortunately not possible to use a split for e.g. 0-20 years because the youngest forests in this region and database are ~45 yrs (which seems a bit incorrect)
      
      
      
      # raster merging
      pred_rast_static <- c(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, aboveground_biomass_carbon_2010_Above_belowground_biomass,
                            tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock,
                            dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, 
                            belowground_biomass_carbon_2010_Above_belowground_biomass,
                            BLDFIE_M_sl1_250m_ll_SoilGrids, dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m,
                            sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent, ppt_TerraClimate_averages,
                            PHIHOX_M_sl1_250m_ll_SoilGrids, terra_trend_19601990,
                            UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, TKWP_Thermokarst, TKHP_Thermokarst, 
                            ndvi_trend_19812010, forest_age_class_forest_age_sites)
      
      # change raster names so that they are exactly the same as in the model file
      names(pred_rast_static) <- c("ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", "aboveground_biomass_carbon_2010_Above_belowground_biomass",
                                   "tmean_TerraClimate_averages", "SoilGrids_SOC_SoilGrids_SOCstock",
                                   "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", 
                                   "belowground_biomass_carbon_2010_Above_belowground_biomass",
                                   "BLDFIE_M_sl1_250m_ll_SoilGrids", "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m",
                                   "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", "ppt_TerraClimate_averages",
                                   "PHIHOX_M_sl1_250m_ll_SoilGrids", "terra_trend_19601990",
                                   "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", "TKWP_Thermokarst", "TKHP_Thermokarst",
                                   "ndvi_trend_19812010", "forest_age_class_forest_age_sites")
      
      # convert to a data frame
      pred_rast_static_df <- as.data.frame(pred_rast_static, xy=TRUE)
      
      # remove if any of the variables have NA in the pixel
      pred_rast_static_na <- na.omit(pred_rast_static_df)
      str(pred_rast_static_na)
      
      # check to convert back to raster - yep, works ok!
      # pred_rast_static_m  <- as.matrix(pred_rast_static_na[]) 
      # r <- rast(pred_rast_static_m[], type="xyz")
      # plot(r)
      #writeRaster(r, "pred_rast_static_na.tif", overwrite=TRUE)
      
      
      # remove the individual layers from memory - they are just taking space
      rm(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
      rm(aboveground_biomass_carbon_2010_Above_belowground_biomass)
      rm(tmean_TerraClimate_averages)
      rm(SoilGrids_SOC_SoilGrids_SOCstock)
      rm(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      rm(terra_trend_19812010)
      rm(belowground_biomass_carbon_2010_Above_belowground_biomass)
      rm(BLDFIE_M_sl1_250m_ll_SoilGrids)
      rm(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
      rm(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      rm(ppt_TerraClimate_averages)
      rm(PHIHOX_M_sl1_250m_ll_SoilGrids)
      rm(terra_trend_19601990)
      rm(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      rm(TKWP_Thermokarst)
      rm(ndvi_trend_19812010)
      rm(forest_age_class_forest_age_sites)
      gc()
      
      
      print("static vars loaded")
      
      
      
    }
    
    # loop through the time periods and load dynamic data rasters
    for (t in 1:length(time)) {
      
      # t <- 1  
      setwd("/home/master/cloud/") 
      gcs_get_object(paste0("predictors_8km/", "srad_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km/", "srad_", time_alt[t], ".tif"), overwrite=TRUE)
      srad_terraclimate_sites <- rast(paste0("/home/master/cloud/predictors_8km/", "srad_", time_alt[t], ".tif"))
      #plot(srad_terraclimate_sites)
      srad_terraclimate_sites
      summary(d$srad_terraclimate_sites)
      srad_terraclimate_sites <- srad_terraclimate_sites/10  # this was changed
      srad_terraclimate_sites <- terra::crop(srad_terraclimate_sites, ext(cropped_exent))
      #srad_terraclimate_sites <- as.data.frame(srad_terraclimate_sites, xy=TRUE)
      # Downward surface shortwave radiation. Unit W/m2. Both need to be divided by 10 to get to the original scale
      
      
      gcs_get_object(paste0("predictors_8km/", "co2_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km/", "co2_", time_alt[t], ".tif"), overwrite=TRUE)
      Barrow_CO2_conc_Barrow_CO2conc <- rast(paste0("/home/master/cloud/predictors_8km/", "co2_", time_alt[t], ".tif"))
      #plot(Barrow_CO2_conc_Barrow_CO2conc)
      Barrow_CO2_conc_Barrow_CO2conc
      summary(d$Barrow_CO2_conc_Barrow_CO2conc)
      Barrow_CO2_conc_Barrow_CO2conc <- Barrow_CO2_conc_Barrow_CO2conc/1000  
      Barrow_CO2_conc_Barrow_CO2conc <- terra::crop(Barrow_CO2_conc_Barrow_CO2conc, ext(cropped_exent))
      #Barrow_CO2_conc_Barrow_CO2conc <- as.data.frame(Barrow_CO2_conc_Barrow_CO2conc, xy=TRUE)
      # atm CO2 concentrations in ppm
      
      gcs_get_object(paste0("predictors_8km/", "ndvi_gimms_", time[t], ".tif"), saveToDisk = paste0("predictors_8km/", "ndvi_gimms_", time[t], ".tif"), overwrite=TRUE)
      ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled <- rast(paste0("/home/master/cloud/predictors_8km/","ndvi_gimms_", time[t], ".tif")) 
      #plot(NDVI_whittaker_constant_monthly_mean)
      ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled
      summary(d$ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled)
      ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled <- ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled/10000 
      ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled <- terra::crop(ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled, ext(cropped_exent))
      #NDVI_whittaker_constant_monthly_mean <- as.data.frame(NDVI_whittaker_constant_monthly_mean, xy=TRUE)
      
      gcs_get_object(paste0("predictors_8km/", "soiltemplevel1_", time[t], ".tif"), saveToDisk = paste0("predictors_8km/", "soiltemplevel1_", time[t], ".tif"), overwrite=TRUE)
      Soil.temperature.level.1_era5_soilmoist_temp_snow <- rast(paste0("/home/master/cloud/predictors_8km/","soiltemplevel1_", time[t], ".tif"))
      #plot(Soil.temperature.level.1_era5_soilmoist_temp_snow)
      Soil.temperature.level.1_era5_soilmoist_temp_snow
      summary(d$Soil.temperature.level.1_era5_soilmoist_temp_snow)
      Soil.temperature.level.1_era5_soilmoist_temp_snow <- Soil.temperature.level.1_era5_soilmoist_temp_snow/100
      Soil.temperature.level.1_era5_soilmoist_temp_snow <- terra::crop(Soil.temperature.level.1_era5_soilmoist_temp_snow, ext(cropped_exent))
      #Soil.temperature.level.1_era5_soilmoist_temp_snow <- as.data.frame(Soil.temperature.level.1_era5_soilmoist_temp_snow, xy=TRUE)
      # Topsoil temp. Temperature measured in kelvin can be converted to degrees Celsius (Â°C) by subtracting 273.15.
      
      gcs_get_object(paste0("predictors_8km/", "tmean_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km/", "tmean_", time_alt[t], ".tif"), overwrite=TRUE)
      tmean_terraclimate_sites <- rast(paste0("/home/master/cloud/predictors_8km/","tmean_", time_alt[t], ".tif"))
      #plot(vpd_terraclimate_sites)
      tmean_terraclimate_sites
      summary(d$tmean_terraclimate_sites)
      tmean_terraclimate_sites <- tmean_terraclimate_sites/10 # changed  
      tmean_terraclimate_sites <- terra::crop(tmean_terraclimate_sites, ext(cropped_exent))
      #tmean_terraclimate_sites <- as.data.frame(tmean_terraclimate_sites, xy=TRUE)
      #  Mean annual air temperature C degrees
      
      gcs_get_object(paste0("predictors_8km/", "vpd_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km/", "vpd_", time_alt[t], ".tif"), overwrite=TRUE)
      vpd_terraclimate_sites <- rast(paste0("/home/master/cloud/predictors_8km/","vpd_", time_alt[t], ".tif"))
      #plot(vpd_terraclimate_sites)
      vpd_terraclimate_sites
      summary(d$vpd_terraclimate_sites)
      #vpd_terraclimate_sites <- vpd_terraclimate_sites/100  ### NO CONVERSION NEEDED - checked by comparing to new extractions too
      vpd_terraclimate_sites <- terra::crop(vpd_terraclimate_sites, ext(cropped_exent))
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
      
      
      # remove from disk too
      file.remove(paste0("/home/master/cloud/predictors_8km/", "srad_", time_alt[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "co2_", time_alt[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "ndvi_gimms_", time[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "soiltemplevel1_", time[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "tmean_", time_alt[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "vpd_", time_alt[t], ".tif"))

      
      print("done")
      
      # continue with the rest of dynamic rasters...
      gcs_get_object(paste0("predictors_8km/", "vpd_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km/", "snowcover_", time_alt[t], ".tif"), overwrite=TRUE)
      Snow.cover_era5_soilmoist_temp_snow <- rast(paste0("/home/master/cloud/predictors_8km/","snowcover_", time_alt[t], ".tif"))
      #plot(Snow.cover_era5_soilmoist_temp_snow)
      Snow.cover_era5_soilmoist_temp_snow
      summary(d$Snow.cover_era5_soilmoist_temp_snow)
      Snow.cover_era5_soilmoist_temp_snow <- Snow.cover_era5_soilmoist_temp_snow/100
      Snow.cover_era5_soilmoist_temp_snow <- terra::crop(Snow.cover_era5_soilmoist_temp_snow, ext(cropped_exent))
      #Snow.cover_era5_soilmoist_temp_snow <- as.data.frame(Snow.cover_era5_soilmoist_temp_snow, xy=TRUE)
      # Snow cover %
      
      gcs_get_object(paste0("predictors_8km/", "soilmoistlevel1_", time[t], ".tif"), saveToDisk = paste0("predictors_8km/", "soilmoistlevel1_", time[t], ".tif"), overwrite=TRUE)
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- rast(paste0("/home/master/cloud/predictors_8km/","soilmoistlevel1_", time[t], ".tif"))
      #plot(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow
      summary(d$Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow/100
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- terra::crop(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow, ext(cropped_exent))
      #Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- as.data.frame(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow, xy=TRUE)
      # volumetric water content (0-1)
      
      
      # AVHRR fields lacking data from 1994 and 2000 - use data from the previous year in this case
      if (substr(time[t],1, 4)==1994) {
        
        gcs_get_object(paste0("predictors_8km/", "Percent_TreeCover_VCF5KYR_", 1993, "001.tif"), saveToDisk = paste0("predictors_8km/", "Percent_TreeCover_VCF5KYR_", 1993, "001.tif"), overwrite=TRUE)
        Percent_TreeCover_AVHRR_VCF5KYR <- rast(paste0("/home/master/cloud/predictors_8km/","Percent_TreeCover_VCF5KYR_", 1993, "001.tif"))
        print(Percent_TreeCover_AVHRR_VCF5KYR)
        #plot(Percent_TreeCover_AVHRR_VCF5KYR)
        Percent_TreeCover_AVHRR_VCF5KYR
        summary(d$Percent_TreeCover_AVHRR_VCF5KYR)
        Percent_TreeCover_AVHRR_VCF5KYR <- terra::crop(Percent_TreeCover_AVHRR_VCF5KYR, ext(cropped_exent))
        #Percent_TreeCover_AVHRR_VCF5KYR <- as.data.frame(Percent_TreeCover_AVHRR_VCF5KYR, xy=TRUE)
        
        gcs_get_object(paste0("predictors_8km/", "Percent_NonTree_Vegetation_VCF5KYR_", 1993, "001.tif"), saveToDisk = paste0("predictors_8km/", "Percent_NonTree_Vegetation_VCF5KYR_", 1993, "001.tif"), overwrite=TRUE)
        Percent_NonTree_Vegetation_AVHRR_VCF5KYR <- rast(paste0("/home/master/cloud/predictors_8km/","Percent_NonTree_Vegetation_VCF5KYR_", 1993, "001.tif"))
        print(Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
        #plot(Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
        Percent_NonTree_Vegetation_AVHRR_VCF5KYR
        summary(d$Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
        Percent_NonTree_Vegetation_AVHRR_VCF5KYR <- terra::crop(Percent_NonTree_Vegetation_AVHRR_VCF5KYR, ext(cropped_exent))
        #Percent_NonTree_Vegetation_AVHRR_VCF5KYR <- as.data.frame(Percent_NonTree_Vegetation_AVHRR_VCF5KYR, xy=TRUE)
        
        gcs_get_object(paste0("predictors_8km/", "Percent_NonVegetated_VCF5KYR_", 1993, "001.tif"), saveToDisk = paste0("predictors_8km/", "Percent_NonVegetated_VCF5KYR_", 1993, "001.tif"), overwrite=TRUE)
        Percent_NonVegetated_AVHRR_VCF5KYR <- rast(paste0("/home/master/cloud/predictors_8km/","Percent_NonVegetated_VCF5KYR_", 1993, "001.tif"))
        print(Percent_NonVegetated_AVHRR_VCF5KYR)
        #plot(Percent_NonVegetated_AVHRR_VCF5KYR)
        Percent_NonVegetated_AVHRR_VCF5KYR
        summary(d$Percent_NonVegetated_AVHRR_VCF5KYR)
        Percent_NonVegetated_AVHRR_VCF5KYR <- terra::crop(Percent_NonVegetated_AVHRR_VCF5KYR, ext(cropped_exent))
        #Percent_NonVegetated_AVHRR_VCF5KYR <- as.data.frame(Percent_NonVegetated_AVHRR_VCF5KYR, xy=TRUE)
        
        
      } else if (substr(time[t],1, 4)==2000) {
        gcs_get_object(paste0("predictors_8km/", "Percent_TreeCover_VCF5KYR_", 1999, "001.tif"), saveToDisk = paste0("predictors_8km/", "Percent_TreeCover_VCF5KYR_", 1999, "001.tif"), overwrite=TRUE)
        Percent_TreeCover_AVHRR_VCF5KYR <- rast(paste0("/home/master/cloud/predictors_8km/","Percent_TreeCover_VCF5KYR_", 1999, "001.tif"))
        print(Percent_TreeCover_AVHRR_VCF5KYR)
        #plot(Percent_TreeCover_AVHRR_VCF5KYR)
        Percent_TreeCover_AVHRR_VCF5KYR
        summary(d$Percent_TreeCover_AVHRR_VCF5KYR)
        Percent_TreeCover_AVHRR_VCF5KYR <- terra::crop(Percent_TreeCover_AVHRR_VCF5KYR, ext(cropped_exent))
        #Percent_TreeCover_AVHRR_VCF5KYR <- as.data.frame(Percent_TreeCover_AVHRR_VCF5KYR, xy=TRUE)
        
        gcs_get_object(paste0("predictors_8km/", "Percent_NonTree_Vegetation_VCF5KYR_", 1999, "001.tif"), saveToDisk = paste0("predictors_8km/", "Percent_NonTree_Vegetation_VCF5KYR_", 1999, "001.tif"), overwrite=TRUE)
        Percent_NonTree_Vegetation_AVHRR_VCF5KYR <- rast(paste0("/home/master/cloud/predictors_8km/","Percent_NonTree_Vegetation_VCF5KYR_", 1999, "001.tif"))
        print(Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
        #plot(Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
        Percent_NonTree_Vegetation_AVHRR_VCF5KYR
        summary(d$Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
        Percent_NonTree_Vegetation_AVHRR_VCF5KYR <- terra::crop(Percent_NonTree_Vegetation_AVHRR_VCF5KYR, ext(cropped_exent))
        #Percent_NonTree_Vegetation_AVHRR_VCF5KYR <- as.data.frame(Percent_NonTree_Vegetation_AVHRR_VCF5KYR, xy=TRUE)
        
        gcs_get_object(paste0("predictors_8km/", "Percent_NonVegetated_VCF5KYR_", 1999, "001.tif"), saveToDisk = paste0("predictors_8km/", "Percent_NonVegetated_VCF5KYR_", 1999, "001.tif"), overwrite=TRUE)
        Percent_NonVegetated_AVHRR_VCF5KYR <- rast(paste0("/home/master/cloud/predictors_8km/","Percent_NonVegetated_VCF5KYR_", 1999, "001.tif"))
        print(Percent_NonVegetated_AVHRR_VCF5KYR)
        #plot(Percent_NonVegetated_AVHRR_VCF5KYR)
        Percent_NonVegetated_AVHRR_VCF5KYR
        summary(d$Percent_NonVegetated_AVHRR_VCF5KYR)
        Percent_NonVegetated_AVHRR_VCF5KYR <- terra::crop(Percent_NonVegetated_AVHRR_VCF5KYR, ext(cropped_exent))
        #Percent_NonVegetated_AVHRR_VCF5KYR <- as.data.frame(Percent_NonVegetated_AVHRR_VCF5KYR, xy=TRUE)
        
        
      } else {
        
        gcs_get_object(paste0("predictors_8km/", "Percent_TreeCover_VCF5KYR_", substr(time[t],1, 4), "001.tif"), saveToDisk = paste0("predictors_8km/", "Percent_TreeCover_VCF5KYR_", substr(time[t],1, 4), "001.tif"), overwrite=TRUE)
        Percent_TreeCover_AVHRR_VCF5KYR <- rast(paste0("/home/master/cloud/predictors_8km/","Percent_TreeCover_VCF5KYR_", substr(time[t],1, 4), "001.tif"))
        print(Percent_TreeCover_AVHRR_VCF5KYR)
        #plot(Percent_TreeCover_AVHRR_VCF5KYR)
        Percent_TreeCover_AVHRR_VCF5KYR
        summary(d$Percent_TreeCover_AVHRR_VCF5KYR)
        Percent_TreeCover_AVHRR_VCF5KYR <- terra::crop(Percent_TreeCover_AVHRR_VCF5KYR, ext(cropped_exent))
        #Percent_TreeCover_AVHRR_VCF5KYR <- as.data.frame(Percent_TreeCover_AVHRR_VCF5KYR, xy=TRUE)
        
        gcs_get_object(paste0("predictors_8km/", "Percent_NonTree_Vegetation_VCF5KYR_", substr(time[t],1, 4), "001.tif"), saveToDisk = paste0("predictors_8km/", "Percent_NonTree_Vegetation_VCF5KYR_", substr(time[t],1, 4), "001.tif"), overwrite=TRUE)
        Percent_NonTree_Vegetation_AVHRR_VCF5KYR <- rast(paste0("/home/master/cloud/predictors_8km/","Percent_NonTree_Vegetation_VCF5KYR_", substr(time[t],1, 4), "001.tif"))
        print(Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
        #plot(Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
        Percent_NonTree_Vegetation_AVHRR_VCF5KYR
        summary(d$Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
        Percent_NonTree_Vegetation_AVHRR_VCF5KYR <- terra::crop(Percent_NonTree_Vegetation_AVHRR_VCF5KYR, ext(cropped_exent))
        #Percent_NonTree_Vegetation_AVHRR_VCF5KYR <- as.data.frame(Percent_NonTree_Vegetation_AVHRR_VCF5KYR, xy=TRUE)
        
        gcs_get_object(paste0("predictors_8km/", "Percent_NonVegetated_VCF5KYR_", substr(time[t],1, 4), "001.tif"), saveToDisk = paste0("predictors_8km/", "Percent_NonVegetated_VCF5KYR_", substr(time[t],1, 4), "001.tif"), overwrite=TRUE)
        Percent_NonVegetated_AVHRR_VCF5KYR <- rast(paste0("/home/master/cloud/predictors_8km/","Percent_NonVegetated_VCF5KYR_", substr(time[t],1, 4), "001.tif"))
        print(Percent_NonVegetated_AVHRR_VCF5KYR)
        #plot(Percent_NonVegetated_AVHRR_VCF5KYR)
        Percent_NonVegetated_AVHRR_VCF5KYR
        summary(d$Percent_NonVegetated_AVHRR_VCF5KYR)
        Percent_NonVegetated_AVHRR_VCF5KYR <- terra::crop(Percent_NonVegetated_AVHRR_VCF5KYR, ext(cropped_exent))
        #Percent_NonVegetated_AVHRR_VCF5KYR <- as.data.frame(Percent_NonVegetated_AVHRR_VCF5KYR, xy=TRUE)
        
        
        
      }
      
      
      
      
      
      
     
      
      gc()
      
      
      gcs_get_object(paste0("predictors_8km/", "tmean20yrprior_trend_", substr(time[t],1, 4), ".tif"), saveToDisk = paste0("predictors_8km/", "tmean20yrprior_trend_", substr(time[t],1, 4), ".tif"), overwrite=TRUE)
      trend_20yrprior_terra_change_id <- rast(paste0("/home/master/cloud/predictors_8km/","tmean20yrprior_trend_", substr(time[t],1, 4), ".tif"))
      #plot(trend_20yrprior_terra_change_id)
      trend_20yrprior_terra_change_id
      summary(d$trend_20yrprior_terra_change_id)
      trend_20yrprior_terra_change_id <- trend_20yrprior_terra_change_id/1000
      trend_20yrprior_terra_change_id <- terra::crop(trend_20yrprior_terra_change_id, ext(cropped_exent))
      #trend_20yrprior_terra_change_id <- as.data.frame(trend_20yrprior_terra_change_id, xy=TRUE)
      # Unit annual change in mean annual air temp during the20 prior measurement years (flux and gridded data would still need to be divided by 10 to go to the orig scale)
      
      gcs_get_object(paste0("predictors_8km/", "snowdepth_", time[t], ".tif"), saveToDisk = paste0("predictors_8km/", "snowdepth_", time[t], ".tif"), overwrite=TRUE)
      Snow.depth_era5_soilmoist_temp_snow <- rast(paste0("/home/master/cloud/predictors_8km/","snowdepth_", time[t], ".tif"))
      #plot(Snow.depth_era5_soilmoist_temp_snow)
      Snow.depth_era5_soilmoist_temp_snow
      summary(d$Snow.depth_era5_soilmoist_temp_snow)
      Snow.depth_era5_soilmoist_temp_snow <- Snow.depth_era5_soilmoist_temp_snow/100
      Snow.depth_era5_soilmoist_temp_snow <- terra::crop(Snow.depth_era5_soilmoist_temp_snow, ext(cropped_exent))
      #Snow.depth_era5_soilmoist_temp_snow <- as.data.frame(Snow.depth_era5_soilmoist_temp_snow, xy=TRUE)
      # snow depth in meters
      
      gcs_get_object(paste0("predictors_8km/", "ppt_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km/", "ppt_", time_alt[t], ".tif"), overwrite=TRUE)
      pr_terraclimate_sites <- rast(paste0("/home/master/cloud/predictors_8km/","ppt_", time_alt[t], ".tif"))
      #plot(pr_terraclimate_sites)
      pr_terraclimate_sites
      summary(d$pr_terraclimate_sites)
      pr_terraclimate_sites <- pr_terraclimate_sites/100
      pr_terraclimate_sites <- terra::crop(pr_terraclimate_sites, ext(cropped_exent))
      #pr_terraclimate_sites <- as.data.frame(pr_terraclimate_sites, xy=TRUE)
      # Monthly precipitation (mm)
      
      gcs_get_object(paste0("predictors_8km/", "pdsi_", time_alt[t], ".tif"), saveToDisk = paste0("predictors_8km/", "pdsi_", time_alt[t], ".tif"), overwrite=TRUE)
      pdsi_terraclimate_sites <- rast(paste0("/home/master/cloud/predictors_8km/","pdsi_", time_alt[t], ".tif"))
      #plot(pdsi_terraclimate_sites)
      pdsi_terraclimate_sites
      summary(d$pdsi_terraclimate_sites)
      #pdsi_terraclimate_sites <- pdsi_terraclimate_sites/100 # not needed, checked by re-extracting data
      pdsi_terraclimate_sites <- terra::crop(pdsi_terraclimate_sites, ext(cropped_exent))
      #pdsi_terraclimate_sites <- as.data.frame(pdsi_terraclimate_sites, xy=TRUE)
      
      
      
      print("dynamic vars pt 2 merging")
      
      pred_rast_dynamic2 <- c(Snow.cover_era5_soilmoist_temp_snow, 
                              trend_20yrprior_terra_change_id, Snow.depth_era5_soilmoist_temp_snow, 
                              pr_terraclimate_sites, pdsi_terraclimate_sites,
                              Percent_TreeCover_AVHRR_VCF5KYR, Percent_NonTree_Vegetation_AVHRR_VCF5KYR, Percent_NonVegetated_AVHRR_VCF5KYR, Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow) 
      
      names(pred_rast_dynamic2) <- c("Snow.cover_era5_soilmoist_temp_snow", 
                                     "trend_20yrprior_terra_change_id",
                                     "Snow.depth_era5_soilmoist_temp_snow", "pr_terraclimate_sites",
                                     "pdsi_terraclimate_sites",
                                     "Percent_TreeCover_AVHRR_VCF5KYR", "Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Percent_NonVegetated_AVHRR_VCF5KYR", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow")
      str(pred_rast_dynamic2)
      
      
      
      pred_rast_dynamic2_df <- as.data.frame(pred_rast_dynamic2, xy=TRUE)
      
      pred_rast_dynamic2_na <- na.omit(pred_rast_dynamic2_df)
      str(pred_rast_dynamic2_na)
      
      rm(Snow.cover_era5_soilmoist_temp_snow)
      rm(trend_20yrprior_terra_change_id)
      rm(Snow.depth_era5_soilmoist_temp_snow)
      rm(pr_terraclimate_sites)
      rm(pdsi_terraclimate_sites)
      rm(Percent_TreeCover_AVHRR_VCF5KYR)
      rm(Percent_NonTree_Vegetation_AVHRR_VCF5KYR)
      rm(Percent_NonVegetated_AVHRR_VCF5KYR)
      rm(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
      
      gc()
      
      
      file.remove(paste0("/home/master/cloud/predictors_8km/", "vpd_", time_alt[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "soilmoistlevel1_", time[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "Percent_TreeCover_VCF5KYR_", time[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "Percent_NonTree_Vegetation_VCF5KYR_", time[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "Percent_NonVegetated_VCF5KYR_", time[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "tmean20yrprior_trend_", substr(time[t],1, 4), ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "snowdepth_", time[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "ppt_", time_alt[t], ".tif"))
      file.remove(paste0("/home/master/cloud/predictors_8km/", "pdsi_", time_alt[t], ".tif"))
      
      
      ### combine all
      
      print("merge dynamic 1 and 2") 
      pred_rast_dynamic <- merge(pred_rast_dynamic1_na, pred_rast_dynamic2_na, by=c("x", "y"))
      print("merge all")
      str(pred_rast_dynamic)
      str(pred_rast_static_na)
      pred_rast <- merge(pred_rast_static_na, pred_rast_dynamic, by=c("x", "y")) # rows that have NA are skipped 
      
      # added in kubernetes - remove if forest age is 0, terra somehow created this value as a NA flag
      pred_rast <- subset(pred_rast, forest_age_class_forest_age_sites!=0)
      
      # edit esa cci class and thermokarst class
      pred_rast$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- ifelse(pred_rast$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged==1, 31, pred_rast$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
      # not enough tkwp sites for class 3 in 20 km models... merge 3 and 4
      pred_rast$TKWP_Thermokarst <- ifelse(pred_rast$TKWP_Thermokarst==4, 3, pred_rast$TKWP_Thermokarst)
      
      # Remove files that are not needed anymore
      #rm(pred_rast_static) # keep this in memory because we will need it later!!
      rm(pred_rast_dynamic)
      rm(pred_rast_dynamic1)
      rm(pred_rast_dynamic2)
      rm(pred_rast_dynamic1_na)
      rm(pred_rast_dynamic2_na)
      rm(pred_rast_dynamic2_df)
      rm(pred_rast_dynamic1_df)
      rm(pred_rast_static_df)
      
      gc()
      
      print("prediction data done")
      
      
      
      for (i in resp_vars) {
        
        #i <- "NEE_gC_m2"
        
        print("looping through resp vars")
        print(i)
        
        
        # load the model training data: we'll use this to finalize model prediction data
        modeldata2 <- d[,c("Study_ID_Short", "id", i, Baseline_vars_20km)]
        modeldata1 <- na.omit(modeldata2) 
        
        
        # editing the prediction raster based on this comment https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
        # because of an error in the random forest prediction: random forest needs all the factor levels used in model training also in the prediction data
        
        # structure the prediction data so that they have the same columns
        pred_rast_final <- pred_rast[, 3:ncol(pred_rast)]
        pred_rast_final<-pred_rast_final[names(modeldata1)[4:35]]
        
        # add modeldata to the prediction data temporarily so that all factor levels are included
        pred_rast_final <- rbind(modeldata1[1:nrow(modeldata1),4:35 ] , pred_rast_final)
        
        # then convert to factors, this is a must
        pred_rast_final$TKWP_Thermokarst <- as.factor(pred_rast_final$TKWP_Thermokarst)
        pred_rast_final$TKHP_Thermokarst <- as.factor(pred_rast_final$TKHP_Thermokarst)
        pred_rast_final$forest_age_class_forest_age_sites <- as.factor(pred_rast_final$forest_age_class_forest_age_sites)
        pred_rast_final$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- as.factor(pred_rast_final$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
        
        
        # loop through the models - this loop is also unnecessary now since we only have one model
        
        for (m in models) {
          
          # m <- "qrf"
          
          print(m)
          
          # Load model files
          mod <- readRDS(paste0("/home/master/abcflux_modeling/results/", paste(i,  km, m, "train_loocv_full_model_without_larvaloutbreak", sep="_"), ".rds"))
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
            out_path = file.path("/home/master/local_outputs/predictions_8km/csv", this_q)
            
            #get the dimension of interest
            sub_pred = pred[, q]
            
            sub_pred <- unname(sub_pred)
            sub_pred <- sub_pred*1000 #multiply by 1000 so that can save in integers
            sub_pred <- round(sub_pred, digits = 0)
            
            
            # remove the predictions to the model training data which were just done to fix the random forest error
            sub_pred <- sub_pred[(nrow(modeldata1)+1):length(sub_pred)]
            
            
            print("prediction to dataframe done")
            
            # add cell coordinates and write out
            pred_matrix  <- data.matrix(data.frame(cbind(pred_rast[,1:2], sub_pred)))
            write.csv(pred_matrix, file.path(out_path, paste(i,  km, m, time[t], "train_loocv_full_model_without_larvaloutbreak.csv", sep="_")), row.names=FALSE) 
            
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
            #file.remove(file.path(out_path, paste(i,  km, m, time[t], "train_loocv_full_model_without_larvaloutbreak.csv", sep="_")))
            
            
            
            
            
          }
          
          
        } # model loop
        
        print("model loop done")
        rm(mod); rm(mod2)
        
      } # resp var loop
      
      print("resp loop done")
      
      
    } # time period loop done
    
    print("model loop done")
    
    rm(pred_rast)
    rm(pred_rast_final)
    
    
    
  } # km loop done
  
  print("km loop done")

    
    
    
  

} # crop loop done

setwd("/home/master/cloud") 
gcs_get_object("predictions_8km/0.5/NEE_gC_m2_20km_qrf_1990_01_train_loocv_full_model_without_larvaloutbreak.csv", saveToDisk = "predictions_8km/0.5/NEE_gC_m2_20km_qrf_1990_01_train_loocv_full_model_without_larvaloutbreak.csv")

abcflux_modeling_files/predictions_8km/0.5/GPP_gC_m2_20km_qrf_1990_07_train_loocv_full_model_without_larvaloutbreak.csv
# r <- read.csv("predictions_8km/0.5/NEE_gC_m2_20km_qrf_1990_01_train_loocv_full_model_without_larvaloutbreak.csv")
# pred_rast_static_m  <- as.matrix(r[]) 
# r <- rast(pred_rast_static_m[], type="xyz")
# # plot(r)
