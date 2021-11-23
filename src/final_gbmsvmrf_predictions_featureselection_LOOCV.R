


########### ANNA MUSITA TÄMÄ https://stackoverflow.com/questions/25121725/error-in-predicting-raster-with-randomforest-caret-and-factor-variables

# Packages
library("caret")
library("dplyr")
library("purrr")
library("raster")
library("terra")
#library("terra", lib.loc="/mnt/data1/boreal/avirkkala/packages")
library("terra", lib.loc="/mnt/data1/boreal/avirkkala")
#library("terra", lib.loc="/tmp/RtmpChZoVD/downloaded_packages")
library(stringr)

install.packages('terra', repos='https://rspatial.r-universe.dev', lib="/mnt/data1/boreal/avirkkala/packages")

# install.packages("gbm", lib="/mnt/data1/boreal/avirkkala/packages")
# install.packages("kernlab", lib="/mnt/data1/boreal/avirkkala/packages")
# install.packages("randomForest", lib="/mnt/data1/boreal/avirkkala/packages")



terraOptions(memfrac=0.9, tempdir = "/mnt/data1/boreal/avirkkala/Temp") # testing a different style


### Data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv") # remember that fire classes rcl


# for testing purposes
xy <- data.frame(lon=d$Longitude, lat=d$Latitude, site=d$Study_ID_Short)
sp <- vect(xy, crs="+proj=longlat +datum=WGS84 +no_defs ", geom=c("lon", "lat"))
sp <- project(sp, ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)

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


### Cropping extents: 4
# crops <- rbind(c(-4834843, 4834843, -4834843, 0),  
#                c(-4834843, 4834843,  0, 4834843))
# 
# 
# crops <- rbind(c(-4834843, 0, -4834843, 0),
#                c(0, 4834843, -4834843, 0),
#                c(-4834843, 0, 0, 4834843),
#                c(0, 4834843, 0, 4834843))



# split raster to pieces: https://stackoverflow.com/questions/29784829/r-raster-package-split-image-into-multiples
SplitRas <- function(raster,ppside,save,plot){
  h        <- ceiling(ncol(raster)/ppside)
  v        <- ceiling(nrow(raster)/ppside)
  agg      <- aggregate(raster,fact=c(h,v))
  agg[]    <- 1:ncell(agg)
  agg_poly <- rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()
  for(i in 1:ncell(agg)){
    e1          <- extent(agg_poly[agg_poly$polis==i,])
    r_list[[i]] <- crop(raster,e1)
  }
  if(save==T){
    for(i in 1:length(r_list)){
      writeRaster(r_list[[i]],filename=paste("SplitRas",i,sep=""),
                  format="GTiff",datatype="FLT4S",overwrite=TRUE)  
    }
  }
  if(plot==T){
    par(mfrow=c(ppside,ppside))
    for(i in 1:length(r_list)){
      plot(r_list[[i]],axes=F,legend=F,bty="n",box=FALSE)  
    }
  }
  return(r_list)
}

setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_1km")

esa <- raster("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")

r <- SplitRas(raster=esa,ppside=3,save=TRUE,plot=FALSE)

# get extents
for (i in 1:9) {
  
  print(i)
  # i <- 1
  
  if (i==1) {
    crops <- cbind(extent(r[[i]])@xmin, extent(r[[i]])@xmax, extent(r[[i]])@ymin, extent(r[[i]])@ymax)
  } else {
    crops2 <- cbind(extent(r[[i]])@xmin, extent(r[[i]])@xmax, extent(r[[i]])@ymin, extent(r[[i]])@ymax)
    crops <- rbind(crops, crops2)
  }
}

library("terra")

# creating predictions in two parts: NA and Siberia
### Loop over times and load dynamic vars


print("starting to loop over the files")

for (c in 1:nrow(crops)) {
  # c <- 3
  print(c)
  
  for (km in kms) {
    
    print(km)
    # km <- "1km"
    # i <- "NEE_gC_m2"
    
    
    if (km=="1km") {
      
      ### Load static vars (only once)
      setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_1km")
      
      ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")
      ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- terra::crop(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, ext(crops[c,]))
      ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- as.data.frame(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, xy=TRUE)
      # 1 barren tundra, 21 graminoid tundra, 30 boreal mosaic vegetation, 31 prostrate shrub tundra, 33 low-shrub tundra, 41 tundra wetland, 
      # 50 Tree cover, broadleaved, evergreen, 60 Tree cover, broadleaved, deciduous, 70 Tree cover, needleleaved, evergreen
      # 80 Tree cover, needleleaved, deciduous, 90 mixed tree cover, 120 Sparse vegetation (boreal), 160 Wetland (boreal)
      
      
      aboveground_biomass_carbon_2010_Above_belowground_biomass <- rast("abovegroundbiomass.tif")
      plot(aboveground_biomass_carbon_2010_Above_belowground_biomass)
      aboveground_biomass_carbon_2010_Above_belowground_biomass
      summary(d$aboveground_biomass_carbon_2010_Above_belowground_biomass)
      aboveground_biomass_carbon_2010_Above_belowground_biomass <- aboveground_biomass_carbon_2010_Above_belowground_biomass/100
      aboveground_biomass_carbon_2010_Above_belowground_biomass <- crop(aboveground_biomass_carbon_2010_Above_belowground_biomass, ext(crops[c,]))
      aboveground_biomass_carbon_2010_Above_belowground_biomass <- as.data.frame(aboveground_biomass_carbon_2010_Above_belowground_biomass, xy=TRUE)
      # Unit MgC/ha
      
      tmean_TerraClimate_averages <-  rast("Terraclimate_averages_tmean.tif")
      plot(tmean_TerraClimate_averages)
      tmean_TerraClimate_averages
      summary(d$tmean_TerraClimate_averages)
      tmean_TerraClimate_averages <- tmean_TerraClimate_averages/1000
      tmean_TerraClimate_averages <- crop(tmean_TerraClimate_averages, ext(crops[c,]))
      tmean_TerraClimate_averages <- as.data.frame(tmean_TerraClimate_averages, xy=TRUE)
      # Unit C degrees
      
      SoilGrids_SOC_SoilGrids_SOCstock <-  rast("soc.tif")
      plot(SoilGrids_SOC_SoilGrids_SOCstock)
      SoilGrids_SOC_SoilGrids_SOCstock
      summary(d$SoilGrids_SOC_SoilGrids_SOCstock)
      SoilGrids_SOC_SoilGrids_SOCstock <- SoilGrids_SOC_SoilGrids_SOCstock/100
      SoilGrids_SOC_SoilGrids_SOCstock <- crop(SoilGrids_SOC_SoilGrids_SOCstock, ext(crops[c,]))
      SoilGrids_SOC_SoilGrids_SOCstock <- as.data.frame(SoilGrids_SOC_SoilGrids_SOCstock, xy=TRUE)
      # Unit tonnes per ha
      
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("roughscale.tif")
      plot(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
      summary(d$dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m/100
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- crop(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, ext(crops[c,]))
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- as.data.frame(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, xy=TRUE)
      # see paragraph Multiscale roughness here: https://www.nature.com/articles/s41597-020-0479-6
      # Scale of the maximum multiscale deviation
      
      terra_trend_19812010<- rast("tmean_trend_19812010.tif")
      plot(terra_trend_19812010)
      terra_trend_19812010
      summary(d$terra_trend_19812010) 
      terra_trend_19812010 <- terra_trend_19812010/1000 # checked this by new extractions
      terra_trend_19812010 <- crop(terra_trend_19812010, ext(crops[c,]))
      terra_trend_19812010 <- as.data.frame(terra_trend_19812010, xy=TRUE)
      # Unit: temperature change per year (both flux data and raster data need to be divided by 10 to come to the original scale)
    
      
      belowground_biomass_carbon_2010_Above_belowground_biomass <- rast("belowgroundbiomass.tif") 
      plot(belowground_biomass_carbon_2010_Above_belowground_biomass)
      belowground_biomass_carbon_2010_Above_belowground_biomass
      summary(d$belowground_biomass_carbon_2010_Above_belowground_biomass)
      belowground_biomass_carbon_2010_Above_belowground_biomass <- belowground_biomass_carbon_2010_Above_belowground_biomass/100 
      belowground_biomass_carbon_2010_Above_belowground_biomass <- crop(belowground_biomass_carbon_2010_Above_belowground_biomass, ext(crops[c,]))
      belowground_biomass_carbon_2010_Above_belowground_biomass <- as.data.frame(belowground_biomass_carbon_2010_Above_belowground_biomass, xy=TRUE)
      # Unit MgC/ha
      
      
      BLDFIE_M_sl1_250m_ll_SoilGrids <- rast("bulkdensity.tif")
      plot(BLDFIE_M_sl1_250m_ll_SoilGrids)
      BLDFIE_M_sl1_250m_ll_SoilGrids
      summary(d$BLDFIE_M_sl1_250m_ll_SoilGrids)
      BLDFIE_M_sl1_250m_ll_SoilGrids <- BLDFIE_M_sl1_250m_ll_SoilGrids/100 
      BLDFIE_M_sl1_250m_ll_SoilGrids <- crop(BLDFIE_M_sl1_250m_ll_SoilGrids, ext(crops[c,]))
      BLDFIE_M_sl1_250m_ll_SoilGrids <- as.data.frame(BLDFIE_M_sl1_250m_ll_SoilGrids, xy=TRUE)
      # kg m-3
      
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("cti.tif")
      plot(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
      summary(d$dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m/100 
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- crop(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, ext(crops[c,]))
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- as.data.frame(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, xy=TRUE)
      # Compound topographic index, high value= high topographic moisture
      
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- rast("sol_watercontent.tif")
      plot(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent
      summary(d$sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent/100 
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- crop(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent, ext(crops[c,]))
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- as.data.frame(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent, xy=TRUE)
      # Soil water content (volumetric %) for 1500kPa suction
      
      ppt_TerraClimate_averages <- rast("Terraclimate_averages_ppt.tif")
      plot(ppt_TerraClimate_averages)
      ppt_TerraClimate_averages
      summary(d$ppt_TerraClimate_averages)
      ppt_TerraClimate_averages <- ppt_TerraClimate_averages/100 
      ppt_TerraClimate_averages <- crop(ppt_TerraClimate_averages, ext(crops[c,]))
      ppt_TerraClimate_averages <- as.data.frame(ppt_TerraClimate_averages, xy=TRUE)
      # cumulative precipitation (mm)
      
      ndvi_trend_19812010 <-  rast("ndvi_trend_19812010.tif") 
      plot(ndvi_trend_19812010)
      ndvi_trend_19812010
      summary(d$ndvi_trend_19812010)
      ndvi_trend_19812010 <- ndvi_trend_19812010/10000000 # NDVI rasters multiplied by 10000, and the trend multiplied by 1000. This was checked by new extraction too
      ndvi_trend_19812010 <- crop(ndvi_trend_19812010, ext(crops[c,]))
      ndvi_trend_19812010 <- as.data.frame(ndvi_trend_19812010, xy=TRUE)
      # NDVI trend per year
      
      PHIHOX_M_sl1_250m_ll_SoilGrids <- rast("ph.tif")
      plot(PHIHOX_M_sl1_250m_ll_SoilGrids)
      PHIHOX_M_sl1_250m_ll_SoilGrids
      summary(d$PHIHOX_M_sl1_250m_ll_SoilGrids)
      PHIHOX_M_sl1_250m_ll_SoilGrids <- PHIHOX_M_sl1_250m_ll_SoilGrids/100 
      PHIHOX_M_sl1_250m_ll_SoilGrids <- crop(PHIHOX_M_sl1_250m_ll_SoilGrids, ext(crops[c,]))
      PHIHOX_M_sl1_250m_ll_SoilGrids <- as.data.frame(PHIHOX_M_sl1_250m_ll_SoilGrids, xy=TRUE)
      
      
      terra_trend_19601990 <- rast("tmean_trend_19601990.tif")
      plot(terra_trend_19601990)
      terra_trend_19601990
      summary(d$terra_trend_19601990)
      terra_trend_19601990 <- terra_trend_19601990/1000
      terra_trend_19601990 <- crop(terra_trend_19601990, ext(crops[c,]))
      terra_trend_19601990 <- as.data.frame(terra_trend_19601990, xy=TRUE)
      # Unit: temperature change per year (both flux data and raster data need to be divided by 10 to come to the original scale)
      
      
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- rast("UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif")
      plot(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH
      summary(d$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH/100 
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- crop(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, ext(crops[c,]))
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- as.data.frame(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, xy=TRUE)
      # Permafrost probability (fraction 0-1)
      
      TKWP_Thermokarst <- rast("Circumpolar_Thermokarst_Landscapes_TKWP.tif")
      plot(TKWP_Thermokarst)
      summary(d$TKWP_Thermokarst)
      ########## TEMPORARY!
      #TKWP_Thermokarst  <- classify(TKWP_Thermokarst, cbind(3, 4))
      TKWP_Thermokarst <- crop(TKWP_Thermokarst, ext(crops[c,]))
      TKWP_Thermokarst <- as.data.frame(TKWP_Thermokarst, xy=TRUE)
      # Themokarst wetland vulnerability, 0=low, 4=high
      
      
      
      
      TKHP_Thermokarst <- rast("Circumpolar_Thermokarst_Landscapes_TKHP.tif")
      plot(TKHP_Thermokarst)
      summary(d$TKHP_Thermokarst)
      TKHP_Thermokarst <- crop(TKHP_Thermokarst, ext(crops[c,]))
      TKHP_Thermokarst <- as.data.frame(TKHP_Thermokarst, xy=TRUE)
      # Themokarst wetland vulnerability, 0=low, 4=high
      
      
      
      
      forest_age_class_forest_age_sites <- rast("forest_age.tif")
      plot(forest_age_class_forest_age_sites)
      forest_age_class_forest_age_sites 
      summary(d$forest_age_class_forest_age_sites)
      forest_age_class_forest_age_sites <- crop(forest_age_class_forest_age_sites, ext(crops[c,]))
      forest_age_class_forest_age_sites <- as.data.frame(forest_age_class_forest_age_sites, xy=TRUE)
      # forest age 1=0-60 yrs, 2=60-90, 3=90-120, 4=120-400, 5=>400 or tundra
      # unfortunately not possible to use a split for e.g. 0-20 years because the youngest forests in this region and database are ~45 yrs (which seems a bit incorrect)
      
      
      Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- rast("fire.tif")
      plot(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
      summary(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
      Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- crop(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned, ext(crops[c,]))
      Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- as.data.frame(Number_of_days_since_fire_classes_MCD64A1_sites_cleaned, xy=TRUE)
      
      # merge
      pred_rast_static <- list(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, aboveground_biomass_carbon_2010_Above_belowground_biomass,
                               tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock,
                               dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, terra_trend_19812010,
                               wtd_Water_table_depth, belowground_biomass_carbon_2010_Above_belowground_biomass,
                               BLDFIE_M_sl1_250m_ll_SoilGrids, dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m,
                               sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent, ppt_TerraClimate_averages,
                               PHIHOX_M_sl1_250m_ll_SoilGrids, terra_trend_19601990,
                               UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, TKWP_Thermokarst, TKHP_Thermokarst, 
                               ndvi_trend_19812010, forest_age_class_forest_age_sites, Number_of_days_since_fire_classes_MCD64A1_sites_cleaned) %>% reduce(full_join, by = c("x", "y"))
      
      # rename
      names(pred_rast_static) <- c("x", "y", "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", "aboveground_biomass_carbon_2010_Above_belowground_biomass",
        "tmean_TerraClimate_averages", "SoilGrids_SOC_SoilGrids_SOCstock",
        "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "terra_trend_19812010",
        "wtd_Water_table_depth", "belowground_biomass_carbon_2010_Above_belowground_biomass",
        "BLDFIE_M_sl1_250m_ll_SoilGrids", "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m",
        "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", "ppt_TerraClimate_averages",
        "PHIHOX_M_sl1_250m_ll_SoilGrids", "terra_trend_19601990",
        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", "TKWP_Thermokarst", "TKHP_Thermokarst",
        "ndvi_trend_19812010", "forest_age_class_forest_age_sites", "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned")
      
      # remove NAs
      pred_rast_static_na <- na.omit(pred_rast_static)
      str(pred_rast_static_na)
      
      
      # check to convert back to raster - yep, works ok!
      # pred_rast_static_m  <- as.matrix(pred_rast_static_na[]) 
      # r <- rast(pred_rast_static_m[,c(1:2, 5)], type="xyz")
      # plot(r)
      
     
      # remove the individual layers - they are just taking space
      rm(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
      rm(aboveground_biomass_carbon_2010_Above_belowground_biomass)
      rm(tmean_TerraClimate_averages)
      rm(SoilGrids_SOC_SoilGrids_SOCstock)
      rm(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      rm(terra_trend_19812010)
      rm(wtd_Water_table_depth)
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
    
    for (t in 1:length(time)) {
      
      # t <- 1
      srad_terraclimate_sites <- rast(paste0("srad_", time_alt[t], ".tif"))
      plot(srad_terraclimate_sites)
      srad_terraclimate_sites
      summary(d$srad_terraclimate_sites)
      #srad_terraclimate_sites <- srad_terraclimate_sites/100  ### no conversion needed
      srad_terraclimate_sites <- crop(srad_terraclimate_sites, ext(crops[c,]))
      srad_terraclimate_sites <- as.data.frame(srad_terraclimate_sites, xy=TRUE)
      # Downward surface shortwave radiation. Unit W/m2. Both need to be divided by 10 to get to the original scale
      
      
      Barrow_CO2_conc_Barrow_CO2conc <- rast(paste0("co2_", time_alt[t], ".tif"))
      plot(Barrow_CO2_conc_Barrow_CO2conc)
      Barrow_CO2_conc_Barrow_CO2conc
      summary(d$Barrow_CO2_conc_Barrow_CO2conc)
      Barrow_CO2_conc_Barrow_CO2conc <- Barrow_CO2_conc_Barrow_CO2conc/1000  
      Barrow_CO2_conc_Barrow_CO2conc <- crop(Barrow_CO2_conc_Barrow_CO2conc, ext(crops[c,]))
      Barrow_CO2_conc_Barrow_CO2conc <- as.data.frame(Barrow_CO2_conc_Barrow_CO2conc, xy=TRUE)
      # atm CO2 concentrations in ppm
      
      NDVI_whittaker_constant_monthly_mean <- rast(paste0("ndvi_", time_alt[t], ".tif")) ## EI ARVOJA plotissa mutta valuesin kautta tulee??? mutta on arvoja raster-komennon kautta plotissa???
      plot(NDVI_whittaker_constant_monthly_mean)
      NDVI_whittaker_constant_monthly_mean
      summary(d$NDVI_whittaker_constant_monthly_mean)
      NDVI_whittaker_constant_monthly_mean <- NDVI_whittaker_constant_monthly_mean/1000
      NDVI_whittaker_constant_monthly_mean <- crop(NDVI_whittaker_constant_monthly_mean, ext(crops[c,]))
      NDVI_whittaker_constant_monthly_mean <- as.data.frame(NDVI_whittaker_constant_monthly_mean, xy=TRUE)
      
      Soil.temperature.level.1_era5_soilmoist_temp_snow <- rast(paste0("soiltemplevel1_", time[t], ".tif"))
      plot(Soil.temperature.level.1_era5_soilmoist_temp_snow)
      Soil.temperature.level.1_era5_soilmoist_temp_snow
      summary(d$Soil.temperature.level.1_era5_soilmoist_temp_snow)
      Soil.temperature.level.1_era5_soilmoist_temp_snow <- Soil.temperature.level.1_era5_soilmoist_temp_snow/100
      Soil.temperature.level.1_era5_soilmoist_temp_snow <- crop(Soil.temperature.level.1_era5_soilmoist_temp_snow, ext(crops[c,]))
      Soil.temperature.level.1_era5_soilmoist_temp_snow <- as.data.frame(Soil.temperature.level.1_era5_soilmoist_temp_snow, xy=TRUE)
      # Topsoil temp. Temperature measured in kelvin can be converted to degrees Celsius (°C) by subtracting 273.15.
      
      water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality <- rast(paste0("ndii", substr(time[t],1, 4), ".tif"))
      plot(water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality)
      water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality
      summary(d$water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality)
      water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality <- water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality/100
      water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality <- crop(water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality, ext(crops[c,]))
      water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality <- as.data.frame(water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality, xy=TRUE)
      
      water_ground_MCD43A4_annual_water_ground_sites_low_quality <- rast(paste0("ndwi", substr(time[t],1, 4), ".tif"))
      plot(water_ground_MCD43A4_annual_water_ground_sites_low_quality)
      water_ground_MCD43A4_annual_water_ground_sites_low_quality
      summary(d$water_ground_MCD43A4_annual_water_ground_sites_low_quality)
      water_ground_MCD43A4_annual_water_ground_sites_low_quality <- water_ground_MCD43A4_annual_water_ground_sites_low_quality/100
      water_ground_MCD43A4_annual_water_ground_sites_low_quality <- crop(water_ground_MCD43A4_annual_water_ground_sites_low_quality, ext(crops[c,]))
      water_ground_MCD43A4_annual_water_ground_sites_low_quality <- as.data.frame(water_ground_MCD43A4_annual_water_ground_sites_low_quality, xy=TRUE)
      
      LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality <- rast(paste0("lst_", time_alt[t], ".tif"))
      plot(LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality)
      LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality
      summary(d$LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality)
      LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality <- LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality/1000
      LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality <- crop(LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality, ext(crops[c,]))
      LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality <- as.data.frame(LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality, xy=TRUE)
      
      vpd_terraclimate_sites <- rast(paste0("vpd_", time_alt[t], ".tif"))
      plot(vpd_terraclimate_sites)
      vpd_terraclimate_sites
      summary(d$vpd_terraclimate_sites)
      #vpd_terraclimate_sites <- vpd_terraclimate_sites/100  ### NO CONVERSION NEEDED - checked by comparing to new extractions too
      vpd_terraclimate_sites <- crop(vpd_terraclimate_sites, ext(crops[c,]))
      vpd_terraclimate_sites <- as.data.frame(vpd_terraclimate_sites, xy=TRUE)
      #  Vapor pressure deficit kpa, both have a scale factor of   0.01 so values are really -0.001-1.4 kPA
      
      
      
      # merge pt 1
      print("merging dynamic rasters pt 1")
      
      pred_rast_dynamic1 <- list(srad_terraclimate_sites, Barrow_CO2_conc_Barrow_CO2conc,
                                 NDVI_whittaker_constant_monthly_mean, Soil.temperature.level.1_era5_soilmoist_temp_snow,
                                 water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality,
                                 water_ground_MCD43A4_annual_water_ground_sites_low_quality,
                                 LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality, vpd_terraclimate_sites) %>% reduce(full_join, by = c("x", "y"))
      
      names(pred_rast_dynamic1) <- c("x", "y", "srad_terraclimate_sites", "Barrow_CO2_conc_Barrow_CO2conc",
                                     "NDVI_whittaker_constant_monthly_mean", "Soil.temperature.level.1_era5_soilmoist_temp_snow",
                                     "water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality",
                                     "water_ground_MCD43A4_annual_water_ground_sites_low_quality",
                                     "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", "vpd_terraclimate_sites")
      
      pred_rast_dynamic1_na <- na.omit(pred_rast_dynamic1)
      str(pred_rast_dynamic1_na)
      
      
      # remove the individual layers - they are just taking space
      rm(srad_terraclimate_sites)
      rm(Barrow_CO2_conc_Barrow_CO2conc)
      rm(NDVI_whittaker_constant_monthly_mean)
      rm(Soil.temperature.level.1_era5_soilmoist_temp_snow)
      rm(water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality)
      rm(water_ground_MCD43A4_annual_water_ground_sites_low_quality)
      rm(LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality)
      rm(vpd_terraclimate_sites)
      gc()
      
      print("done")
      
      # continue...
      Snow.cover_era5_soilmoist_temp_snow <- rast(paste0("snowcover_", time[t], ".tif"))
      plot(Snow.cover_era5_soilmoist_temp_snow)
      Snow.cover_era5_soilmoist_temp_snow
      summary(d$Snow.cover_era5_soilmoist_temp_snow)
      Snow.cover_era5_soilmoist_temp_snow <- Snow.cover_era5_soilmoist_temp_snow/100
      Snow.cover_era5_soilmoist_temp_snow <- crop(Snow.cover_era5_soilmoist_temp_snow, ext(crops[c,]))
      Snow.cover_era5_soilmoist_temp_snow <- as.data.frame(Snow.cover_era5_soilmoist_temp_snow, xy=TRUE)
      # Snow cover %
      
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- rast(paste0("soilmoistlevel1_", time[t], ".tif"))
      plot(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow
      summary(d$Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow/100
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- crop(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow, ext(crops[c,]))
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- as.data.frame(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow, xy=TRUE)
      # volumetric water content (0-1)
      
      Percent_Tree_Cover_MOD44B_sites <- raster::stack(paste0("Percent_Tree_Cover_mod44b", substr(time[t],1, 4), ".tif")) # THE FIRST LAYER!!!!
      Percent_Tree_Cover_MOD44B_sites <- Percent_Tree_Cover_MOD44B_sites[[1]]
      Percent_Tree_Cover_MOD44B_sites <- rast(Percent_Tree_Cover_MOD44B_sites)
      Percent_Tree_Cover_MOD44B_sites <- Percent_Tree_Cover_MOD44B_sites[[1]]
      print(Percent_Tree_Cover_MOD44B_sites)
      plot(Percent_Tree_Cover_MOD44B_sites)
      Percent_Tree_Cover_MOD44B_sites
      summary(d$Percent_Tree_Cover_MOD44B_sites)
      Percent_Tree_Cover_MOD44B_sites <- crop(Percent_Tree_Cover_MOD44B_sites, ext(crops[c,]))
      Percent_Tree_Cover_MOD44B_sites <- as.data.frame(Percent_Tree_Cover_MOD44B_sites, xy=TRUE)
      
      
      Percent_NonTree_Vegetation_MOD44B_sites <- raster::stack(paste0("Percent_Tree_Cover_mod44b", substr(time[t],1, 4), ".tif"))  # THE SECOND LAYER!!!! (non-vegetated is the third)
      Percent_NonTree_Vegetation_MOD44B_sites <- Percent_NonTree_Vegetation_MOD44B_sites[[2]]
      Percent_NonTree_Vegetation_MOD44B_sites <- rast(Percent_NonTree_Vegetation_MOD44B_sites)
      Percent_NonTree_Vegetation_MOD44B_sites <- Percent_NonTree_Vegetation_MOD44B_sites[[2]]
      print(Percent_NonTree_Vegetation_MOD44B_sites)
      plot(Percent_NonTree_Vegetation_MOD44B_sites)
      Percent_NonTree_Vegetation_MOD44B_sites
      summary(d$Percent_NonTree_Vegetation_MOD44B_sites)
      Percent_NonTree_Vegetation_MOD44B_sites <- crop(Percent_NonTree_Vegetation_MOD44B_sites, ext(crops[c,]))
      Percent_NonTree_Vegetation_MOD44B_sites <- as.data.frame(Percent_NonTree_Vegetation_MOD44B_sites, xy=TRUE)
      
      Percent_NonVegetated_MOD44B_sites <- raster::stack(paste0("Percent_Tree_Cover_mod44b", substr(time[t],1, 4), ".tif"))  # THE THIRD LAYER!!!! (non-vegetated is the third)
      Percent_NonVegetated_MOD44B_sites <- Percent_NonVegetated_MOD44B_sites[[3]]
      Percent_NonVegetated_MOD44B_sites <- rast(Percent_NonVegetated_MOD44B_sites)
      Percent_NonVegetated_MOD44B_sites <- Percent_NonVegetated_MOD44B_sites[[3]]
      print(Percent_NonVegetated_MOD44B_sites)
      plot(Percent_NonVegetated_MOD44B_sites)
      Percent_NonVegetated_MOD44B_sites
      summary(d$Percent_NonVegetated_MOD44B_sites)
      Percent_NonVegetated_MOD44B_sites <- crop(Percent_NonVegetated_MOD44B_sites, ext(crops[c,]))
      Percent_NonVegetated_MOD44B_sites <- as.data.frame(Percent_NonVegetated_MOD44B_sites, xy=TRUE)
      
      terra_trend_10yrprior_terra_change_id <- rast(paste0("tmean10yrprior_trend_", substr(time[t],1, 4), ".tif"))
      plot(terra_trend_10yrprior_terra_change_id)
      terra_trend_10yrprior_terra_change_id
      summary(d$terra_trend_10yrprior_terra_change_id)
      terra_trend_10yrprior_terra_change_id <- terra_trend_10yrprior_terra_change_id/1000 # added an extra 100 to the processing (originally divided only by 10) 
      #terra_trend_10yrprior_terra_change_id <- crop(terra_trend_10yrprior_terra_change_id, ext(crops[c,]))
      terra_trend_10yrprior_terra_change_id <- as.data.frame(terra_trend_10yrprior_terra_change_id, xy=TRUE)
      # Unit annual change in mean annual air temp during the 10 prior measurement years (flux and gridded data would still need to be divided by 10 to go to the orig scale)
      
      gc()
      
      
      trend_20yrprior_terra_change_id <- rast(paste0("tmean20yrprior_trend_", substr(time[t],1, 4), ".tif"))
      plot(trend_20yrprior_terra_change_id)
      trend_20yrprior_terra_change_id
      summary(d$trend_20yrprior_terra_change_id)
      trend_20yrprior_terra_change_id <- trend_20yrprior_terra_change_id/1000
      trend_20yrprior_terra_change_id <- crop(trend_20yrprior_terra_change_id, ext(crops[c,]))
      trend_20yrprior_terra_change_id <- as.data.frame(trend_20yrprior_terra_change_id, xy=TRUE)
      # Unit annual change in mean annual air temp during the20 prior measurement years (flux and gridded data would still need to be divided by 10 to go to the orig scale)
      
      ndvi_trend_10yrprior_ndvi_change_id <- rast(paste0("ndvi10yrprior_trend_", substr(time[t],1, 4), ".tif"))
      plot(ndvi_trend_10yrprior_ndvi_change_id)
      ndvi_trend_10yrprior_ndvi_change_id
      summary(d$ndvi_trend_10yrprior_ndvi_change_id)
      ndvi_trend_10yrprior_ndvi_change_id <- ndvi_trend_10yrprior_ndvi_change_id/10000000 
      ndvi_trend_10yrprior_ndvi_change_id <- crop(ndvi_trend_10yrprior_ndvi_change_id, ext(crops[c,]))
      ndvi_trend_10yrprior_ndvi_change_id <- as.data.frame(ndvi_trend_10yrprior_ndvi_change_id, xy=TRUE)
      
      Snow.depth_era5_soilmoist_temp_snow <- rast(paste0("snowdepth_", time[t], ".tif"))
      plot(Snow.depth_era5_soilmoist_temp_snow)
      Snow.depth_era5_soilmoist_temp_snow
      summary(d$Snow.depth_era5_soilmoist_temp_snow)
      Snow.depth_era5_soilmoist_temp_snow <- Snow.depth_era5_soilmoist_temp_snow/100
      Snow.depth_era5_soilmoist_temp_snow <- crop(Snow.depth_era5_soilmoist_temp_snow, ext(crops[c,]))
      Snow.depth_era5_soilmoist_temp_snow <- as.data.frame(Snow.depth_era5_soilmoist_temp_snow, xy=TRUE)
      # snow depth in meters
      
      pr_terraclimate_sites <- rast(paste0("ppt_", time_alt[t], ".tif"))
      plot(pr_terraclimate_sites)
      pr_terraclimate_sites
      summary(d$pr_terraclimate_sites)
      pr_terraclimate_sites <- pr_terraclimate_sites/100
      pr_terraclimate_sites <- crop(pr_terraclimate_sites, ext(crops[c,]))
      pr_terraclimate_sites <- as.data.frame(pr_terraclimate_sites, xy=TRUE)
      # Monthly precipitation (mm)
      
      pdsi_terraclimate_sites <- rast(paste0("pdsi_", time_alt[t], ".tif"))
      plot(pdsi_terraclimate_sites)
      pdsi_terraclimate_sites
      summary(d$pdsi_terraclimate_sites)
      #pdsi_terraclimate_sites <- pdsi_terraclimate_sites/100 # not needed, checked by re-extracting data
      pdsi_terraclimate_sites <- crop(pdsi_terraclimate_sites, ext(crops[c,]))
      pdsi_terraclimate_sites <- as.data.frame(pdsi_terraclimate_sites, xy=TRUE)
      
      
      
      print("dynamic vars pt 2 merging")
      
      
      pred_rast_dynamic2 <- list(Snow.cover_era5_soilmoist_temp_snow, terra_trend_10yrprior_terra_change_id,
                                trend_20yrprior_terra_change_id, Snow.depth_era5_soilmoist_temp_snow, 
                                pr_terraclimate_sites, pdsi_terraclimate_sites,
                                ndvi_trend_10yrprior_ndvi_change_id, 
                                Percent_Tree_Cover_MOD44B_sites, Percent_NonTree_Vegetation_MOD44B_sites, Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow) %>% reduce(full_join, by = c("x", "y"))
      
      names(pred_rast_dynamic2) <- c("x", "y", "Snow.cover_era5_soilmoist_temp_snow", "terra_trend_10yrprior_terra_change_id",
                                    "trend_20yrprior_terra_change_id",
                                    "Snow.depth_era5_soilmoist_temp_snow", "pr_terraclimate_sites",
                                    "pdsi_terraclimate_sites",
                                    "ndvi_trend_10yrprior_ndvi_change_id",
                                    "Percent_Tree_Cover_MOD44B_sites", "Percent_NonTree_Vegetation_MOD44B_sites", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow")
      str(pred_rast_dynamic2)
      
      
      # remove NAs
      pred_rast_dynamic2_na <- na.omit(pred_rast_dynamic2)

      print("done")
      
      # remove the individual layers - they are just taking space
      rm(Snow.cover_era5_soilmoist_temp_snow)
      rm(terra_trend_10yrprior_terra_change_id)
      rm(trend_20yrprior_terra_change_id)
      rm(Snow.depth_era5_soilmoist_temp_snow)
      rm(pr_terraclimate_sites)
      rm(pdsi_terraclimate_sites)
      rm(ndvi_trend_10yrprior_ndvi_change_id)
      rm(Percent_Tree_Cover_MOD44B_sites)
      rm(Percent_NonTree_Vegetation_MOD44B_sites)
      rm(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
      
      gc()
      
      
      # 
      # pred_rast_dynamic <- list(srad_terraclimate_sites, Barrow_CO2_conc_Barrow_CO2conc,
      #                 NDVI_whittaker_constant_monthly_mean, Soil.temperature.level.1_era5_soilmoist_temp_snow,
      #                 water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality,
      #                 water_ground_MCD43A4_annual_water_ground_sites_low_quality,
      #                 LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality, vpd_terraclimate_sites, 
      #                 Snow.cover_era5_soilmoist_temp_snow, terra_trend_10yrprior_terra_change_id,
      #                 trend_20yrprior_terra_change_id, Snow.depth_era5_soilmoist_temp_snow, 
      #                 pr_terraclimate_sites, pdsi_terraclimate_sites,
      #                 ndvi_trend_10yrprior_ndvi_change_id, 
      #                 Percent_Tree_Cover_MOD44B_sites, Percent_NonTree_Vegetation_MOD44B_sites, Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow) %>% reduce(full_join, by = c("x", "y"))
      # 
      # names(pred_rast_dynamic) <- c("srad_terraclimate_sites", "Barrow_CO2_conc_Barrow_CO2conc",
      #                            "NDVI_whittaker_constant_monthly_mean", "Soil.temperature.level.1_era5_soilmoist_temp_snow",
      #                            "water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality",
      #                        "water_ground_MCD43A4_annual_water_ground_sites_low_quality",
      #                        "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", "vpd_terraclimate_sites",
      #                        "Snow.cover_era5_soilmoist_temp_snow", "terra_trend_10yrprior_terra_change_id",
      #                        "trend_20yrprior_terra_change_id",
      #                        "Snow.depth_era5_soilmoist_temp_snow", "pr_terraclimate_sites",
      #                        "pdsi_terraclimate_sites",
      #                        "ndvi_trend_10yrprior_ndvi_change_id",
      #                        "Percent_Tree_Cover_MOD44B_sites", "Percent_NonTree_Vegetation_MOD44B_sites", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow")
      
      
      
      ### combine all
     
      print("merge dynamic 1 and 2") 
      pred_rast_dynamic <- merge(pred_rast_dynamic1_na, pred_rast_dynamic2_na, by=c("x", "y"))
      rm(pred_rast_dynamic1)
      rm(pred_rast_dynamic2)
      rm(pred_rast_dynamic1_na)
      rm(pred_rast_dynamic2_na)
      print("merge all")
      str(pred_rast_dynamic_na)
      str(pred_rast_static_na)
      pred_rast <- merge(pred_rast_static_na, pred_rast_dynamic_na, by=c("x", "y")) # rows that have NA are skipped - buit note that for this we want to have predictors that are included in a model, not just any kind
      pred_rast$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- factor(pred_rast$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
      pred_rast$TKWP_Thermokarst <- factor(pred_rast$TKWP_Thermokarst)
      
      
      
      ##### ONGELMA VARMAAN TÄSSÄ:: https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
      # pred_rast2 <- pred_rast
      # pred_rast2$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- factor(pred_rast2$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
      # pred_rast2$TKWP_Thermokarst <- factor(pred_rast2$TKWP_Thermokarst)
      
      ### NÄin EI VOI TEHDÄ KOSKA MUUTTAA TASOJA VÄÄRIKSI
      #### ONGELMA VARMAAN RATKEAA JOS DESKTOP SERVERILLÄ VOI OLLA ISOMPI CROPPIALUE (TAI KOKO ALUE SUORAAN) JA TASOT ON SAMAT!
      # levels(pred_rast2$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) <- levels(factor(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged))
      # levels(pred_rast2$TKWP_Thermokarst) <- levels(factor(d$TKWP_Thermokarst))
      
      #rm(pred_rast_static) # keep this in memory because we will need it later!!
      rm(pred_rast_dynamic)
      gc()
      
      print("prediction data done")
      
      
      
      for (i in resp_vars) {
        
        #i <- "NEE_gC_m2"
        
        print("looping through resp vars and models")
        print(i)
        
        
        # Loop through the models 
        
        for (m in models) {
          
          # m <- "rf"
          # m <- "gbm"
          # m <- "svm"
          print(m)
          
          # Load model files
          mod <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i,  km, m, "loocv", sep="_"), ".rds"))
          
          # # Print the best variables
          # print("Best variables are:")
          mod$optVariables
          
          
          #pred <- predict(pred_data, mod, na.rm=TRUE, cores=7, cpkgs=c("ranger", "caret"), filename="temp_pred.tif", overwrite=TRUE)
          
          if (km=="1km") {
            
            # library("caret")
            # library("gbm", lib.loc="/mnt/data1/boreal/avirkkala/packages")
            # library("kernlab", lib.loc="/mnt/data1/boreal/avirkkala/packages")
            # library("randomForest", lib.loc="/mnt/data1/boreal/avirkkala/packages")
            library("randomForest")
            pred <- predict(newdata=pred_rast[,3:37], object=mod) # SEE THIS: https://www.py4u.net/discuss/865320; https://stackoverflow.com/questions/30097730/error-when-using-predict-on-a-randomforest-object-trained-with-carets-train
            #pred <- predict.rfe(newdata=pred_rast, object=mod)
            #multiply by 1000
            pred <- pred*1000
            
            print("prediction to dataframe done")
            
            
            #  convert back to raster
            pred_matrix  <- data.matrix(data.frame(cbind(pred_rast[,1:2], pred)))
            r <- rast(pred_matrix, type="xyz")
            terra::crs(r) <- terra::crs(esa)
            #plot(r)
            
            writeRaster(r, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, time[t],c, "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')

            print(paste("1 km prediction raster done and written out for ", i, km, m, time[t], c))
            
            # # note that prediction with terra should work: https://stackoverflow.com/questions/68082827/problem-with-type-prob-argument-in-carettrain-package
            # 
            # library(terra) # https://stackoverflow.com/questions/38623624/usemethodpredict-no-applicable-method-for-predict-applied-to-an-object-o
            # pred <- predict(pred_rast, mod, na.rm=TRUE,  
            #                 filename=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv", c, sep="_"), ".tif"), overwrite=TRUE) # cores=7, cpkgs=c("ranger", "caret"),
            # 
            # print("terra pred done")
            # # testing raster
            # library("raster")
            # #pred_rast_test <- brick(pred_rast)
            # pred <- raster::predict(brick(pred_rast), mod, na.rm=TRUE,  
            #                         filename=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv_raster", c, sep="_"), ".tif"), overwrite=TRUE,
            #                         progress='window', fun = function(model, ...) predict(model, ...)$predictions) # cores=7, cpkgs=c("ranger", "caret"), # see this: https://github.com/imbs-hl/ranger/issues/319
            # print("raster pred done")
            # 
            # r <- terra::rast(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv", c, sep="_"), ".tif"))*100
            # 
            # writeRaster(r, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv", c, sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
            # 
            # print("1 km prediction done for ", i, km, m, c)
            # 
            # rm(pred); rm(r)
            # gc()
          }
          
          
        } # model loop
        
        print("model loop done")
        
      } # resp var loop
      
      print("resp loop done")
      
      
    } # time period loop done
    
    print("model loop done")
    
    rm(pred_rast)
    rm(pred_rast_dynamic)
    
    
    
    
  } # km loop done
  
  print("km loop done")



  
} # crop loop done
        
        

        

  





