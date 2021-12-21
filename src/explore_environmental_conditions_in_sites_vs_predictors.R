

library("raster")
library("terra")
library("ggplot2")
library("dplyr")


### flux data

terraOptions(memfrac=0.9, tempdir = "/mnt/data1/boreal/avirkkala/Temp")


### Data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv") 


# Reclassify veg type and fire
# not enough fire sites... merge 1-2 and 3-4!!
d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- ifelse(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned==2, 1, d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- ifelse(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned==4, 3, d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)

# not enough cavm observations for GPP and Reco... merge class 1 (barren) and 31 (prostrate shrub)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- ifelse(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged==1, 31, d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)


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
                       
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned",   # time since fire
                       
                       "TKWP_Thermokarst", "TKHP_Thermokarst", # themokarst vulnerability 
                       
                       "forest_age_class_forest_age_sites" # forest age
                       
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km




flux <- "NEE_gC_m2"


### Model parameter inputs for feature (variable) selection and model tuning

### remove NA across columns for 1 and 20 km datasets
modeldata2 <- d[,c("Study_ID_Short", "id", "Meas_year", "Interval", "Country", "Biome", flux, Baseline_vars_1km)]
modeldata1 <- na.omit(modeldata2) 
sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
print("1 km data set:")
print(nrow(modeldata1))



# take the year with the largest amount of data
t <- modeldata1 %>% group_by(Meas_year) %>% summarize(n=n()) %>% sort(n)
sort(t$n) # pick 2012?


### most important predictors for year 2012
# [1] "srad_terraclimate_sites"                                                                    
# [2] "Barrow_CO2_conc_Barrow_CO2conc"                                                             
# [3] "terra_trend_19601990"                                                                       
# [4] "SoilGrids_SOC_SoilGrids_SOCstock"                                                           
# [5] "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality"                                          
# [6] "NDVI_whittaker_constant_monthly_mean"                                                       
# [7] "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m"                         
# [8] "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m"                 
# [9] "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent" 
# [10] "BLDFIE_M_sl1_250m_ll_SoilGrids"                                                             
# [11] "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged"
# [12] "ndvi_trend_19812010"  

### Load static vars (only once)
setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_1km") ### REMEMBER TO CHANGE THIS!!!

ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- raster("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")
SoilGrids_SOC_SoilGrids_SOCstock <-  rast("soc.tif")/100
dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("roughscale.tif")/100
dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("cti.tif")/100
BLDFIE_M_sl1_250m_ll_SoilGrids <- rast("bulkdensity.tif")/100
ndvi_trend_19812010 <-  rast("ndvi_trend_19812010.tif")/10000000 
terra_trend_19601990 <- rast("tmean_trend_19601990.tif")/1000
sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- rast("sol_watercontent.tif")/100

srad_terraclimate_sites <- rast(list.files(pattern="srad_2012"))/10
NDVI_whittaker_constant_monthly_mean <- rast(list.files(pattern="ndvi_2012"))/10000 ## EI ARVOJA plotissa mutta valuesin kautta tulee??? mutta on arvoja raster-komennon kautta plotissa???
LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality <- rast(list.files(pattern="lst_2012"))/1000


# test
ndvi <- spatSample(NDVI_whittaker_constant_monthly_mean[[9]], 10000)


# loop through countries

# subset to year
d2 <- subset(modeldata1, Meas_year==2012)

k <- length(unique(modeldata1$Country))

country <- terra::vect("/mnt/data1/boreal/avirkkala/abcflux_modeling/visualization/ne_10m_admin_0_countries.shp")

country <- terra::crop(country, ext(-180, 180, 40, 90))

country2 <- terra::project(country, terra_trend_19601990)
unique(country2$ADMIN)
unique(modeldata1$Country)


countrys <- shapefile("/mnt/data1/boreal/avirkkala/abcflux_modeling/visualization/ne_10m_admin_0_countries.shp")

countrys <- raster::crop(countrys, extent(-180, 180, 40, 90))

countrys2 <- spTransform(countrys, "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")

modeldata1$Country <- as.character(modeldata1$Country)
modeldata1$Country <- ifelse(modeldata1$Country=="USA", "United States of America", modeldata1$Country )
countries <- unique(modeldata1$Country)


for (i in 1:length(countries)) {
  
  # i <- 4
  country1 <- countries[i]
  print(country1)
  
  # crop env data to the country
  c1 <- terra::subset(country2, country2$ADMIN==country1)
  
  c1esa <- raster::subset(countrys2, country2$ADMIN==country1)
  

  
  
  ### ESA
  # extract env data
  esa <- raster::crop(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, extent(c1esa)) ## VOIKO ESA TULLA NYT JOTENKIN VÄÄRIN????
  esa <- mask(esa, c1esa)
  esater <- rast(esa)
  
  # extract site-level data
  rvals <- getValues(esa)
  sitevals <- subset(modeldata1, Country==country1)
  sitevals_winter <- subset(modeldata1, Country==country1 & (Interval>=10 | Interval <=3))
  
  # visualize
  vals_all <- rbind(data.frame(data=c(rep("sites", length(sitevals$id)), rep("sites_winter", length(sitevals_winter$id)), rep("pan-Arctic", length(rvals))), values=c(sitevals$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, sitevals_winter$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged,  rvals)))
  vals_all_summary <- vals_all %>% group_by(data, values) %>% summarize(n=n())
  vals_all_summary <- subset(vals_all_summary, !is.na(values))
  print(ggplot(vals_all_summary)) + geom_boxplot(aes(y=n, x=factor(values))) + facet_wrap(~data, scales = "free") + ggtitle(paste(country1, "Veg type"))
  
  rm(vals_all); rm(esa); gc()
  
  ### SoilGrids_SOC_SoilGrids_SOCstock TÄHÄN JÄiN LISÄÄ SITEVALUES WINTER VALUES DF
  # extract env data
  soc <- terra::crop(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, c1) ## VOIKO ESA TULLA NYT JOTENKIN VÄÄRIN????
  soc <- mask(soc, esater)
  
  # extract site-level data
  rvals <- values(soc)

  # visualize
  vals_all <- rbind(data.frame(data=c(rep("sites", length(sitevals$id)), rep("sites_winter", length(sitevals_winter$id)), rep("pan-Arctic", length(rvals))), values=c(sitevals$dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, rvals)))

  print(ggplot(vals_all)) + geom_density(aes(values, color=data)) + ggtitle(paste(country1, "SOC"))
  
  rm(vals_all); rm(soc); gc()
  
  
  
  ### dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
  # extract env data
  soc <- terra::crop(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, c1) ## VOIKO ESA TULLA NYT JOTENKIN VÄÄRIN????
  soc <- mask(soc, esater)
  
  # extract site-level data
  rvals <- values(soc)
  
  # visualize
  vals_all <- rbind(data.frame(data=c(rep("sites", length(sitevals$id)), rep("sites_winter", length(sitevals_winter$id)), rep("pan-Arctic", length(rvals))), values=c(sitevals$dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, rvals)))
  
  print(ggplot(vals_all)) + geom_density(aes(values, color=data)) + ggtitle(paste(country1, "Roughness scale"))
  
  rm(vals_all); rm(soc); gc()
  
  
  
  ### dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
  # extract env data
  soc <- terra::crop(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, c1) ## VOIKO ESA TULLA NYT JOTENKIN VÄÄRIN????
  soc <- mask(soc, esater)
  
  # extract site-level data
  rvals <- values(soc)
  
  # visualize
  vals_all <- rbind(data.frame(data=c(rep("sites", length(sitevals$id)), rep("sites_winter", length(sitevals_winter$id)), rep("pan-Arctic", length(rvals))), values=c(sitevals$dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, rvals)))
  
  print(ggplot(vals_all)) + geom_density(aes(values, color=data)) + ggtitle(paste(country1, "CTI"))
  
  rm(vals_all); rm(soc); gc()
  
  
  
  ### BLDFIE_M_sl1_250m_ll_SoilGrids
  # extract env data
  soc <- terra::crop(BLDFIE_M_sl1_250m_ll_SoilGrids, c1)
  soc <- mask(soc, esater)
  
  # extract site-level data
  rvals <- values(soc)
  
  # visualize
  vals_all <- rbind(data.frame(data=c(rep("sites", length(sitevals$id)), rep("sites_winter", length(sitevals_winter$id)), rep("pan-Arctic", length(rvals))), values=c(sitevals$BLDFIE_M_sl1_250m_ll_SoilGrids, rvals)))
  
  print(ggplot(vals_all)) + geom_density(aes(values, color=data)) + ggtitle(paste(country1, "Bulk density"))
  
  rm(vals_all); rm(soc); gc()
  
  
  ### ndvi_trend_19812010
  # extract env data
  soc <- terra::crop(ndvi_trend_19812010, c1) 
  soc <- mask(soc, esater)
  
  # extract site-level data
  rvals <- values(soc)
  
  # visualize
  vals_all <- rbind(data.frame(data=c(rep("sites", length(sitevals$id)), rep("sites_winter", length(sitevals_winter$id)), rep("pan-Arctic", length(rvals))), values=c(sitevals$ndvi_trend_19812010, rvals)))
  
  print(ggplot(vals_all)) + geom_density(aes(values, color=data)) + ggtitle(paste(country1, "NDVI trend 1960-1990"))
  
  rm(vals_all); rm(soc); gc()
  
  
  ### terra_trend_19601990
  # extract env data
  soc <- terra::crop(terra_trend_19601990, c1) 
  soc <- mask(soc, esater)
  
  # extract site-level data
  rvals <- values(soc)
  
  # visualize
  vals_all <- rbind(data.frame(data=c(rep("sites", length(sitevals$id)), rep("sites_winter", length(sitevals_winter$id)), rep("pan-Arctic", length(rvals))), values=c(sitevals$terra_trend_19601990, rvals)))
  
  print(ggplot(vals_all)) + geom_density(aes(values, color=data)) + ggtitle(paste(country1, "Tmean trend 1960-1990"))
  
  rm(vals_all); rm(soc); gc()
  
  
  
  ### srad_terraclimate_sites
  # extract env data
  soc <- terra::crop(srad_terraclimate_sites, c1) 
  soc <- mask(soc, esater)
  
  # extract site-level data
  rvals <- values(soc)
  rvals_df <- data.frame(rvals)
  rvals_df <- na.omit(rvals_df)
  
  
  # visualize
  vals_all <- rbind(data.frame(data=c(rep("sites", length(sitevals$id)), rep("sites_winter", length(sitevals_winter$id)), rep("pan-Arctic", length(rvals_df[,1])*12)), values=c(sitevals$srad_terraclimate_sites, rvals_df[,1], rvals_df[,2], rvals_df[,3],
                                                                                                                               rvals_df[,4], rvals_df[,5], rvals_df[,6], rvals_df[,7], rvals_df[,8],
                                                                                                                               rvals_df[,9], rvals_df[,10], rvals_df[,11], rvals_df[,12])))
  
  print(ggplot(vals_all)) + geom_density(aes(values, color=data)) + ggtitle(paste(country1, "Srad"))
  
  rm(vals_all); rm(soc); gc()
  
  
  ### NDVI_whittaker_constant_monthly_mean
  # extract env data
  soc <- terra::crop(NDVI_whittaker_constant_monthly_mean, c1) 
  soc <- mask(soc, esater)
  
  # extract site-level data
  rvals <- values(soc)
  rvals_df <- data.frame(rvals)
  rvals_df <- na.omit(rvals_df)

  
  # visualize
  vals_all <- rbind(data.frame(data=c(rep("sites", length(sitevals$id)), rep("sites_winter", length(sitevals_winter$id)), rep("pan-Arctic", length(rvals_df[,1])*12)), values=c(sitevals$NDVI_whittaker_constant_monthly_mean, rvals_df[,1], rvals_df[,2], rvals_df[,3],
                                                                                                                               rvals_df[,4], rvals_df[,5], rvals_df[,6], rvals_df[,7], rvals_df[,8],
                                                                                                                               rvals_df[,9], rvals_df[,10], rvals_df[,11], rvals_df[,12])))
  
  
  print(ggplot(vals_all)) + geom_density(aes(values, color=data)) + ggtitle(paste(country1, "NDVI"))
  
  rm(vals_all); rm(soc); gc()
  
  
  ### LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality
  # extract env data
  soc <- terra::crop(LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality, c1) 
  soc <- mask(soc, esater)
  
  # extract site-level data
  rvals <- values(soc)
  rvals_df <- data.frame(rvals)
  rvals_df <- na.omit(rvals_df)
  
  
  # visualize
  vals_all <- rbind(data.frame(data=c(rep("sites", length(sitevals$id)), rep("sites_winter", length(sitevals_winter$id)), rep("pan-Arctic", length(rvals_df[,1])*12)), values=c(sitevals$LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality, rvals_df[,1], rvals_df[,2], rvals_df[,3],
                                                                                                                               rvals_df[,4], rvals_df[,5], rvals_df[,6], rvals_df[,7], rvals_df[,8],
                                                                                                                               rvals_df[,9], rvals_df[,10], rvals_df[,11], rvals_df[,12])))
  
  
  print(ggplot(vals_all)) + geom_density(aes(values, color=data)) + ggtitle(paste(country1, "LST"))
  
  rm(vals_all); rm(soc); gc()
  
  
  
  
}






