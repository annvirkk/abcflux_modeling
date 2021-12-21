
library("stringr")
library("stringi")
library("terra")
library("dplyr")
terraOptions(memfrac=0.9, tempdir = "/mnt/data1/boreal/avirkkala/Temp") 
library("ggplot2")
library("tidyr")


# visualize variability in environmental conditions and fluxes in different time steps



### period 2000-2004 as an example ###

rasterfile <-  rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_aggregate_northpolelambert8km_tundraboreal_attfix.tif")


# random sample
sp <- terra::spatSample(rasterfile, size=10000, method="random", replace=FALSE, na.rm=TRUE, as.points=TRUE)

# list predictor rasters that include a year 2000-2004
pattern1 <- seq(2000, 2004, by=1) %>% as.character()
rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km", pattern=paste0(pattern1, collapse="|"))
rasters <- rasters[endsWith(rasters, ".tif")]

setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km") 
r <- rasters[grepl( "srad", rasters)]
srad_terraclimate_sites <- (rast(r)/10)
srad <- extract(srad_terraclimate_sites, sp)

r <- rasters[grepl( "ndvi", rasters)]
ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled  <- (rast(r)/10000)
ndvi <- extract(ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled, sp)

r <- rasters[grepl( "soiltemplevel1_", rasters)]
Soil.temperature.level.1_era5_soilmoist_temp_snow <- (rast(r)/100)
soiltemp <- extract(Soil.temperature.level.1_era5_soilmoist_temp_snow, sp)

r <- rasters[grepl( "tmean_", rasters)]
tmean_terraclimate_sites  <- (rast(r)/100)
tmean <- extract(tmean_terraclimate_sites, sp)

r <- rasters[grepl( "vpd", rasters)]
vpd_terraclimate_sites  <- (rast(r))
vpd <- extract(vpd_terraclimate_sites, sp)

r <- rasters[grepl( "snowcover", rasters)]
Snow.cover_era5_soilmoist_temp_snow   <- (rast(r)/100)
snowcover <- extract(Snow.cover_era5_soilmoist_temp_snow , sp)

r <- rasters[grepl( "soilmoist", rasters)]
Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    <- (rast(r)/100)
soilmoist <- extract(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "NonTree_Vegetation", rasters)]
Percent_NonTree_Vegetation_AVHRR_VCF5KYR     <- (rast(r))
NonTree_Vegetation <- extract(Percent_NonTree_Vegetation_AVHRR_VCF5KYR  , sp)

r <- rasters[grepl( "TreeCover", rasters)]
Percent_TreeCover_AVHRR_VCF5KYR      <- (rast(r))
TreeCover <- extract(Percent_TreeCover_AVHRR_VCF5KYR   , sp)

r <- rasters[grepl( "NonVegetated", rasters)]
Percent_NonVegetated_AVHRR_VCF5KYR       <- (rast(r))
NonVegetated <- extract(Percent_NonVegetated_AVHRR_VCF5KYR    , sp)


r <- rasters[grepl( "tmean20", rasters)]
trend_20yrprior_terra_change_id        <- (rast(r)/1000)
tmeantrend <- extract(trend_20yrprior_terra_change_id     , sp)

r <- rasters[grepl( "snowdepth", rasters)]
Snow.depth_era5_soilmoist_temp_snow    <- (rast(r)/100)
snowdepth <- extract(Snow.depth_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "ppt", rasters)]
pr_terraclimate_sites     <- (rast(r)/100)
ppt <- extract(pr_terraclimate_sites   , sp)

r <- rasters[grepl( "pdsi", rasters)]
pdsi_terraclimate_sites      <- (rast(r))
pdsi <- extract(pdsi_terraclimate_sites    , sp)


env <- data.frame(cbind(srad[,2], ndvi[, 2], soiltemp [, 2], tmean [, 2], vpd [, 2], snowcover [, 2],
                              soilmoist [, 2], NonTree_Vegetation [, 2], TreeCover [, 2], NonVegetated [, 2], tmeantrend [, 2],
                              snowdepth [, 2], ppt [, 2],pdsi [, 2]))

names(env) <- c("srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                                        "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                                        "pdsi")

env_long <- pivot_longer(env, 1:14)

ggplot(env_long) + geom_density(aes(value)) + facet_wrap(~name, scales="free") 



# might be more interesting to visualize NDVI only from June-August period or so
r <- rasters[grepl( "ndvi", rasters)]
r <- r[str_detect(r, paste(c( "06", "07", "08"), collapse = "|")) ]

ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled  <- (rast(r)/10000)
ndvi <- terra::extract(ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled, sp)

env <- data.frame(cbind(srad[,2], ndvi[, 2], soiltemp [, 2], tmean [, 2], vpd [, 2], snowcover [, 2],
                        soilmoist [, 2], NonTree_Vegetation [, 2], TreeCover [, 2], NonVegetated [, 2], tmeantrend [, 2],
                        snowdepth [, 2], ppt [, 2],pdsi [, 2]))

names(env) <- c("srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                "pdsi")

env_long <- pivot_longer(env, 1:14)

ggplot(env_long) + geom_density(aes(value)) + facet_wrap(~name, scales="free") 


# and then can plot these across differnt time periods (years, seasons), same for flux predictions
