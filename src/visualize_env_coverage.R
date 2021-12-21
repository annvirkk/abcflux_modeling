

library("terra")
library("dplyr")
terraOptions(memfrac=0.9, tempdir = "/mnt/data1/boreal/avirkkala/Temp") 
library("modEvA")



## Visualize the conditions that the sites cover in 5-year splits based on 8 km predictors
# always use just the sites that were included in the models and time series analysis. Also split between all sites and sites that have winter data!

# for this, calculate 5-year annual averages for each predictor
# don't use factors
# extract annual avg values for each site
# focus on sites with NEE data




### Load the model training data 
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv") 
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



modeldata2 <- d[,c("Study_ID_Short", "id", "NEE_gC_m2", "Meas_year", "Interval", "Season", "Latitude", "Longitude", Baseline_vars_20km)]
modeldata1 <- na.omit(modeldata2) 



# Load static rasters
### Load static vars (only once)
setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km") 



aboveground_biomass_carbon_2010_Above_belowground_biomass <- rast("abovegroundbiomass.tif")
#plot(aboveground_biomass_carbon_2010_Above_belowground_biomass)
aboveground_biomass_carbon_2010_Above_belowground_biomass
summary(d$aboveground_biomass_carbon_2010_Above_belowground_biomass)
aboveground_biomass_carbon_2010_Above_belowground_biomass <- aboveground_biomass_carbon_2010_Above_belowground_biomass/100


tmean_TerraClimate_averages <-  rast("Terraclimate_averages_tmean.tif")
#plot(tmean_TerraClimate_averages)
tmean_TerraClimate_averages
summary(d$tmean_TerraClimate_averages)
tmean_TerraClimate_averages <- tmean_TerraClimate_averages/1000


SoilGrids_SOC_SoilGrids_SOCstock <-  rast("soc.tif")
#plot(SoilGrids_SOC_SoilGrids_SOCstock)
SoilGrids_SOC_SoilGrids_SOCstock
summary(d$SoilGrids_SOC_SoilGrids_SOCstock)
SoilGrids_SOC_SoilGrids_SOCstock <- SoilGrids_SOC_SoilGrids_SOCstock/100


dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("roughscale.tif")
#plot(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
summary(d$dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m/100



belowground_biomass_carbon_2010_Above_belowground_biomass <- rast("belowgroundbiomass.tif") 
#plot(belowground_biomass_carbon_2010_Above_belowground_biomass)
belowground_biomass_carbon_2010_Above_belowground_biomass
summary(d$belowground_biomass_carbon_2010_Above_belowground_biomass)
belowground_biomass_carbon_2010_Above_belowground_biomass <- belowground_biomass_carbon_2010_Above_belowground_biomass/100 



BLDFIE_M_sl1_250m_ll_SoilGrids <- rast("bulkdensity.tif")
#plot(BLDFIE_M_sl1_250m_ll_SoilGrids)
BLDFIE_M_sl1_250m_ll_SoilGrids
summary(d$BLDFIE_M_sl1_250m_ll_SoilGrids)
BLDFIE_M_sl1_250m_ll_SoilGrids <- BLDFIE_M_sl1_250m_ll_SoilGrids/100 

dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("cti.tif")
#plot(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
summary(d$dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m/100 


sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- rast("sol_watercontent.tif")
#plot(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent
summary(d$sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent/100 


ppt_TerraClimate_averages <- rast("Terraclimate_averages_ppt.tif")
#plot(ppt_TerraClimate_averages)
ppt_TerraClimate_averages
summary(d$ppt_TerraClimate_averages)
ppt_TerraClimate_averages <- ppt_TerraClimate_averages/1000 


ndvi_trend_19812010 <-  rast("ndvi_trend_19812010.tif") 
#plot(ndvi_trend_19812010)
ndvi_trend_19812010
summary(d$ndvi_trend_19812010)
ndvi_trend_19812010 <- ndvi_trend_19812010/10000000 # NDVI rasters multiplied by 10000, and the trend multiplied by 1000. This was checked by new extraction too


PHIHOX_M_sl1_250m_ll_SoilGrids <- rast("ph.tif")
#plot(PHIHOX_M_sl1_250m_ll_SoilGrids)
PHIHOX_M_sl1_250m_ll_SoilGrids
summary(d$PHIHOX_M_sl1_250m_ll_SoilGrids)
PHIHOX_M_sl1_250m_ll_SoilGrids <- PHIHOX_M_sl1_250m_ll_SoilGrids/100 



terra_trend_19601990 <- rast("tmean_trend_19601990.tif")
#plot(terra_trend_19601990)
terra_trend_19601990
summary(d$terra_trend_19601990)
terra_trend_19601990 <- terra_trend_19601990/1000



UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- rast("UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif")
#plot(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH
summary(d$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH/100 







# Load dynamic rasters in 5-year splits and calculate avg



### period 2: 1990-1994 ###

# model training data subset
modelsub <- subset(modeldata1, Meas_year<=1994 & Meas_year>=1990)
sp <- vect(modelsub, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
sp <- project(sp, UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)

# list rasters that include a year 1990-1994
pattern1 <- seq(1990, 1994, by=1) %>% as.character()
rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km", pattern=paste0(pattern1, collapse="|"))
rasters <- rasters[endsWith(rasters, ".tif")]

setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km") 
r <- rasters[grepl( "srad", rasters)]
srad_terraclimate_sites <- mean(rast(r)/10)
srad <- extract(srad_terraclimate_sites, sp)

r <- rasters[grepl( "ndvi", rasters)]
ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled  <- mean(rast(r)/10000)
ndvi <- extract(ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled, sp)

r <- rasters[grepl( "soiltemplevel1_", rasters)]
Soil.temperature.level.1_era5_soilmoist_temp_snow <- mean(rast(r)/100)
soiltemp <- extract(Soil.temperature.level.1_era5_soilmoist_temp_snow, sp)

r <- rasters[grepl( "tmean_", rasters)]
tmean_terraclimate_sites  <- mean(rast(r)/10)
tmean <- extract(tmean_terraclimate_sites, sp)

r <- rasters[grepl( "vpd", rasters)]
vpd_terraclimate_sites  <- mean(rast(r))
vpd <- extract(vpd_terraclimate_sites, sp)

r <- rasters[grepl( "snowcover", rasters)]
Snow.cover_era5_soilmoist_temp_snow   <- mean(rast(r)/100)
snowcover <- extract(Snow.cover_era5_soilmoist_temp_snow , sp)

r <- rasters[grepl( "soilmoist", rasters)]
Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
soilmoist <- extract(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "NonTree_Vegetation", rasters)]
Percent_NonTree_Vegetation_AVHRR_VCF5KYR     <- mean(rast(r))
NonTree_Vegetation <- extract(Percent_NonTree_Vegetation_AVHRR_VCF5KYR  , sp)

r <- rasters[grepl( "TreeCover", rasters)]
Percent_TreeCover_AVHRR_VCF5KYR      <- mean(rast(r))
TreeCover <- extract(Percent_TreeCover_AVHRR_VCF5KYR   , sp)

r <- rasters[grepl( "NonVegetated", rasters)]
Percent_NonVegetated_AVHRR_VCF5KYR       <- mean(rast(r))
NonVegetated <- extract(Percent_NonVegetated_AVHRR_VCF5KYR    , sp)


r <- rasters[grepl( "tmean20", rasters)]
trend_20yrprior_terra_change_id        <- mean(rast(r)/1000)
tmeantrend <- extract(trend_20yrprior_terra_change_id     , sp)

r <- rasters[grepl( "snowdepth", rasters)]
Snow.depth_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
snowdepth <- extract(Snow.depth_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "ppt", rasters)]
pr_terraclimate_sites     <- mean(rast(r)/100)
ppt <- extract(pr_terraclimate_sites   , sp)

r <- rasters[grepl( "pdsi", rasters)]
pdsi_terraclimate_sites      <- mean(rast(r))
pdsi <- extract(pdsi_terraclimate_sites    , sp)

# static extractions
agb <- extract(aboveground_biomass_carbon_2010_Above_belowground_biomass     , sp)
bgb <- extract(belowground_biomass_carbon_2010_Above_belowground_biomass      , sp)
tmeanav <- extract(tmean_TerraClimate_averages, sp)
soc <- extract(SoilGrids_SOC_SoilGrids_SOCstock , sp)
scale <- extract(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m , sp)
bulk <- extract(BLDFIE_M_sl1_250m_ll_SoilGrids  , sp)
cti <- extract(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m   , sp)
soilwat <- extract(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent    , sp)
pptav <- extract(ppt_TerraClimate_averages     , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
ph <- extract(PHIHOX_M_sl1_250m_ll_SoilGrids      , sp)
perma <- extract(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH       , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
terratrend60 <- extract(terra_trend_19601990      , sp)


rasters <- c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
             Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
             vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
             Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
             Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
             trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
             pr_terraclimate_sites     , pdsi_terraclimate_sites,
             
             aboveground_biomass_carbon_2010_Above_belowground_biomass, belowground_biomass_carbon_2010_Above_belowground_biomass,
             tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock,
             dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, BLDFIE_M_sl1_250m_ll_SoilGrids,
             dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent,
             ppt_TerraClimate_averages, ndvi_trend_19812010, PHIHOX_M_sl1_250m_ll_SoilGrids, 
             UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, terra_trend_19601990)

rasters_matrix <- as.data.frame(rasters, xy=TRUE)
names(rasters_matrix) <- c("x", "y", "srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                           "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                           "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                           "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

p2_insitu <- data.frame(cbind(srad[,2], ndvi[, 2], soiltemp [, 2], tmean [, 2], vpd [, 2], snowcover [, 2],
                              soilmoist [, 2], NonTree_Vegetation [, 2], TreeCover [, 2], NonVegetated [, 2], tmeantrend [, 2],
                              snowdepth [, 2], ppt [, 2],pdsi [, 2],agb[, 2],bgb[, 2],
                              tmeanav[, 2],soc[, 2],scale[, 2],bulk[, 2],
                              cti[, 2],soilwat[, 2],pptav[, 2],ndvitrend[, 2],
                              ph[, 2],perma[, 2],terratrend60[, 2]))

names(p2_insitu) <- c("srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                           "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                           "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                           "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

colnames(rasters_matrix) %in% colnames(p2_insitu)

mess <- MESS(p2_insitu, rasters_matrix[, 3:ncol(rasters_matrix)])

mess_coord <- data.frame(cbind(rasters_matrix[, 1:2], mess[, ncol(mess)-1]))
mess_coord_all <- data.frame(cbind(rasters_matrix[, 1:2], mess[, (ncol(mess)-1) : (ncol(mess))]))

write.csv(mess_coord_all, "/mnt/data1/boreal/avirkkala/abcflux_modeling/environmental_coverage/mess_19901994.csv", row.names=TRUE)
pred_rast_static_m  <- as.matrix(mess_coord[]) 
r <- rast(pred_rast_static_m[], type="xyz")
plot(r)
#Negative values indicate localities that are environmentally dissimilar from the reference region. 
#The last column, MoD, indicates which of the column names of P corresponds to the most dissimilar variable, i.e., the limiting factor or the variable that drives the MESS in that locality (Elith et al. 2010).
# another plot
vals <- values(r)
m <- c(quantile(vals, .75, na.rm=TRUE), 0, -1,
  min(vals, na.rm=TRUE), quantile(vals, .25, na.rm=TRUE), -4,
  quantile(vals, .25, na.rm=TRUE), median(vals,  na.rm=TRUE), -3,
  median(vals,  na.rm=TRUE),quantile(vals, .75, na.rm=TRUE), -2,
 0, max(vals,  na.rm=TRUE), 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(r, rclmat, include.lowest=TRUE)
plot(rc1)



rm(list = c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
    Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
    vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
    Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
    Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
    trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
    pr_terraclimate_sites     , pdsi_terraclimate_sites))











### period 3: 1995-1999 ###

# model training data subset
modelsub <- subset(modeldata1, Meas_year<=1999 & Meas_year>=1995)
sp <- vect(modelsub, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
sp <- project(sp, UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)

# list rasters that include years
pattern1 <- seq(1995, 1999, by=1) %>% as.character()
rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km", pattern=paste0(pattern1, collapse="|"))
rasters <- rasters[endsWith(rasters, ".tif")]

setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km") 
r <- rasters[grepl( "srad", rasters)]
srad_terraclimate_sites <- mean(rast(r)/10)
srad <- extract(srad_terraclimate_sites, sp)

r <- rasters[grepl( "ndvi", rasters)]
ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled  <- mean(rast(r)/10000)
ndvi <- extract(ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled, sp)

r <- rasters[grepl( "soiltemplevel1_", rasters)]
Soil.temperature.level.1_era5_soilmoist_temp_snow <- mean(rast(r)/100)
soiltemp <- extract(Soil.temperature.level.1_era5_soilmoist_temp_snow, sp)

r <- rasters[grepl( "tmean_", rasters)]
tmean_terraclimate_sites  <- mean(rast(r)/100)
tmean <- extract(tmean_terraclimate_sites, sp)

r <- rasters[grepl( "vpd", rasters)]
vpd_terraclimate_sites  <- mean(rast(r))
vpd <- extract(vpd_terraclimate_sites, sp)

r <- rasters[grepl( "snowcover", rasters)]
Snow.cover_era5_soilmoist_temp_snow   <- mean(rast(r)/100)
snowcover <- extract(Snow.cover_era5_soilmoist_temp_snow , sp)

r <- rasters[grepl( "soilmoist", rasters)]
Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
soilmoist <- extract(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "NonTree_Vegetation", rasters)]
Percent_NonTree_Vegetation_AVHRR_VCF5KYR     <- mean(rast(r))
NonTree_Vegetation <- extract(Percent_NonTree_Vegetation_AVHRR_VCF5KYR  , sp)

r <- rasters[grepl( "TreeCover", rasters)]
Percent_TreeCover_AVHRR_VCF5KYR      <- mean(rast(r))
TreeCover <- extract(Percent_TreeCover_AVHRR_VCF5KYR   , sp)

r <- rasters[grepl( "NonVegetated", rasters)]
Percent_NonVegetated_AVHRR_VCF5KYR       <- mean(rast(r))
NonVegetated <- extract(Percent_NonVegetated_AVHRR_VCF5KYR    , sp)


r <- rasters[grepl( "tmean20", rasters)]
trend_20yrprior_terra_change_id        <- mean(rast(r)/1000)
tmeantrend <- extract(trend_20yrprior_terra_change_id     , sp)

r <- rasters[grepl( "snowdepth", rasters)]
Snow.depth_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
snowdepth <- extract(Snow.depth_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "ppt", rasters)]
pr_terraclimate_sites     <- mean(rast(r)/100)
ppt <- extract(pr_terraclimate_sites   , sp)

r <- rasters[grepl( "pdsi", rasters)]
pdsi_terraclimate_sites      <- mean(rast(r))
pdsi <- extract(pdsi_terraclimate_sites    , sp)

# static extractions
agb <- extract(aboveground_biomass_carbon_2010_Above_belowground_biomass     , sp)
bgb <- extract(belowground_biomass_carbon_2010_Above_belowground_biomass      , sp)
tmeanav <- extract(tmean_TerraClimate_averages, sp)
soc <- extract(SoilGrids_SOC_SoilGrids_SOCstock , sp)
scale <- extract(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m , sp)
bulk <- extract(BLDFIE_M_sl1_250m_ll_SoilGrids  , sp)
cti <- extract(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m   , sp)
soilwat <- extract(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent    , sp)
pptav <- extract(ppt_TerraClimate_averages     , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
ph <- extract(PHIHOX_M_sl1_250m_ll_SoilGrids      , sp)
perma <- extract(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH       , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
terratrend60 <- extract(terra_trend_19601990      , sp)


rasters <- c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
             Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
             vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
             Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
             Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
             trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
             pr_terraclimate_sites     , pdsi_terraclimate_sites,
             
             aboveground_biomass_carbon_2010_Above_belowground_biomass, belowground_biomass_carbon_2010_Above_belowground_biomass,
             tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock,
             dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, BLDFIE_M_sl1_250m_ll_SoilGrids,
             dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent,
             ppt_TerraClimate_averages, ndvi_trend_19812010, PHIHOX_M_sl1_250m_ll_SoilGrids, 
             UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, terra_trend_19601990)

rasters_matrix <- as.data.frame(rasters, xy=TRUE)
names(rasters_matrix) <- c("x", "y", "srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                           "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                           "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                           "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

p2_insitu <- data.frame(cbind(srad[,2], ndvi[, 2], soiltemp [, 2], tmean [, 2], vpd [, 2], snowcover [, 2],
                              soilmoist [, 2], NonTree_Vegetation [, 2], TreeCover [, 2], NonVegetated [, 2], tmeantrend [, 2],
                              snowdepth [, 2], ppt [, 2],pdsi [, 2],agb[, 2],bgb[, 2],
                              tmeanav[, 2],soc[, 2],scale[, 2],bulk[, 2],
                              cti[, 2],soilwat[, 2],pptav[, 2],ndvitrend[, 2],
                              ph[, 2],perma[, 2],terratrend60[, 2]))

names(p2_insitu) <- c("srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                      "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                      "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                      "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

colnames(rasters_matrix) %in% colnames(p2_insitu)

mess <- MESS(p2_insitu, rasters_matrix[, 3:ncol(rasters_matrix)])

mess_coord <- data.frame(cbind(rasters_matrix[, 1:2], mess[, ncol(mess)-1]))
mess_coord_all <- data.frame(cbind(rasters_matrix[, 1:2], mess[, (ncol(mess)-1) : (ncol(mess))]))

write.csv(mess_coord_all, "/mnt/data1/boreal/avirkkala/abcflux_modeling/environmental_coverage/mess_19951999.csv", row.names=TRUE)
pred_rast_static_m  <- as.matrix(mess_coord[]) 
r <- rast(pred_rast_static_m[], type="xyz")
plot(r)
#Negative values indicate localities that are environmentally dissimilar from the reference region. 
#The last column, MoD, indicates which of the column names of P corresponds to the most dissimilar variable, i.e., the limiting factor or the variable that drives the MESS in that locality (Elith et al. 2010).
# another plot
vals <- values(r)
m <- c(quantile(vals, .75, na.rm=TRUE), 0, -1,
       min(vals, na.rm=TRUE), quantile(vals, .25, na.rm=TRUE), -4,
       quantile(vals, .25, na.rm=TRUE), median(vals,  na.rm=TRUE), -3,
       median(vals,  na.rm=TRUE),quantile(vals, .75, na.rm=TRUE), -2,
       0, max(vals,  na.rm=TRUE), 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(r, rclmat, include.lowest=TRUE)
plot(rc1)



rm(list = c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
            Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
            vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
            Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
            Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
            trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
            pr_terraclimate_sites     , pdsi_terraclimate_sites))









### period 3: 2000-2004 ###

# model training data subset
modelsub <- subset(modeldata1, Meas_year<=2004 & Meas_year>=2000)
sp <- vect(modelsub, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
sp <- project(sp, UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)

# list rasters that include years
pattern1 <- seq(2000, 2004, by=1) %>% as.character()
rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km", pattern=paste0(pattern1, collapse="|"))
rasters <- rasters[endsWith(rasters, ".tif")]

setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km") 
r <- rasters[grepl( "srad", rasters)]
srad_terraclimate_sites <- mean(rast(r)/10)
srad <- extract(srad_terraclimate_sites, sp)

r <- rasters[grepl( "ndvi", rasters)]
ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled  <- mean(rast(r)/10000)
ndvi <- extract(ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled, sp)

r <- rasters[grepl( "soiltemplevel1_", rasters)]
Soil.temperature.level.1_era5_soilmoist_temp_snow <- mean(rast(r)/100)
soiltemp <- extract(Soil.temperature.level.1_era5_soilmoist_temp_snow, sp)

r <- rasters[grepl( "tmean_", rasters)]
tmean_terraclimate_sites  <- mean(rast(r)/100)
tmean <- extract(tmean_terraclimate_sites, sp)

r <- rasters[grepl( "vpd", rasters)]
vpd_terraclimate_sites  <- mean(rast(r))
vpd <- extract(vpd_terraclimate_sites, sp)

r <- rasters[grepl( "snowcover", rasters)]
Snow.cover_era5_soilmoist_temp_snow   <- mean(rast(r)/100)
snowcover <- extract(Snow.cover_era5_soilmoist_temp_snow , sp)

r <- rasters[grepl( "soilmoist", rasters)]
Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
soilmoist <- extract(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "NonTree_Vegetation", rasters)]
Percent_NonTree_Vegetation_AVHRR_VCF5KYR     <- mean(rast(r))
NonTree_Vegetation <- extract(Percent_NonTree_Vegetation_AVHRR_VCF5KYR  , sp)

r <- rasters[grepl( "TreeCover", rasters)]
Percent_TreeCover_AVHRR_VCF5KYR      <- mean(rast(r))
TreeCover <- extract(Percent_TreeCover_AVHRR_VCF5KYR   , sp)

r <- rasters[grepl( "NonVegetated", rasters)]
Percent_NonVegetated_AVHRR_VCF5KYR       <- mean(rast(r))
NonVegetated <- extract(Percent_NonVegetated_AVHRR_VCF5KYR    , sp)


r <- rasters[grepl( "tmean20", rasters)]
trend_20yrprior_terra_change_id        <- mean(rast(r)/1000)
tmeantrend <- extract(trend_20yrprior_terra_change_id     , sp)

r <- rasters[grepl( "snowdepth", rasters)]
Snow.depth_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
snowdepth <- extract(Snow.depth_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "ppt", rasters)]
pr_terraclimate_sites     <- mean(rast(r)/100)
ppt <- extract(pr_terraclimate_sites   , sp)

r <- rasters[grepl( "pdsi", rasters)]
pdsi_terraclimate_sites      <- mean(rast(r))
pdsi <- extract(pdsi_terraclimate_sites    , sp)

# static extractions
agb <- extract(aboveground_biomass_carbon_2010_Above_belowground_biomass     , sp)
bgb <- extract(belowground_biomass_carbon_2010_Above_belowground_biomass      , sp)
tmeanav <- extract(tmean_TerraClimate_averages, sp)
soc <- extract(SoilGrids_SOC_SoilGrids_SOCstock , sp)
scale <- extract(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m , sp)
bulk <- extract(BLDFIE_M_sl1_250m_ll_SoilGrids  , sp)
cti <- extract(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m   , sp)
soilwat <- extract(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent    , sp)
pptav <- extract(ppt_TerraClimate_averages     , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
ph <- extract(PHIHOX_M_sl1_250m_ll_SoilGrids      , sp)
perma <- extract(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH       , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
terratrend60 <- extract(terra_trend_19601990      , sp)


rasters <- c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
             Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
             vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
             Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
             Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
             trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
             pr_terraclimate_sites     , pdsi_terraclimate_sites,
             
             aboveground_biomass_carbon_2010_Above_belowground_biomass, belowground_biomass_carbon_2010_Above_belowground_biomass,
             tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock,
             dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, BLDFIE_M_sl1_250m_ll_SoilGrids,
             dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent,
             ppt_TerraClimate_averages, ndvi_trend_19812010, PHIHOX_M_sl1_250m_ll_SoilGrids, 
             UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, terra_trend_19601990)

rasters_matrix <- as.data.frame(rasters, xy=TRUE)
names(rasters_matrix) <- c("x", "y", "srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                           "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                           "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                           "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

p2_insitu <- data.frame(cbind(srad[,2], ndvi[, 2], soiltemp [, 2], tmean [, 2], vpd [, 2], snowcover [, 2],
                              soilmoist [, 2], NonTree_Vegetation [, 2], TreeCover [, 2], NonVegetated [, 2], tmeantrend [, 2],
                              snowdepth [, 2], ppt [, 2],pdsi [, 2],agb[, 2],bgb[, 2],
                              tmeanav[, 2],soc[, 2],scale[, 2],bulk[, 2],
                              cti[, 2],soilwat[, 2],pptav[, 2],ndvitrend[, 2],
                              ph[, 2],perma[, 2],terratrend60[, 2]))

names(p2_insitu) <- c("srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                      "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                      "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                      "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

colnames(rasters_matrix) %in% colnames(p2_insitu)

mess <- MESS(p2_insitu, rasters_matrix[, 3:ncol(rasters_matrix)])

mess_coord <- data.frame(cbind(rasters_matrix[, 1:2], mess[, ncol(mess)-1]))
mess_coord_all <- data.frame(cbind(rasters_matrix[, 1:2], mess[, (ncol(mess)-1) : (ncol(mess))]))

write.csv(mess_coord_all, "/mnt/data1/boreal/avirkkala/abcflux_modeling/environmental_coverage/mess_20002004.csv", row.names=TRUE)
pred_rast_static_m  <- as.matrix(mess_coord[]) 
r <- rast(pred_rast_static_m[], type="xyz")
plot(r)
#Negative values indicate localities that are environmentally dissimilar from the reference region. 
#The last column, MoD, indicates which of the column names of P corresponds to the most dissimilar variable, i.e., the limiting factor or the variable that drives the MESS in that locality (Elith et al. 2010).
# another plot
vals <- values(r)
m <- c(quantile(vals, .75, na.rm=TRUE), 0, -1,
       min(vals, na.rm=TRUE), quantile(vals, .25, na.rm=TRUE), -4,
       quantile(vals, .25, na.rm=TRUE), median(vals,  na.rm=TRUE), -3,
       median(vals,  na.rm=TRUE),quantile(vals, .75, na.rm=TRUE), -2,
       0, max(vals,  na.rm=TRUE), 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(r, rclmat, include.lowest=TRUE)
plot(rc1)



rm(list = c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
            Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
            vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
            Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
            Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
            trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
            pr_terraclimate_sites     , pdsi_terraclimate_sites))





### period 3: 2005-2009 ###

# model training data subset
modelsub <- subset(modeldata1, Meas_year<=2009 & Meas_year>=2005)
sp <- vect(modelsub, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
sp <- project(sp, UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)

# list rasters that include years
pattern1 <- seq(2005, 2009, by=1) %>% as.character()
rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km", pattern=paste0(pattern1, collapse="|"))
rasters <- rasters[endsWith(rasters, ".tif")]

setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km") 
r <- rasters[grepl( "srad", rasters)]
srad_terraclimate_sites <- mean(rast(r)/10)
srad <- extract(srad_terraclimate_sites, sp)

r <- rasters[grepl( "ndvi", rasters)]
ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled  <- mean(rast(r)/10000)
ndvi <- extract(ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled, sp)

r <- rasters[grepl( "soiltemplevel1_", rasters)]
Soil.temperature.level.1_era5_soilmoist_temp_snow <- mean(rast(r)/100)
soiltemp <- extract(Soil.temperature.level.1_era5_soilmoist_temp_snow, sp)

r <- rasters[grepl( "tmean_", rasters)]
tmean_terraclimate_sites  <- mean(rast(r)/100)
tmean <- extract(tmean_terraclimate_sites, sp)

r <- rasters[grepl( "vpd", rasters)]
vpd_terraclimate_sites  <- mean(rast(r))
vpd <- extract(vpd_terraclimate_sites, sp)

r <- rasters[grepl( "snowcover", rasters)]
Snow.cover_era5_soilmoist_temp_snow   <- mean(rast(r)/100)
snowcover <- extract(Snow.cover_era5_soilmoist_temp_snow , sp)

r <- rasters[grepl( "soilmoist", rasters)]
Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
soilmoist <- extract(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "NonTree_Vegetation", rasters)]
Percent_NonTree_Vegetation_AVHRR_VCF5KYR     <- mean(rast(r))
NonTree_Vegetation <- extract(Percent_NonTree_Vegetation_AVHRR_VCF5KYR  , sp)

r <- rasters[grepl( "TreeCover", rasters)]
Percent_TreeCover_AVHRR_VCF5KYR      <- mean(rast(r))
TreeCover <- extract(Percent_TreeCover_AVHRR_VCF5KYR   , sp)

r <- rasters[grepl( "NonVegetated", rasters)]
Percent_NonVegetated_AVHRR_VCF5KYR       <- mean(rast(r))
NonVegetated <- extract(Percent_NonVegetated_AVHRR_VCF5KYR    , sp)


r <- rasters[grepl( "tmean20", rasters)]
trend_20yrprior_terra_change_id        <- mean(rast(r)/1000)
tmeantrend <- extract(trend_20yrprior_terra_change_id     , sp)

r <- rasters[grepl( "snowdepth", rasters)]
Snow.depth_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
snowdepth <- extract(Snow.depth_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "ppt", rasters)]
pr_terraclimate_sites     <- mean(rast(r)/100)
ppt <- extract(pr_terraclimate_sites   , sp)

r <- rasters[grepl( "pdsi", rasters)]
pdsi_terraclimate_sites      <- mean(rast(r))
pdsi <- extract(pdsi_terraclimate_sites    , sp)

# static extractions
agb <- extract(aboveground_biomass_carbon_2010_Above_belowground_biomass     , sp)
bgb <- extract(belowground_biomass_carbon_2010_Above_belowground_biomass      , sp)
tmeanav <- extract(tmean_TerraClimate_averages, sp)
soc <- extract(SoilGrids_SOC_SoilGrids_SOCstock , sp)
scale <- extract(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m , sp)
bulk <- extract(BLDFIE_M_sl1_250m_ll_SoilGrids  , sp)
cti <- extract(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m   , sp)
soilwat <- extract(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent    , sp)
pptav <- extract(ppt_TerraClimate_averages     , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
ph <- extract(PHIHOX_M_sl1_250m_ll_SoilGrids      , sp)
perma <- extract(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH       , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
terratrend60 <- extract(terra_trend_19601990      , sp)


rasters <- c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
             Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
             vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
             Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
             Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
             trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
             pr_terraclimate_sites     , pdsi_terraclimate_sites,
             
             aboveground_biomass_carbon_2010_Above_belowground_biomass, belowground_biomass_carbon_2010_Above_belowground_biomass,
             tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock,
             dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, BLDFIE_M_sl1_250m_ll_SoilGrids,
             dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent,
             ppt_TerraClimate_averages, ndvi_trend_19812010, PHIHOX_M_sl1_250m_ll_SoilGrids, 
             UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, terra_trend_19601990)

rasters_matrix <- as.data.frame(rasters, xy=TRUE)
names(rasters_matrix) <- c("x", "y", "srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                           "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                           "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                           "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

p2_insitu <- data.frame(cbind(srad[,2], ndvi[, 2], soiltemp [, 2], tmean [, 2], vpd [, 2], snowcover [, 2],
                              soilmoist [, 2], NonTree_Vegetation [, 2], TreeCover [, 2], NonVegetated [, 2], tmeantrend [, 2],
                              snowdepth [, 2], ppt [, 2],pdsi [, 2],agb[, 2],bgb[, 2],
                              tmeanav[, 2],soc[, 2],scale[, 2],bulk[, 2],
                              cti[, 2],soilwat[, 2],pptav[, 2],ndvitrend[, 2],
                              ph[, 2],perma[, 2],terratrend60[, 2]))

names(p2_insitu) <- c("srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                      "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                      "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                      "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

colnames(rasters_matrix) %in% colnames(p2_insitu)

mess <- MESS(p2_insitu, rasters_matrix[, 3:ncol(rasters_matrix)])

mess_coord <- data.frame(cbind(rasters_matrix[, 1:2], mess[, ncol(mess)-1]))
mess_coord_all <- data.frame(cbind(rasters_matrix[, 1:2], mess[, (ncol(mess)-1) : (ncol(mess))]))

write.csv(mess_coord_all, "/mnt/data1/boreal/avirkkala/abcflux_modeling/environmental_coverage/mess_20052009.csv", row.names=TRUE)
pred_rast_static_m  <- as.matrix(mess_coord[]) 
r <- rast(pred_rast_static_m[], type="xyz")
plot(r)
#Negative values indicate localities that are environmentally dissimilar from the reference region. 
#The last column, MoD, indicates which of the column names of P corresponds to the most dissimilar variable, i.e., the limiting factor or the variable that drives the MESS in that locality (Elith et al. 2010).
# another plot
vals <- values(r)
m <- c(quantile(vals, .75, na.rm=TRUE), 0, -1,
       min(vals, na.rm=TRUE), quantile(vals, .25, na.rm=TRUE), -4,
       quantile(vals, .25, na.rm=TRUE), median(vals,  na.rm=TRUE), -3,
       median(vals,  na.rm=TRUE),quantile(vals, .75, na.rm=TRUE), -2,
       0, max(vals,  na.rm=TRUE), 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(r, rclmat, include.lowest=TRUE)
plot(rc1)



rm(list = c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
            Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
            vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
            Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
            Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
            trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
            pr_terraclimate_sites     , pdsi_terraclimate_sites))







### period 3: 2010-2014 ###

# model training data subset
modelsub <- subset(modeldata1, Meas_year<=2014 & Meas_year>=2010)
sp <- vect(modelsub, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
sp <- project(sp, UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)

# list rasters that include years
pattern1 <- seq(2010, 2014, by=1) %>% as.character()
rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km", pattern=paste0(pattern1, collapse="|"))
rasters <- rasters[endsWith(rasters, ".tif")]

setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km") 
r <- rasters[grepl( "srad", rasters)]
srad_terraclimate_sites <- mean(rast(r)/10)
srad <- extract(srad_terraclimate_sites, sp)

r <- rasters[grepl( "ndvi", rasters)]
ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled  <- mean(rast(r)/10000)
ndvi <- extract(ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled, sp)

r <- rasters[grepl( "soiltemplevel1_", rasters)]
Soil.temperature.level.1_era5_soilmoist_temp_snow <- mean(rast(r)/100)
soiltemp <- extract(Soil.temperature.level.1_era5_soilmoist_temp_snow, sp)

r <- rasters[grepl( "tmean_", rasters)]
tmean_terraclimate_sites  <- mean(rast(r)/100)
tmean <- extract(tmean_terraclimate_sites, sp)

r <- rasters[grepl( "vpd", rasters)]
vpd_terraclimate_sites  <- mean(rast(r))
vpd <- extract(vpd_terraclimate_sites, sp)

r <- rasters[grepl( "snowcover", rasters)]
Snow.cover_era5_soilmoist_temp_snow   <- mean(rast(r)/100)
snowcover <- extract(Snow.cover_era5_soilmoist_temp_snow , sp)

r <- rasters[grepl( "soilmoist", rasters)]
Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
soilmoist <- extract(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "NonTree_Vegetation", rasters)]
Percent_NonTree_Vegetation_AVHRR_VCF5KYR     <- mean(rast(r))
NonTree_Vegetation <- extract(Percent_NonTree_Vegetation_AVHRR_VCF5KYR  , sp)

r <- rasters[grepl( "TreeCover", rasters)]
Percent_TreeCover_AVHRR_VCF5KYR      <- mean(rast(r))
TreeCover <- extract(Percent_TreeCover_AVHRR_VCF5KYR   , sp)

r <- rasters[grepl( "NonVegetated", rasters)]
Percent_NonVegetated_AVHRR_VCF5KYR       <- mean(rast(r))
NonVegetated <- extract(Percent_NonVegetated_AVHRR_VCF5KYR    , sp)


r <- rasters[grepl( "tmean20", rasters)]
trend_20yrprior_terra_change_id        <- mean(rast(r)/1000)
tmeantrend <- extract(trend_20yrprior_terra_change_id     , sp)

r <- rasters[grepl( "snowdepth", rasters)]
Snow.depth_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
snowdepth <- extract(Snow.depth_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "ppt", rasters)]
pr_terraclimate_sites     <- mean(rast(r)/100)
ppt <- extract(pr_terraclimate_sites   , sp)

r <- rasters[grepl( "pdsi", rasters)]
pdsi_terraclimate_sites      <- mean(rast(r))
pdsi <- extract(pdsi_terraclimate_sites    , sp)

# static extractions
agb <- extract(aboveground_biomass_carbon_2010_Above_belowground_biomass     , sp)
bgb <- extract(belowground_biomass_carbon_2010_Above_belowground_biomass      , sp)
tmeanav <- extract(tmean_TerraClimate_averages, sp)
soc <- extract(SoilGrids_SOC_SoilGrids_SOCstock , sp)
scale <- extract(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m , sp)
bulk <- extract(BLDFIE_M_sl1_250m_ll_SoilGrids  , sp)
cti <- extract(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m   , sp)
soilwat <- extract(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent    , sp)
pptav <- extract(ppt_TerraClimate_averages     , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
ph <- extract(PHIHOX_M_sl1_250m_ll_SoilGrids      , sp)
perma <- extract(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH       , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
terratrend60 <- extract(terra_trend_19601990      , sp)


rasters <- c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
             Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
             vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
             Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
             Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
             trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
             pr_terraclimate_sites     , pdsi_terraclimate_sites,
             
             aboveground_biomass_carbon_2010_Above_belowground_biomass, belowground_biomass_carbon_2010_Above_belowground_biomass,
             tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock,
             dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, BLDFIE_M_sl1_250m_ll_SoilGrids,
             dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent,
             ppt_TerraClimate_averages, ndvi_trend_19812010, PHIHOX_M_sl1_250m_ll_SoilGrids, 
             UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, terra_trend_19601990)

rasters_matrix <- as.data.frame(rasters, xy=TRUE)
names(rasters_matrix) <- c("x", "y", "srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                           "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                           "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                           "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

p2_insitu <- data.frame(cbind(srad[,2], ndvi[, 2], soiltemp [, 2], tmean [, 2], vpd [, 2], snowcover [, 2],
                              soilmoist [, 2], NonTree_Vegetation [, 2], TreeCover [, 2], NonVegetated [, 2], tmeantrend [, 2],
                              snowdepth [, 2], ppt [, 2],pdsi [, 2],agb[, 2],bgb[, 2],
                              tmeanav[, 2],soc[, 2],scale[, 2],bulk[, 2],
                              cti[, 2],soilwat[, 2],pptav[, 2],ndvitrend[, 2],
                              ph[, 2],perma[, 2],terratrend60[, 2]))

names(p2_insitu) <- c("srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                      "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                      "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                      "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

colnames(rasters_matrix) %in% colnames(p2_insitu)

mess <- MESS(p2_insitu, rasters_matrix[, 3:ncol(rasters_matrix)])

mess_coord <- data.frame(cbind(rasters_matrix[, 1:2], mess[, ncol(mess)-1]))
mess_coord_all <- data.frame(cbind(rasters_matrix[, 1:2], mess[, (ncol(mess)-1) : (ncol(mess))]))

write.csv(mess_coord_all, "/mnt/data1/boreal/avirkkala/abcflux_modeling/environmental_coverage/mess_20102014.csv", row.names=TRUE)
pred_rast_static_m  <- as.matrix(mess_coord[]) 
r <- rast(pred_rast_static_m[], type="xyz")
plot(r)
#Negative values indicate localities that are environmentally dissimilar from the reference region. 
#The last column, MoD, indicates which of the column names of P corresponds to the most dissimilar variable, i.e., the limiting factor or the variable that drives the MESS in that locality (Elith et al. 2010).
# another plot
vals <- values(r)
m <- c(quantile(vals, .75, na.rm=TRUE), 0, -1,
       min(vals, na.rm=TRUE), quantile(vals, .25, na.rm=TRUE), -4,
       quantile(vals, .25, na.rm=TRUE), median(vals,  na.rm=TRUE), -3,
       median(vals,  na.rm=TRUE),quantile(vals, .75, na.rm=TRUE), -2,
       0, max(vals,  na.rm=TRUE), 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(r, rclmat, include.lowest=TRUE)
plot(rc1)



rm(list = c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
            Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
            vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
            Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
            Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
            trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
            pr_terraclimate_sites     , pdsi_terraclimate_sites))





### period 3: 2015-2020 ###
# should probably use 1 km predictions for this....

# model training data subset
modelsub <- subset(modeldata1, Meas_year<=2020 & Meas_year>=2015)
sp <- vect(modelsub, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
sp <- project(sp, UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)

# list rasters that include years
pattern1 <- seq(2015, 2016, by=1) %>% as.character()
rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km", pattern=paste0(pattern1, collapse="|"))
rasters <- rasters[endsWith(rasters, ".tif")]

setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_8km") 
r <- rasters[grepl( "srad", rasters)]
srad_terraclimate_sites <- mean(rast(r)/10)
srad <- extract(srad_terraclimate_sites, sp)

r <- rasters[grepl( "ndvi", rasters)]
ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled  <- mean(rast(r)/10000)
ndvi <- extract(ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled, sp)

r <- rasters[grepl( "soiltemplevel1_", rasters)]
Soil.temperature.level.1_era5_soilmoist_temp_snow <- mean(rast(r)/100)
soiltemp <- extract(Soil.temperature.level.1_era5_soilmoist_temp_snow, sp)

r <- rasters[grepl( "tmean_", rasters)]
tmean_terraclimate_sites  <- mean(rast(r)/100)
tmean <- extract(tmean_terraclimate_sites, sp)

r <- rasters[grepl( "vpd", rasters)]
vpd_terraclimate_sites  <- mean(rast(r))
vpd <- extract(vpd_terraclimate_sites, sp)

r <- rasters[grepl( "snowcover", rasters)]
Snow.cover_era5_soilmoist_temp_snow   <- mean(rast(r)/100)
snowcover <- extract(Snow.cover_era5_soilmoist_temp_snow , sp)

r <- rasters[grepl( "soilmoist", rasters)]
Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
soilmoist <- extract(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "NonTree_Vegetation", rasters)]
Percent_NonTree_Vegetation_AVHRR_VCF5KYR     <- mean(rast(r))
NonTree_Vegetation <- extract(Percent_NonTree_Vegetation_AVHRR_VCF5KYR  , sp)

r <- rasters[grepl( "TreeCover", rasters)]
Percent_TreeCover_AVHRR_VCF5KYR      <- mean(rast(r))
TreeCover <- extract(Percent_TreeCover_AVHRR_VCF5KYR   , sp)

r <- rasters[grepl( "NonVegetated", rasters)]
Percent_NonVegetated_AVHRR_VCF5KYR       <- mean(rast(r))
NonVegetated <- extract(Percent_NonVegetated_AVHRR_VCF5KYR    , sp)


r <- rasters[grepl( "tmean20", rasters)]
trend_20yrprior_terra_change_id        <- mean(rast(r)/1000)
tmeantrend <- extract(trend_20yrprior_terra_change_id     , sp)

r <- rasters[grepl( "snowdepth", rasters)]
Snow.depth_era5_soilmoist_temp_snow    <- mean(rast(r)/100)
snowdepth <- extract(Snow.depth_era5_soilmoist_temp_snow  , sp)

r <- rasters[grepl( "ppt", rasters)]
pr_terraclimate_sites     <- mean(rast(r)/100)
ppt <- extract(pr_terraclimate_sites   , sp)

r <- rasters[grepl( "pdsi", rasters)]
pdsi_terraclimate_sites      <- mean(rast(r))
pdsi <- extract(pdsi_terraclimate_sites    , sp)

# static extractions
agb <- extract(aboveground_biomass_carbon_2010_Above_belowground_biomass     , sp)
bgb <- extract(belowground_biomass_carbon_2010_Above_belowground_biomass      , sp)
tmeanav <- extract(tmean_TerraClimate_averages, sp)
soc <- extract(SoilGrids_SOC_SoilGrids_SOCstock , sp)
scale <- extract(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m , sp)
bulk <- extract(BLDFIE_M_sl1_250m_ll_SoilGrids  , sp)
cti <- extract(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m   , sp)
soilwat <- extract(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent    , sp)
pptav <- extract(ppt_TerraClimate_averages     , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
ph <- extract(PHIHOX_M_sl1_250m_ll_SoilGrids      , sp)
perma <- extract(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH       , sp)
ndvitrend <- extract(ndvi_trend_19812010      , sp)
terratrend60 <- extract(terra_trend_19601990      , sp)


rasters <- c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
             Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
             vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
             Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
             Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
             trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
             pr_terraclimate_sites     , pdsi_terraclimate_sites,
             
             aboveground_biomass_carbon_2010_Above_belowground_biomass, belowground_biomass_carbon_2010_Above_belowground_biomass,
             tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock,
             dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, BLDFIE_M_sl1_250m_ll_SoilGrids,
             dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent,
             ppt_TerraClimate_averages, ndvi_trend_19812010, PHIHOX_M_sl1_250m_ll_SoilGrids, 
             UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, terra_trend_19601990)

rasters_matrix <- as.data.frame(rasters, xy=TRUE)
names(rasters_matrix) <- c("x", "y", "srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                           "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                           "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                           "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

p2_insitu <- data.frame(cbind(srad[,2], ndvi[, 2], soiltemp [, 2], tmean [, 2], vpd [, 2], snowcover [, 2],
                              soilmoist [, 2], NonTree_Vegetation [, 2], TreeCover [, 2], NonVegetated [, 2], tmeantrend [, 2],
                              snowdepth [, 2], ppt [, 2],pdsi [, 2],agb[, 2],bgb[, 2],
                              tmeanav[, 2],soc[, 2],scale[, 2],bulk[, 2],
                              cti[, 2],soilwat[, 2],pptav[, 2],ndvitrend[, 2],
                              ph[, 2],perma[, 2],terratrend60[, 2]))

names(p2_insitu) <- c("srad", "ndvi", "soiltemp", "tmean", "vpd", "snowcover",
                      "soilmoist", "nontree", "treecover", "nonveg", "tmeantrend20", "snowdepth", "ppt",
                      "pdsi", "agb", "bgb", "tmeanav", "soc", "scale", "bulk", "cti",
                      "watercont", "pptav", "ndvitrend", "ph", "perma", "tmeantrend")

colnames(rasters_matrix) %in% colnames(p2_insitu)

mess <- MESS(p2_insitu, rasters_matrix[, 3:ncol(rasters_matrix)])

mess_coord <- data.frame(cbind(rasters_matrix[, 1:2], mess[, ncol(mess)-1]))
mess_coord_all <- data.frame(cbind(rasters_matrix[, 1:2], mess[, (ncol(mess)-1) : (ncol(mess))]))

write.csv(mess_coord_all, "/mnt/data1/boreal/avirkkala/abcflux_modeling/environmental_coverage/mess_20152020.csv", row.names=TRUE)
pred_rast_static_m  <- as.matrix(mess_coord[]) 
r <- rast(pred_rast_static_m[], type="xyz")
plot(r)
#Negative values indicate localities that are environmentally dissimilar from the reference region. 
#The last column, MoD, indicates which of the column names of P corresponds to the most dissimilar variable, i.e., the limiting factor or the variable that drives the MESS in that locality (Elith et al. 2010).
# another plot
vals <- values(r)
m <- c(quantile(vals, .75, na.rm=TRUE), 0, -1,
       min(vals, na.rm=TRUE), quantile(vals, .25, na.rm=TRUE), -4,
       quantile(vals, .25, na.rm=TRUE), median(vals,  na.rm=TRUE), -3,
       median(vals,  na.rm=TRUE),quantile(vals, .75, na.rm=TRUE), -2,
       0, max(vals,  na.rm=TRUE), 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(r, rclmat, include.lowest=TRUE)
plot(rc1)



rm(list = c(srad_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
            Soil.temperature.level.1_era5_soilmoist_temp_snow , tmean_terraclimate_sites  ,
            vpd_terraclimate_sites  , Snow.cover_era5_soilmoist_temp_snow   ,
            Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow    , Percent_NonTree_Vegetation_AVHRR_VCF5KYR     ,
            Percent_TreeCover_AVHRR_VCF5KYR      , Percent_NonVegetated_AVHRR_VCF5KYR       ,
            trend_20yrprior_terra_change_id        , Snow.depth_era5_soilmoist_temp_snow    ,
            pr_terraclimate_sites     , pdsi_terraclimate_sites))











### Visualize


list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/environmental_coverage")
setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/environmental_coverage")
d1 <- read.csv("mess_19901994.csv")
d2 <- read.csv("mess_19951999.csv")
d3 <- read.csv("mess_20002004.csv")
d4 <- read.csv("mess_20102014.csv")
d5 <- read.csv("mess_20152020.csv")

mess_coord <- d1[, c(2, 3, 4)]
pred_rast_static_m  <- as.matrix(mess_coord[]) 
r1 <- rast(pred_rast_static_m[], type="xyz")
plot(r1)


mess_coord <- d2[, c(2, 3, 4)]
pred_rast_static_m  <- as.matrix(mess_coord[]) 
r2 <- rast(pred_rast_static_m[], type="xyz")
plot(r2)


mess_coord <- d3[, c(2, 3, 4)]
pred_rast_static_m  <- as.matrix(mess_coord[]) 
r3 <- rast(pred_rast_static_m[], type="xyz")
plot(r3)

mess_coord <- d4[, c(2, 3, 4)]
pred_rast_static_m  <- as.matrix(mess_coord[]) 
r4 <- rast(pred_rast_static_m[], type="xyz")
plot(r4)


mess_coord <- d5[, c(2, 3, 4)]
pred_rast_static_m  <- as.matrix(mess_coord[]) 
r5 <- rast(pred_rast_static_m[], type="xyz")
plot(r5)




# reclassify
vals <- values(r3)
summary(vals)
m <- c(-20, 0, -1,
       -80, -20, -2,
       -200, -80, -3,
       -100000, -80, -4,
       0, 5, 1,
       5, 40, 2)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

rc1 <- classify(r1, rclmat, include.lowest=TRUE)
plot(rc1)

rc2 <- classify(r2, rclmat, include.lowest=TRUE)
plot(rc2)

rc3 <- classify(r3, rclmat, include.lowest=TRUE)
plot(rc3)

rc4 <- classify(r4, rclmat, include.lowest=TRUE)
plot(rc4)

rc5 <- classify(r5, rclmat, include.lowest=TRUE)
plot(rc5)

### Same but include only winter months!