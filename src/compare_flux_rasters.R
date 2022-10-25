

### Load the model training data just in case
d <- read.csv("/home/master/cloud/flux_upscaling_data/results/final/modeldata_avg.csv") 



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


modeldata1 <- d[,c("Study_ID_Short", "Latitude", "Longitude", "Meas_year", "id", "NEE_gC_m2", Baseline_vars_20km)]
modeldata1 <- na.omit(modeldata1)


d2 <- modeldata1 %>% group_by(Study_ID_Short, Meas_year, Latitude, Longitude) %>% summarize(nee=sum(NEE_gC_m2), n=n()) %>% filter(n==12)
nrow(d2)
length(unique(d2$Study_ID_Short))
# 55 sites and 226 obs

d3 <- vect(d2, geom=c("Longitude", "Latitude"), crs="+init=epsg:4326", keepgeom=FALSE)

files <- list.files("/home/master/local_outputs/predictions_8km/raster/0.5/")
files2 <- files[nchar(files)==18]
setwd("/home/master/local_outputs/predictions_8km/raster/0.5/")
r <- rast(files3)

names(r) <- seq(1982, 2016, by=1)

d4 <- project(d3, r)
r2 <-extract(r, d4)

d5 <- cbind(d4, r2)

plot(d5$`2006`/1000, d5$nee, xlim=c(-400, 100), ylim=c(-400, 100))
abline(0,1)
