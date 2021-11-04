


########### ANNA MUSITA TÄMÄ https://stackoverflow.com/questions/25121725/error-in-predicting-raster-with-randomforest-caret-and-factor-variables

# Packages
library("caret")
library("dplyr")
library("raster")
library("terra")
#library("terra", lib.loc="/mnt/data1/boreal/avirkkala/packages")

library(stringr)

terraOptions(memfrac=0.95, tempdir = "/mnt/data1/boreal/avirkkala/Temp") # testing a different style


### Data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")



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


crops <- rbind(c(-4834843, 0, -4834843, 0),  
               c(0, 4834843, -4834843, 0),
               c(-4834843, 0, 0, 4834843),  
               c(0, 4834843, 0, 4834843))



# creating predictions in two parts: NA and Siberia
### Loop over times and load dynamic vars


for (c in 1:2) {
  # c <- 1
  print(c)
  
  for (km in kms) {
    
    print(km)
    # km <- "1km"
    # i <- "NEE_gC_m2"
    
    
    if (km=="1km") {
      
      ### Load static vars (only once)
      setwd("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictors_1km")
      
      ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_northpolelambert1km_tundraboreal_attfix.tif")
      ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- crop(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, ext(crops[c,]))
      
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
      tmean_TerraClimate_averages <- crop(tmean_TerraClimate_averages, ext(crops[c,]))
      
      
      SoilGrids_SOC_SoilGrids_SOCstock <-  rast("soc.tif")
      plot(SoilGrids_SOC_SoilGrids_SOCstock)
      SoilGrids_SOC_SoilGrids_SOCstock
      summary(d$SoilGrids_SOC_SoilGrids_SOCstock)
      SoilGrids_SOC_SoilGrids_SOCstock <- SoilGrids_SOC_SoilGrids_SOCstock/100
      SoilGrids_SOC_SoilGrids_SOCstock <- crop(SoilGrids_SOC_SoilGrids_SOCstock, ext(crops[c,]))
      
      
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("roughscale.tif")
      plot(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
      summary(d$dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m/100
      dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- crop(dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, ext(crops[c,]))
      
      
      terra_trend_19812010<- rast("tmean_trend_19812010.tif")
      plot(terra_trend_19812010)
      terra_trend_19812010
      summary(d$terra_trend_19812010)
      terra_trend_19812010 <- terra_trend_19812010/100 ### EI TOIMI
      terra_trend_19812010 <- crop(terra_trend_19812010, ext(crops[c,]))
      
      
      wtd_Water_table_depth <- rast("wtd.tif") ## EI TOIMI; PALJON TYGJÄÄ!!!
      plot(wtd_Water_table_depth)
      wtd_Water_table_depth
      summary(d$wtd_Water_table_depth)
      wtd_Water_table_depth <- wtd_Water_table_depth/100  ############## TÄMÄ IHAN KUMMALLINEN - ARVOT PITÄISI OLLA NEGATIIVISIA?????????
      wtd_Water_table_depth <- crop(wtd_Water_table_depth, ext(crops[c,]))
      
      
      belowground_biomass_carbon_2010_Above_belowground_biomass <- rast("belowgroundbiomass.tif") ## ONKO TÄMä UNCERTAINTY??
      plot(belowground_biomass_carbon_2010_Above_belowground_biomass)
      belowground_biomass_carbon_2010_Above_belowground_biomass
      summary(d$belowground_biomass_carbon_2010_Above_belowground_biomass)
      belowground_biomass_carbon_2010_Above_belowground_biomass <- belowground_biomass_carbon_2010_Above_belowground_biomass/100 
      belowground_biomass_carbon_2010_Above_belowground_biomass <- crop(belowground_biomass_carbon_2010_Above_belowground_biomass, ext(crops[c,]))
      
      
      BLDFIE_M_sl1_250m_ll_SoilGrids <- rast("bulkdensity.tif")
      plot(BLDFIE_M_sl1_250m_ll_SoilGrids)
      BLDFIE_M_sl1_250m_ll_SoilGrids
      summary(d$BLDFIE_M_sl1_250m_ll_SoilGrids)
      BLDFIE_M_sl1_250m_ll_SoilGrids <- BLDFIE_M_sl1_250m_ll_SoilGrids/100 
      BLDFIE_M_sl1_250m_ll_SoilGrids <- crop(BLDFIE_M_sl1_250m_ll_SoilGrids, ext(crops[c,]))
      
      
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- rast("cti.tif")
      plot(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m
      summary(d$dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m/100 
      dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m <- crop(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, ext(crops[c,]))
      
      
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- rast("sol_watercontent.tif")
      plot(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent
      summary(d$sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent/100 
      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent <- crop(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent, ext(crops[c,]))
      
      
      ppt_TerraClimate_averages <- rast("Terraclimate_averages_ppt.tif")
      plot(ppt_TerraClimate_averages)
      ppt_TerraClimate_averages
      summary(d$ppt_TerraClimate_averages)
      ppt_TerraClimate_averages <- ppt_TerraClimate_averages/100 
      ppt_TerraClimate_averages <- crop(ppt_TerraClimate_averages, ext(crops[c,]))
      
      
      ndvi_trend_19812010 <-  rast("ndvi_trend_19812010.tif") 
      plot(ndvi_trend_19812010)
      ndvi_trend_19812010
      summary(d$ndvi_trend_19812010)
      ndvi_trend_19812010 <- ndvi_trend_19812010/100000 ##################### MAYBE CORRECT????
      ndvi_trend_19812010 <- crop(ndvi_trend_19812010, ext(crops[c,]))
      
      
      PHIHOX_M_sl1_250m_ll_SoilGrids <- rast("ph.tif")
      plot(PHIHOX_M_sl1_250m_ll_SoilGrids)
      PHIHOX_M_sl1_250m_ll_SoilGrids
      summary(d$PHIHOX_M_sl1_250m_ll_SoilGrids)
      PHIHOX_M_sl1_250m_ll_SoilGrids <- PHIHOX_M_sl1_250m_ll_SoilGrids/100 
      PHIHOX_M_sl1_250m_ll_SoilGrids <- crop(PHIHOX_M_sl1_250m_ll_SoilGrids, ext(crops[c,]))
      
      
      terra_trend_19601990 <- rast("tmean_trend_19601990.tif")
      plot(terra_trend_19601990)
      terra_trend_19601990
      summary(d$terra_trend_19601990)
      terra_trend_19601990 <- terra_trend_19601990/100 #### TÄMä NÄYTTÄISI SAMALTA KUIN TERRA_TREND 1981....??????
      terra_trend_19601990 <- crop(terra_trend_19601990, ext(crops[c,]))
      
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- rast("UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif")
      plot(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH
      summary(d$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH/100 
      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH <- crop(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, ext(crops[c,]))
      
      TKWP_Thermokarst <- rast("Circumpolar_Thermokarst_Landscapes_TKWP.tif")
      plot(TKWP_Thermokarst)
      summary(d$TKWP_Thermokarst)
      TKWP_Thermokarst <- crop(TKWP_Thermokarst, ext(crops[c,]))
      
      
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
      gc()
      
      # rm(list=c(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, aboveground_biomass_carbon_2010_Above_belowground_biomass,
      #           tmean_TerraClimate_averages, SoilGrids_SOC_SoilGrids_SOCstock, 
      #           dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m, terra_trend_19812010)) ## NOT WORKING???
      
      # rasters 2
      pred_rast2 <- c(wtd_Water_table_depth, belowground_biomass_carbon_2010_Above_belowground_biomass,
                      BLDFIE_M_sl1_250m_ll_SoilGrids, dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m,
                      sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent, ppt_TerraClimate_averages)
      
      names(pred_rast2) <- c("wtd_Water_table_depth", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                             "BLDFIE_M_sl1_250m_ll_SoilGrids", "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m",
                             "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", "ppt_TerraClimate_averages")
      
      rm(wtd_Water_table_depth)
      rm(belowground_biomass_carbon_2010_Above_belowground_biomass)
      rm(BLDFIE_M_sl1_250m_ll_SoilGrids)
      rm(sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent)
      rm(dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m)
      rm(ppt_TerraClimate_averages)
      gc()
      
      
      
      pred_rast3 <- c(PHIHOX_M_sl1_250m_ll_SoilGrids, terra_trend_19601990,
                      UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH, TKWP_Thermokarst, ndvi_trend_19812010)
      
      names(pred_rast3) <- c("PHIHOX_M_sl1_250m_ll_SoilGrids", "terra_trend_19601990",
                             "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", "TKWP_Thermokarst",
                             "ndvi_trend_19812010")
      
      rm(PHIHOX_M_sl1_250m_ll_SoilGrids)
      rm(terra_trend_19601990)
      rm(UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH)
      rm(TKWP_Thermokarst)
      rm(ndvi_trend_19812010)
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
      
      print("static vars loaded")
      
      
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
      
    }
    
    for (t in 1:length(time)) {
      
      # t <- 1
      srad_terraclimate_sites <- rast(paste0("srad_", time_alt[t], ".tif"))
      plot(srad_terraclimate_sites)
      srad_terraclimate_sites
      summary(d$srad_terraclimate_sites)
      #srad_terraclimate_sites <- srad_terraclimate_sites/100  ### NO CONVERSION NEEDED?
      srad_terraclimate_sites <- crop(srad_terraclimate_sites, ext(crops[c,]))
      
      
      Barrow_CO2_conc_Barrow_CO2conc <- rast(paste0("co2_", time_alt[t], ".tif"))
      plot(Barrow_CO2_conc_Barrow_CO2conc)
      Barrow_CO2_conc_Barrow_CO2conc
      summary(d$Barrow_CO2_conc_Barrow_CO2conc)
      Barrow_CO2_conc_Barrow_CO2conc <- Barrow_CO2_conc_Barrow_CO2conc/1000  
      Barrow_CO2_conc_Barrow_CO2conc <- crop(Barrow_CO2_conc_Barrow_CO2conc, ext(crops[c,]))
      
      NDVI_whittaker_constant_monthly_mean <- rast(paste0("ndvi_", time_alt[t], ".tif")) ## EI ARVOJA plotissa mutta valuesin kautta tulee??? mutta on arvoja raster-komennon kautta plotissa???
      #plot(NDVI_whittaker_constant_monthly_mean)
      NDVI_whittaker_constant_monthly_mean
      summary(d$NDVI_whittaker_constant_monthly_mean)
      NDVI_whittaker_constant_monthly_mean <- NDVI_whittaker_constant_monthly_mean/1000
      NDVI_whittaker_constant_monthly_mean <- crop(NDVI_whittaker_constant_monthly_mean, ext(crops[c,]))
      
      Soil.temperature.level.1_era5_soilmoist_temp_snow <- rast(paste0("soiltemplevel1_", time[t], ".tif"))
      plot(Soil.temperature.level.1_era5_soilmoist_temp_snow)
      Soil.temperature.level.1_era5_soilmoist_temp_snow
      summary(d$Soil.temperature.level.1_era5_soilmoist_temp_snow)
      Soil.temperature.level.1_era5_soilmoist_temp_snow <- Soil.temperature.level.1_era5_soilmoist_temp_snow/100
      Soil.temperature.level.1_era5_soilmoist_temp_snow <- crop(Soil.temperature.level.1_era5_soilmoist_temp_snow, ext(crops[c,]))
      
      water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality <- rast(paste0("ndii_", substr(time[t],1, 4), ".tif"))
      plot(water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality)
      water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality
      summary(d$water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality)
      water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality <- water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality/100
      water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality <- crop(water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality, ext(crops[c,]))
      
      water_ground_MCD43A4_annual_water_ground_sites_low_quality <- rast(paste0("ndwi_", substr(time[t],1, 4), ".tif"))
      plot(water_ground_MCD43A4_annual_water_ground_sites_low_quality)
      water_ground_MCD43A4_annual_water_ground_sites_low_quality
      summary(d$water_ground_MCD43A4_annual_water_ground_sites_low_quality)
      water_ground_MCD43A4_annual_water_ground_sites_low_quality <- water_ground_MCD43A4_annual_water_ground_sites_low_quality/100
      water_ground_MCD43A4_annual_water_ground_sites_low_quality <- crop(water_ground_MCD43A4_annual_water_ground_sites_low_quality, ext(crops[c,]))
      
      LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality <- rast(paste0("lst_", time_alt[t], ".tif"))
      plot(LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality)
      LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality
      summary(d$LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality)
      LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality <- LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality/100
      LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality <- crop(LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality, ext(crops[c,]))
      
      vpd_terraclimate_sites <- rast(paste0("vpd_", time_alt[t], ".tif"))
      plot(vpd_terraclimate_sites)
      vpd_terraclimate_sites
      summary(d$vpd_terraclimate_sites)
      #vpd_terraclimate_sites <- vpd_terraclimate_sites/100  ### NO CONVERSION NEEDED?
      vpd_terraclimate_sites <- crop(vpd_terraclimate_sites, ext(crops[c,]))
      
      Snow.cover_era5_soilmoist_temp_snow <- rast(paste0("snowcover_", time[t], ".tif"))
      plot(Snow.cover_era5_soilmoist_temp_snow)
      Snow.cover_era5_soilmoist_temp_snow
      summary(d$Snow.cover_era5_soilmoist_temp_snow)
      Snow.cover_era5_soilmoist_temp_snow <- Snow.cover_era5_soilmoist_temp_snow/100
      Snow.cover_era5_soilmoist_temp_snow <- crop(Snow.cover_era5_soilmoist_temp_snow, ext(crops[c,]))
      
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- rast(paste0("soilmoistlevel1_", time[t], ".tif"))
      plot(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow
      summary(d$Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow/100
      Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow <- crop(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow, ext(crops[c,]))
      
      Percent_Tree_Cover_MOD44B_sites <- raster::stack(paste0("Percent_Tree_Cover_mod44b", substr(time[t],1, 4), ".tif")) # THE FIRST LAYER!!!!
      print(Percent_Tree_Cover_MOD44B_sites)
      Percent_Tree_Cover_MOD44B_sites <- rast(Percent_Tree_Cover_MOD44B_sites[[1]])
      print(Percent_Tree_Cover_MOD44B_sites)
      plot(Percent_Tree_Cover_MOD44B_sites)
      Percent_Tree_Cover_MOD44B_sites
      summary(d$Percent_Tree_Cover_MOD44B_sites)
      Percent_Tree_Cover_MOD44B_sites <- crop(Percent_Tree_Cover_MOD44B_sites, ext(crops[c,]))
      
      Percent_NonTree_Vegetation_MOD44B_sites <- raster::stack(paste0("Percent_NonTree_Vegetation_mod44b", substr(time[t],1, 4), ".tif"))  # THE SECOND LAYER!!!! (non-vegetated is the third)
      Percent_NonTree_Vegetation_MOD44B_sites <- rast(Percent_NonTree_Vegetation_MOD44B_sites[[2]])
      plot(Percent_Tree_Cover_MOD44B_sites)
      Percent_NonTree_Vegetation_MOD44B_sites
      summary(d$Percent_NonTree_Vegetation_MOD44B_sites)
      Percent_NonTree_Vegetation_MOD44B_sites <- crop(Percent_NonTree_Vegetation_MOD44B_sites, ext(crops[c,]))
      
      terra_trend_10yrprior_terra_change_id <- rast(paste0("tmean10yrprior_trend_", substr(time[t],1, 4), ".tif"))
      plot(terra_trend_10yrprior_terra_change_id)
      terra_trend_10yrprior_terra_change_id
      summary(d$terra_trend_10yrprior_terra_change_id)
      terra_trend_10yrprior_terra_change_id <- terra_trend_10yrprior_terra_change_id/100 ########### TÄSSÄ JOTAIN HÄMÄRÄÄÄ
      terra_trend_10yrprior_terra_change_id <- crop(terra_trend_10yrprior_terra_change_id, ext(crops[c,]))
      
      trend_20yrprior_terra_change_id <- rast(paste0("tmean20yrprior_trend_", substr(time[t],1, 4), ".tif"))
      plot(trend_20yrprior_terra_change_id)
      trend_20yrprior_terra_change_id
      summary(d$trend_20yrprior_terra_change_id)
      trend_20yrprior_terra_change_id <- trend_20yrprior_terra_change_id/100  ########### TÄSSÄ JOTAIN HÄMÄRÄÄÄ
      trend_20yrprior_terra_change_id <- crop(trend_20yrprior_terra_change_id, ext(crops[c,]))
      
      ndvi_trend_10yrprior_ndvi_change_id <- rast(paste0("ndvi10yrprior_trend_", substr(time[t],1, 4), ".tif"))
      plot(ndvi_trend_10yrprior_ndvi_change_id)
      ndvi_trend_10yrprior_ndvi_change_id
      summary(d$ndvi_trend_10yrprior_ndvi_change_id)
      ndvi_trend_10yrprior_ndvi_change_id <- ndvi_trend_10yrprior_ndvi_change_id/100 ########### TÄSSÄ JOTAIN HÄMÄRÄÄÄ
      ndvi_trend_10yrprior_ndvi_change_id <- crop(ndvi_trend_10yrprior_ndvi_change_id, ext(crops[c,]))
      
      Snow.depth_era5_soilmoist_temp_snow <- rast(paste0("snowdepth_", time[t], ".tif"))
      plot(Snow.depth_era5_soilmoist_temp_snow)
      Snow.depth_era5_soilmoist_temp_snow
      summary(d$Snow.depth_era5_soilmoist_temp_snow)
      Snow.depth_era5_soilmoist_temp_snow <- Snow.depth_era5_soilmoist_temp_snow/100
      Snow.depth_era5_soilmoist_temp_snow <- crop(Snow.depth_era5_soilmoist_temp_snow, ext(crops[c,]))
      
      pr_terraclimate_sites <- rast(paste0("ppt_", time_alt[t], ".tif"))
      plot(pr_terraclimate_sites)
      pr_terraclimate_sites
      summary(d$pr_terraclimate_sites)
      pr_terraclimate_sites <- pr_terraclimate_sites/100
      pr_terraclimate_sites <- crop(pr_terraclimate_sites, ext(crops[c,]))
      
      pdsi_terraclimate_sites <- rast(paste0("pdsi_", time_alt[t], ".tif"))
      plot(pdsi_terraclimate_sites)
      pdsi_terraclimate_sites
      summary(d$pdsi_terraclimate_sites)
      #pdsi_terraclimate_sites <- pdsi_terraclimate_sites/100 # not needed???
      pdsi_terraclimate_sites <- crop(pdsi_terraclimate_sites, ext(crops[c,]))
      
      
      
      # stack
      
      pred_rast1 <- c(srad_terraclimate_sites, Barrow_CO2_conc_Barrow_CO2conc,
                      NDVI_whittaker_constant_monthly_mean, Soil.temperature.level.1_era5_soilmoist_temp_snow,
                      water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality)
      
      names(pred_rast1) <- c("srad_terraclimate_sites", "Barrow_CO2_conc_Barrow_CO2conc",
                                 "NDVI_whittaker_constant_monthly_mean", "Soil.temperature.level.1_era5_soilmoist_temp_snow",
                                 "water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality")
      
      rm(srad_terraclimate_sites)
      rm(Barrow_CO2_conc_Barrow_CO2conc)
      rm(NDVI_whittaker_constant_monthly_mean)
      rm(Soil.temperature.level.1_era5_soilmoist_temp_snow)
      rm(water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality)
      
      gc()
      
      
      pred_rast2 <- c(water_ground_MCD43A4_annual_water_ground_sites_low_quality,
                      LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality, vpd_terraclimate_sites, 
                      Snow.cover_era5_soilmoist_temp_snow, terra_trend_10yrprior_terra_change_id)
      
      names(pred_rast2) <- c("water_ground_MCD43A4_annual_water_ground_sites_low_quality",
                             "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", "vpd_terraclimate_sites",
                             "Snow.cover_era5_soilmoist_temp_snow", "terra_trend_10yrprior_terra_change_id")
      
      rm(water_ground_MCD43A4_annual_water_ground_sites_low_quality)
      rm(LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality)
      rm(vpd_terraclimate_sites)
      rm(Snow.cover_era5_soilmoist_temp_snow)
      rm(terra_trend_10yrprior_terra_change_id)
      
      gc()
      
      pred_rast3 <- c(trend_20yrprior_terra_change_id, Snow.depth_era5_soilmoist_temp_snow, 
                      pr_terraclimate_sites, pdsi_terraclimate_sites)
      
      names(pred_rast3) <- c("trend_20yrprior_terra_change_id",
                             "Snow.depth_era5_soilmoist_temp_snow", "pr_terraclimate_sites",
                             "pdsi_terraclimate_sites")
      
      rm(trend_20yrprior_terra_change_id)
      rm(Snow.depth_era5_soilmoist_temp_snow)
      rm(pr_terraclimate_sites)
      rm(pdsi_terraclimate_sites)
      
      
      gc()
      
      
      
      pred_rast4 <- c(ndvi_trend_10yrprior_ndvi_change_id, 
                      Percent_Tree_Cover_MOD44B_sites, Percent_NonTree_Vegetation_MOD44B_sites, Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
      print(pred_rast4)
      print(length(c("ndvi_trend_10yrprior_ndvi_change_id",
                   "Percent_Tree_Cover_MOD44B_sites", "Percent_NonTree_Vegetation_MOD44B_sites", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow")))
      
      names(pred_rast4) <- c("ndvi_trend_10yrprior_ndvi_change_id",
                             "Percent_Tree_Cover_MOD44B_sites", "Percent_NonTree_Vegetation_MOD44B_sites", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow")
      
      rm(ndvi_trend_10yrprior_ndvi_change_id)
      rm(Percent_Tree_Cover_MOD44B_sites)
      rm(Percent_NonTree_Vegetation_MOD44B_sites)
      rm(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow)
      
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
      
      pred_rast <- c(pred_rast_static, pred_rast_dynamic)
      rm(pred_rast_static)
      rm(pred_rast_dynamic)

      gc()
      
      print("dynamic vars loaded")
      
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
            
            # note that prediction with terra should work: https://stackoverflow.com/questions/68082827/problem-with-type-prob-argument-in-carettrain-package
           
            library(terra) # https://stackoverflow.com/questions/38623624/usemethodpredict-no-applicable-method-for-predict-applied-to-an-object-o
            pred <- predict(pred_rast, mod, na.rm=TRUE,  
                            filename=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv", c, sep="_"), ".tif"), overwrite=TRUE) # cores=7, cpkgs=c("ranger", "caret"),
            
            print("terra pred done")
            # testing raster
            library("raster")
            #pred_rast_test <- brick(pred_rast)
            pred <- raster::predict(brick(pred_rast), mod, na.rm=TRUE,  
                                    filename=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv_raster", c, sep="_"), ".tif"), overwrite=TRUE,
                                    progress='window', fun = function(model, ...) predict(model, ...)$predictions) # cores=7, cpkgs=c("ranger", "caret"), # see this: https://github.com/imbs-hl/ranger/issues/319
            print("raster pred done")
            
            r <- terra::rast(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv", c, sep="_"), ".tif"))*100
            
            writeRaster(r, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km/", paste(i,  km, m, "loocv", c, sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
            
            print("1 km prediction done for ", i, km, m, c)
            
            rm(pred); rm(r)
            gc()
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
        
        

        

  





