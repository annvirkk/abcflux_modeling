


########### ANNA MUSITA TÄMÄ https://stackoverflow.com/questions/25121725/error-in-predicting-raster-with-randomforest-caret-and-factor-variables

# Packages
# install.packages("caret", lib="/mnt/data1/boreal/avirkkala/packages")
# install.packages("vip", lib="/mnt/data1/boreal/avirkkala/packages")
# install.packages("pdp", lib="/mnt/data1/boreal/avirkkala/packages")
#install.packages("Metrics", lib="/mnt/data1/boreal/avirkkala/packages")

library("caret")
library("vip")
library("pdp")
library(Metrics)
library("ggplot2")
library("viridis")
library("tidyr")
library("dplyr")
library("ggridges")

### Data
d <- read.csv("/home/master/cloud/flux_upscaling_data/results/final/modeldata_avg.csv")


# remove larval outbreak
d <- subset(d, Study_ID_Short!="Lund_Kobbefjord_Ch")

# edit vegetation type for visualization
d$Veg_type_Short <- ifelse(d$Veg_type_Short=="G,S", "G", d$Veg_type_Short)
d$Veg_type_Short <- ifelse(d$Veg_type_Short=="S,G", "S", d$Veg_type_Short)
d$Veg_type_Short <- ifelse(d$Veg_type_Short=="S,W", "S", d$Veg_type_Short)
d$Veg_type_Short <- ifelse(d$Veg_type_Short=="W,G", "W", d$Veg_type_Short)
d$Veg_type_Short <- ifelse(d$Veg_type_Short=="G,W", "G", d$Veg_type_Short)
d$Veg_type_Short <- ifelse(d$Veg_type_Short=="BW,EN", "BW", d$Veg_type_Short)
d$Veg_type_Short <- ifelse(d$Veg_type_Short=="BW,SB", "BW", d$Veg_type_Short)
d$Veg_type_Short <- ifelse(d$Veg_type_Short=="EN,BW", "EN", d$Veg_type_Short)
d$Veg_type_Short <- ifelse(d$Veg_type_Short=="W,S", "W", d$Veg_type_Short)


# SOME EDITS THAT WERE MADE
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- ifelse(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged==1, 31, d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
d$TKWP_Thermokarst <- ifelse(d$TKWP_Thermokarst==4, 3, d$TKWP_Thermokarst)


### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 


### Predictors
#names(d)

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
                       
                       "TKWP_Thermokarst", "TKHP_Thermokarst", # themokarst vulnerability 
                       
                       "forest_age_class_forest_age_sites" # forest age
                       
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km




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



# variables as factors
d$TKWP_Thermokarst <- as.factor(d$TKWP_Thermokarst)
d$TKHP_Thermokarst <- as.factor(d$TKHP_Thermokarst)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- as.factor(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
d$forest_age_class_forest_age_sites <- as.factor(d$forest_age_class_forest_age_sites)
d$Study_ID_Short <- as.factor(d$Study_ID_Short)





### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 

### Models
models <- c("qrf")


### Kilometers
kms <- c("1km", "20km")

### Number of variables
#nvars <- seq(1, 40) # this might be less for some!!!


### Set folder for results
setwd("/home/master/abcflux_modeling/results/figures/")



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
  #i <- "GPP_gC_m2"
  #i <- "Reco_gC_m2"
  
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
    # km <- "20km"
    # i <- "NEE_gC_m2"
    # i <- "GPP_gC_m2"
    # i <- "Reco_gC_m2" 
    
    
    
    # Loop through the models 
    
    for (m in models) {
      
      # m <- "qrf"

      
      # Load model files
      mod <- readRDS(paste0("/home/master/abcflux_modeling/results/", paste(i,  km, m, "train_loocv_full_model_without_larvaloutbreak", sep="_"), ".rds")) 
      mod # model says CV used by it is based on leave-one-site out folds that I created
      

      ### Predictive performance plots ###
      
      
      # Define x and y lab titles for the plot
      if (i=="GPP_gC_m2" & km=="1km") {
        ylab = expression(paste("Observed GPP g C m"^{-2}, month^{-1}))
        xlab = expression(paste("Predicted GPP g C m"^{-2}, month^{-1}, " (1 km model)"))
        
      }
      
      if (i=="NEE_gC_m2" & km=="1km") {
        ylab = expression(paste("Observed NEE g C m"^{-2}, month^{-1}))
        xlab = expression(paste("Predicted NEE g C m"^{-2}, month^{-1}, " (1 km model)"))
        
      }
      
      if (i=="Reco_gC_m2" & km=="1km") {
        ylab = expression(paste("Observed Reco g C m"^{-2}, month^{-1}))
        xlab = expression(paste("Predicted Reco g C m"^{-2}, month^{-1}, " (1 km model)"))
        
      }
      
      
      if (i=="GPP_gC_m2" & km=="20km") {
        ylab = expression(paste("Observed GPP g C m"^{-2}, month^{-1}))
        xlab = expression(paste("Predicted GPP g C m"^{-2}, month^{-1}, " (8 km model)"))
        
      }
      
      if (i=="NEE_gC_m2" & km=="20km") {
        ylab = expression(paste("Observed NEE g C m"^{-2}, month^{-1}))
        xlab = expression(paste("Predicted NEE g C m"^{-2}, month^{-1}, " (8 km model)"))
        
      }
      
      if (i=="Reco_gC_m2" & km=="20km") {
        ylab = expression(paste("Observed Reco g C m"^{-2}, month^{-1}))
        xlab = expression(paste("Predicted Reco g C m"^{-2}, month^{-1}, " (8 km model)"))
        
      }
      
      
      
      # # Extract the final model details
      preds <- mod$pred %>%
        data.frame()
      
     
      
      # Merge
      if (km=="1km") {
        
        obspred <- merge(modeldata11, preds, by.x="samplerow", by.y="rowIndex")
        #plot(obspred$NEE_gC_m2, obspred$obs)


        
      } else {
        
        obspred <- merge(modeldata22, preds, by.x="samplerow", by.y="rowIndex")
        #plot(obspred$NEE_gC_m2, obspred$obs)
        
        
      }
      
      
      ggplot(obspred) + geom_point(aes(x=Interval, y=obs))  + geom_point(aes(x=Interval, y=pred), col="red") + facet_wrap(~Biome) + theme_pub

      ggplot(obspred) + geom_point(aes(x=Interval, y=obs, col=Disturbance))  + facet_wrap(~Biome) + theme_pub
      
      # First plot scatterplots for each variable based on individual models
      # Max and min of several columns 
      scale_max <- max(c(obspred$obs, obspred$pred))
      scale_min <- min(c(obspred$obs, obspred$pred))

      
      # Merge pred perf
      # not sure how to access the results from the best model! This is a shortcut: mod$results[which.min(mod$results[, "RMSE"]), ]
      # note that it looks like predictive performances are calculated separately for each fold (group) after which a mean is calculated. See: mean(mod$resample$Rsquared, na.rm=TRUE)
      r_stats <- data.frame(cbind(model=c(m), RMSE=c(mod$results[which.min(mod$results[, "RMSE"]), ]$RMSE),
                                  Rsquared=(mod$results[which.min(mod$results[, "RMSE"]), ]$Rsquared),
                                  MAE=c(mod$results[which.min(mod$results[, "RMSE"]), ]$MAE)))
      r_stats$RMSE <- as.character(r_stats$RMSE) %>% as.numeric()
      r_stats$Rsquared <- as.character(r_stats$Rsquared) %>% as.numeric()
      r_stats$MAE <- as.character(r_stats$MAE) %>% as.numeric()
      
      
      # how many sites?
      
      length(unique(obspred$Study_ID_Short))
      
      ### Plot XGBOOST based on categorical information that we have
      
      # Colored by biome
      p1 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Biome))) +
        geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
        
        annotate(label = sprintf("\n MAE = %.1f   \n Rsquared = %.2f   \n RMSE = %.1f   \n ", 
                                 r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                                 r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                                 r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1) +
        
        theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(xlab) + ylab(ylab)+ 
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
      
      # Print out
      setwd("/home/master/abcflux_modeling/results/figures/")
      print(p1)
      dev.copy(png, paste(i, m, km,"train_loocv_full_model_without_larvaloutbreak_predperf_biome.png", sep="_"), width=500, height=400)
      dev.off()
      
      
      # Colored by veg type
      p2 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Veg_type_Short))) +
        geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
        
        annotate(label = sprintf("\n MAE = %.1f \n Rsquared = %.2f \n RMSE = %.1f \n ", 
                                 r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                                 r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                                 r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1) +
        
        theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(xlab) + ylab(ylab)+  
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
      
      print(p2)
      dev.copy(png, paste(i, m, km,"train_loocv_predperf_vegtype.png", sep="_"), width=500, height=400)
      dev.off()
      
      p2 <- ggplot(obspred, aes(x=pred, y=obs)) +
        geom_bin2d() + geom_abline(slope = 1) + 
     
        theme_pub  + theme(legend.title=element_blank())  + xlab(xlab) + ylab(ylab)+  
        scale_fill_viridis(direction=-1,  trans = 'log', labels = scales::number_format(accuracy = 1)) + # could add trans='log' but then counts are harder to interpret?
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max) + facet_wrap(~Veg_type_Short)
      
      print(p2)
      dev.copy(png, paste(i, m, km,"train_loocv_predperf_vegtype_separately.png", sep="_"), width=500, height=400)
      dev.off()
      
      
      
      
      # Colored by disturbance
      obspred$Disturbance <- as.character(obspred$Disturbance)
      obspred$Disturbance <- ifelse(is.na(obspred$Disturbance), "No", obspred$Disturbance)
      obspred$Disturbance <- ifelse(obspred$Disturbance=="No", "NA/No", obspred$Disturbance)
      obspred$Disturbance <- factor(obspred$Disturbance, levels=c( "NA/No", "Thermokarst", "Drainage",  "Fire", "Harvest", "Larval Outbreak"))
    
      p3 <- ggplot(obspred, aes(x=pred, y=obs, colour=Disturbance)) +
        geom_point(shape = 16,  size=3, alpha=0.7) + geom_abline(slope = 1) + 
        
        annotate(label = sprintf("\n MAE = %.1f \n Rsquared = %.2f \n RMSE = %.1f \n ", 
                                 r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                                 r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                                 r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1) +
        
        theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(xlab) + ylab(ylab)+ 
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
      
      print(p3)
      dev.copy(png, paste(i, m, km,"train_loocv_full_model_without_larvaloutbreak_predperf_disturbance.png", sep="_"), width=600, height=400)
      dev.off()
      
      
      
      # Colored by flux method
      p5 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Flux_method))) +
        geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
        
        annotate(label = sprintf("\n MAE = %.1f \n Rsquared = %.2f \n RMSE = %.1f \n ", 
                                 r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                                 r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                                 r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1) +
        
        theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(xlab) + ylab(ylab)+  
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
      
      print(p5)
      dev.copy(png, paste(i, m, km,"train_loocv_full_model_without_larvaloutbreak_predperf_fluxmethod.png", sep="_"), width=500, height=400)
      dev.off()
      
      
      # Colored by flux method detail
      p6 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Flux_method_detail))) +
        geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
        
        annotate(label = sprintf("\n MAE = %.1f \n Rsquared = %.2f \n RMSE = %.1f \n ", 
                                 r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                                 r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                                 r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1) +
        
        theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(xlab) + ylab(ylab)+ 
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
      
      
      print(p6)
      dev.copy(png, paste(i, m, km,"train_loocv_full_model_without_larvaloutbreak_predperf_fluxmethoddetail.png", sep="_"), width=500, height=400)
      dev.off()
      
      
      
      # Colored by Measurement frequency
      p7 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Measurement_frequency))) +
        geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
        
        annotate(label = sprintf("\n MAE = %.1f \n Rsquared = %.2f \n RMSE = %.1f \n ", 
                                 r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                                 r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                                 r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1) +
        
        theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(xlab) + ylab(ylab)+ 
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
      
      print(p7)
      dev.copy(png, paste(i, m, km,"train_loocv_full_model_without_larvaloutbreak_predperf_measfreq.png", sep="_"), width=500, height=400)
      dev.off()
      
      
      
      # Color only the sites with outlier observations
      obspred$Study_ID_figure <- ifelse(obspred$obs-obspred$pred>100 | obspred$pred-obspred$obs>100, as.character(obspred$Study_ID_Short), "ok")
      obspred$Study_ID_figure <- ifelse(obspred$Study_ID_figure=="L\xf3pez-Blanco_GL-ZaF_tower1", "Lopez-Blanco_GL-ZaF_tower1", obspred$Study_ID_figure)
      p8 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Study_ID_figure))) +
        geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
        
        annotate(label = sprintf("\n MAE = %.1f \n Rsquared = %.2f \n RMSE = %.1f \n ", 
                                 r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                                 r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                                 r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1) +
        
        theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(xlab) + ylab(ylab)+ 
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
      
      print(p8)
      dev.copy(png, paste(i, m, km,"train_loocv_full_model_without_larvaloutbreak_predperf_outliers.png", sep="_"), width=500, height=400)
      dev.off()
      
      
      
      # Color only the sites with months - asked by Brendan
      p9 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Interval))) +
        geom_point(shape = 16,  size=3, alpha=0.5) + geom_abline(slope = 1) + 
        
        annotate(label = sprintf("\n MAE = %.1f \n Rsquared = %.2f \n RMSE = %.1f \n ", 
                                 r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                                 r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                                 r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1) +
        
        theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(xlab) + ylab(ylab)+  
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
      
      print(p9)
      dev.copy(png, paste(i, m,km, "train_loocv_full_model_without_larvaloutbreak_predperf_months.png", sep="_"), width=500, height=400)
      dev.off()
      
      
      # separate months
      for (month in c(seq(1, 12, by=1))) {
        obspredmonth <- subset(obspred, Interval==month)
        difference <- obspredmonth$obs - obspredmonth$pred
        sd_comp <- sd(obspredmonth$obs)+ sd(obspredmonth$obs)
        obspredmonth$Study_ID_figure <- ifelse(abs(difference)>sd_comp, obspred$Study_ID, "ok")
        obspredmonth$Study_ID_figure <- ifelse(obspredmonth$Study_ID_figure=="L\xf3pez-Blanco_GL-ZaF_tower1", "Lopez-Blanco_GL-ZaF_tower1", obspredmonth$Study_ID_figure)
        
        p9 <- ggplot(obspredmonth, aes(x=pred, y=obs, colour=factor(Study_ID_figure))) +
          geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
          theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(xlab) + ylab(ylab)+  
          xlim(scale_min, scale_max) + ylim(scale_min, scale_max) + ggtitle(paste0("Month ", month))
          

        
        print(p9)
        dev.copy(png, paste(i, m,km, "train_loocv_full_model_without_larvaloutbreak_predperf_months_separately_outliers", month, ".png", sep="_"), width=650, height=400)
        dev.off()



      }

      
      
      obspred2 <- obspred %>% group_by(Interval) %>% summarize(obs=mean(obs, na.rm=TRUE), pred=mean(pred, na.rm=TRUE))
      p9 <- ggplot(obspred2, aes(x=pred, y=obs, colour=factor(Interval))) +
        geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
        
        theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(xlab) + ylab(ylab)

      
      print(p9)
      dev.copy(png, paste(i, m, km, "train_loocv_full_model_without_larvaloutbreak_predperf_months_mean.png", sep="_"), width=900, height=750)
      dev.off()
      
      
      
      # Residual plot
      p10 <- ggplot(obspred, aes(x=obs, y=obs-pred)) +
        geom_point(shape = 16,  size=3) + geom_abline(slope = 0) + 
        
        annotate(label = sprintf("\n MAE = %.1f \n Rsquared = %.2f \n RMSE = %.1f \n ", 
                                 r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                                 r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                                 r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1) +
        
        theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=FALSE) + xlab("obs") + 
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max) 
      
      print(p10)
      dev.copy(png, paste(i, m, km, "train_loocv_full_model_without_larvaloutbreak_predperf_residuals.png", sep="_"), width=900, height=750)
      dev.off()
      
      
      

 
      
      
      
      ### Visualize with bins instead
      # p11 <- ggplot(obspred) + geom_bin2d(aes(x=pred, y=obs),bins=20) + geom_abline(slope = 1) + 
      #   xlim(scale_min, scale_max) + ylim(scale_min, scale_max)  + theme_pub + 
      #   scale_fill_continuous(type = "viridis",  trans = 'reverse') + # could add trans='log' but then counts are harder to interpret?
      #   annotate(label = sprintf("\n MAE = %.1f \n Rsquared = %.2f \n RMSE = %.1f \n ", 
      #                            r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
      #                            r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
      #                            r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1) 
      # print(p11)
      
      
      p11 <- ggplot(obspred) + geom_bin2d(aes(x=pred, y=obs),bins=50) + geom_abline(slope = 1) + 
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max)  + theme_pub + 
        scale_fill_viridis(direction=-1,  trans = 'log', labels = scales::number_format(accuracy = 1)) + # could add trans='log' but then counts are harder to interpret?
        annotate(label = sprintf("\n MAE = %.1f \n Rsquared = %.2f \n RMSE = %.1f \n ", 
                                 r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                                 r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                                 r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 8, hjust = 0, vjust = 1) +
        labs(fill="Log counts") + xlab(xlab) + ylab(ylab)
        
      print(p11)
      dev.copy(png, paste(i, m, km, "train_loocv_full_model_without_larvaloutbreak_predperf_bins.png", sep="_"), width=500, height=400)
      dev.off()
      
      
      p12 <- ggplot(obspred) + geom_bin2d(aes(x=pred, y=obs),bins=50) + geom_abline(slope = 1) + 
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max)  + theme_pub + 
        scale_fill_viridis(direction=-1,  trans = 'log', labels = scales::number_format(accuracy = 1)) + # could add trans='log' but then counts are harder to interpret?
        labs(fill="Log counts") + facet_wrap(~Country) + xlab(xlab) + ylab(ylab)
      
      print(p12)
      dev.copy(png, paste(i, m, km, "train_loocv_full_model_without_larvaloutbreak_predperf_bins_country.png", sep="_"), width=900, height=750)
      dev.off()
      
      
      obspred$Disturbance2 <- as.character(obspred$Disturbance)
      obspred$Disturbance2 <- ifelse(obspred$Disturbance2=="Drainage", "NA/No", obspred$Disturbance2)
      p13 <- ggplot(obspred) + geom_bin2d(aes(x=pred, y=obs),bins=50) + geom_abline(slope = 1) + 
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max)  + theme_pub + 
        scale_fill_viridis(direction=-1,  trans = 'log', labels = scales::number_format(accuracy = 1)) + # could add trans='log' but then counts are harder to interpret?
        labs(fill="Log counts") + facet_wrap(~Disturbance2) + xlab(xlab) + ylab(ylab)
      
      print(p13)
      dev.copy(png, paste(i, m, km, "train_loocv_full_model_without_larvaloutbreak_predperf_bins_disturbance.png", sep="_"), width=900, height=750)
      dev.off()
      



      
      
      ### Model fit 
      predtest <- predict(mod, obspred)
      obspred$predtest <- predtest
      p11 <- ggplot(obspred) + geom_bin2d(aes(x=predtest, y=obs), bins=30) + geom_abline(slope = 1) + 
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max) + facet_wrap(~Country)
      
      print(p11)
      dev.copy(png, paste(i, m, km, "train_loocv_full_model_without_larvaloutbreak_modelfit_bins_country.png", sep="_"), width=900, height=750)
      dev.off()
      
      
      
      # if grouped to annual mean...
      obspredtest <- obspred %>% group_by(Study_ID_Short, Country, Biome, Meas_year) %>% summarize(obs_sum=sum(obs), predtest_sum=sum(predtest), n=n()) %>% filter(n==12)
      ggplot(obspredtest) + geom_bin2d(aes(x=predtest_sum, y=obs_sum), bins=30) + geom_abline(slope = 1) + 
        xlim(scale_min, scale_max) + ylim(scale_min, scale_max) + facet_wrap(~paste(Country, Biome))

      
      # Sweden and USA have quite many source observations that are predicted as larger Co2 sources
      obspredtest_long <- pivot_longer(obspredtest, cols=c(5,6))
      ggplot(obspredtest_long) + geom_density(aes(value, fill=name, color=name), alpha=0.5) + facet_wrap(~Country) + facet_wrap(~paste(Country, Biome)) + geom_vline(xintercept=0)
      
      
      
      
      
      ### Visualize with boxplots
      str(obspred)
      obspred_long <- pivot_longer(obspred, c("obs", "pred", "predtest"))
      obspred_long$name <- ifelse(obspred_long$name=="obs", "Observed", obspred_long$name)
      obspred_long$name <- ifelse(obspred_long$name=="pred", "Predicted (CV)", obspred_long$name)
      obspred_long$name <- ifelse(obspred_long$name=="predtest", "Predicted (no CV)", obspred_long$name)
      obspred_long$Biome_name <- paste(obspred_long$Biome, obspred_long$name)
      obspred_long$Biome_name <- factor(obspred_long$Biome_name, levels=c("Boreal Observed", "Boreal Predicted (no CV)", "Boreal Predicted (CV)",
                                          "Tundra Observed", "Tundra Predicted (no CV)", "Tundra Predicted (CV)"))
      
      
      
      # Define x and y lab titles for the plot
      if (i=="GPP_gC_m2") {
        ylab2 = expression(paste("GPP g C m"^{-2}, month^{-1}))

      }
      
      if (i=="NEE_gC_m2") {
        ylab2 = expression(paste("NEE g C m"^{-2}, month^{-1}))

      }
      
      if (i=="Reco_gC_m2") {
        ylab2 = expression(paste("Reco g C m"^{-2}, month^{-1}))

      }
      
      p14 <- ggplot(obspred_long) + geom_boxplot(aes(x=factor(Interval), y=value), col="black") + facet_wrap(~paste(Biome_name))  + theme_pub + ylab(ylab2) + xlab("Month")
      
      print(p14)
      dev.copy(png, paste(i, m, km, "train_loocv_predperf_boxplot.png", sep="_"), width=900, height=750)
      dev.off()
      
      
      
      obspred$predtest <- predict(mod, obspred)
      remove_sites <- obspred %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n()) 
      remove_sites <- remove_sites %>% group_by(Study_ID_Short) %>% summarize(nmax=max(n)) %>% filter(nmax<8) 
      obspred2 <- subset(obspred, !( obspred$Study_ID_Short %in% unique(remove_sites$Study_ID_Short)))
      p1 <- ggplot(obspred2) + geom_line(aes(x=as.Date(Start_date), y=obs), col="black") +
        
        geom_line(aes(x=as.Date(Start_date), y=pred), col="red") +
        
        geom_line(aes(x=as.Date(Start_date), y=predtest), col="blue") + geom_point(aes(x=as.Date(Start_date), y=obs), col="black", size=0.3) +
        
        facet_wrap(~Study_ID_Short, scales="free")
      
      print(p1)
      dev.copy(png, paste(i, m, km,"predperf_timeseries_sites.png", sep="_"), width=1400, height=1000)
      dev.off() 
      
      
      
      ### IF FIGURES IN EPS FORMAT NEEDED, RUN THESE
      # ### Final graphs for the paper: pred perf, color plots with biome, disturbance, and season across all models and model ensemble
      # # Colored by biome
      # p1 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Biome))) +
      #   geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      #   
      #   annotate(label = sprintf("\n MAE = %.1f   \n Rsquared = %.2f   \n RMSE = %.1f   \n ", 
      #                            r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
      #                            r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
      #                            r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 7, hjust = 0, vjust = 1) +
      #   
      #   theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m) + 
      #   xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
      # 
      # # Print out
      # setwd("/home/master/abcflux_modeling/results/figures/")
      # ggsave(paste(i, m, km,"train_loocv_predperf_biome.eps", sep="_"), device=cairo_ps, p1, width=16, height=11, units=c("cm"))
      # 
      # 
      # 
      # 
      # # Colored by disturbance
      # p1 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Disturbance))) +
      #   geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      #   
      #   annotate(label = sprintf("\n MAE = %.1f   \n Rsquared = %.2f   \n RMSE = %.1f   \n ", 
      #                            r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
      #                            r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
      #                            r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 7, hjust = 0, vjust = 1) +
      #   
      #   theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m) + 
      #   xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
      # 
      # # Print out
      # setwd("/home/master/abcflux_modeling/results/figures/")
      # ggsave(paste(i, m, km,"train_loocv_predperf_disturbance.eps", sep="_"), device=cairo_ps, p1, width=17, height=11, units=c("cm"))
      # 
      # 
      # 
      # 
      # # Colored by months
      # p1 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Interval))) +
      #   geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      #   
      #   annotate(label = sprintf("\n MAE = %.1f   \n Rsquared = %.2f   \n RMSE = %.1f   \n ", 
      #                            r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
      #                            r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
      #                            r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = -Inf, y = Inf, size = 7, hjust = 0, vjust = 1) +
      #   
      #   theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m) + 
      #   xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
      # 
      # # Print out
      # setwd("/home/master/abcflux_modeling/results/figures/")
      # ggsave(paste(i, m, km,"train_loocv_predperf_months.eps", sep="_"), device=cairo_ps, p1, width=15, height=11, units=c("cm"))

      
      
      print("final pred perf figures done")
      
      
    #   ## TEMPORARILY UNCOMMENTED
    #   ### Variable importance
    #   # #varimp <- caret::varImp(mod$finalModel, scale=TRUE)
    #   # all_varImp <- importance(mod$finalModel, scale=TRUE) %>% data.frame() ### TEMPORARY
    #   
    #   # extract varimp results - this should work
    #   all_varImp <- caret::varImp(mod$finalModel, scale=TRUE) %>% data.frame()
    #   
    #   #https://stats.stackexchange.com/questions/109270/caret-varimp-for-randomforest-model
    # 
    # 
    # # ## to use other functions of the package randomForest, convert class back
    # # class(mod$finalModel) <- "randomForest"
    # # importance(mod$finalModel, scale=TRUE) ## importance measure from the standard RF
    # # 
    # # 
    # # class(mod) <- "randomForest"
    # # importance(mod$finalModel, scale=TRUE)
    # # 
    # #   
    # # mod$finalModel$importance[,1]/mod$finalModel$importanceSD
    # # 
    # # varImp(mod$finalModel)
    # #   
    # # library("randomForest")
    # # importance(mod$finalModel, type=2, class=NULL, scale=TRUE)
    # #   
    # #   
    # # varImpPlot(mod)### Variable importance calculation
    #   
    # 
    # 
    #   
    #   
      
    } # model loop done
    
    # 
    # print("model loop done")
    # 
    # 
    # all_varImp$Variable2 <- row.names(all_varImp)
    # 
    # all_varImp$Importance <- all_varImp$Overall
    # 
    # 
    # # change the name
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="srad_terraclimate_sites", "Solar radiation", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="vpd_terraclimate_sites", "Vapor pressure deficit", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="pr_terraclimate_sites", "Precipitation", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="pdsi_terraclimate_sites", "PDSI", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="tmean_TerraClimate_averages", "Mean annual air temperature over 1961-1990", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="ppt_TerraClimate_averages", "Mean annual precipitation over 1961-1990", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="trend_20yrprior_terra_change_id", "Rolling last 20-year air temperature trend", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="terra_trend_19601990", "Annual mean air temperature trend over 1961-1990", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="ndvi_trend_19812010", "June-August mean NDVI trend over 1982-2010", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="Barrow_CO2_conc_Barrow_CO2conc", "Atmospheric CO2", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="Snow.cover_era5_soilmoist_temp_snow", "Snow cover", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="Snow.depth_era5_soilmoist_temp_snow", "Snow depth", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="Soil.temperature.level.1_era5_soilmoist_temp_snow", "Soil temperature", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", "Volumetric soil water", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="NDVI_whittaker_constant_monthly_mean", "NDVI", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", "Land surface temperature (daytime)", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="water_ground_MCD43A4_annual_water_ground_sites_low_quality", "June-August average NDWI", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality", "June-August average NDII", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "Compound topographic index", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "Topographic roughness scale", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="aboveground_biomass_carbon_2010_Above_belowground_biomass", "Aboveground biomass", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="belowground_biomass_carbon_2010_Above_belowground_biomass", "Belowground biomass", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="Percent_NonTree_Vegetation_MOD44B_sites", "Percent non-tree vegetation", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="Percent_NonVegetated_MOD44B_sites", "Percent non-vegetated", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="Percent_Tree_Cover_MOD44B_sites", "Percent tree cover", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", "Vegetation type", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="PHIHOX_M_sl1_250m_ll_SoilGrids", "Soil pH", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="BLDFIE_M_sl1_250m_ll_SoilGrids", "Soil bulk density", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", "Soil water content", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="TKWP_Thermokarst", "Wetland thermokarst", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="TKHP_Thermokarst", "Hillslope thermokarst", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="forest_age_class_forest_age_sites", "Vegetation age", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", "Permafrost probability", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="SoilGrids_SOC_SoilGrids_SOCstock", "Soil organic carbon stock", all_varImp$Variable2)
    # 
    # 
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="Percent_TreeCover_AVHRR_VCF5KYR", "Percent tree cover", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Percent non-tree vegetation", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="Percent_NonVegetated_AVHRR_VCF5KYR", "Percent non-vegetated", all_varImp$Variable2)
    # all_varImp$Variable2 <- ifelse(all_varImp$Variable2=="ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled", "NDVI", all_varImp$Variable2)
    # 
    # all_varImp <- all_varImp[order(all_varImp$Importance, decreasing=TRUE),]
    # all_varImp$Variable2 <- factor(all_varImp$Variable2, levels=unique(all_varImp$Variable2))
    # 
    # 
    # p1 <- ggplot(all_varImp) + geom_bar(aes(x=Variable2, y=Importance), stat="identity")   + 
    #          coord_flip() + scale_x_discrete(limits = rev(levels(all_varImp$Variable2)))+ 
    #          theme_pub #+ ggtitle(title2)
    # 
    # # Print out
    # setwd("/home/master/abcflux_modeling/results/figures/")
    # print(p1)
    # dev.copy(png, paste( i, km,  "qrf_full_model_without_larvaloutbreak_vip.png", sep="_"), width=1300, height=1100)
    # dev.off()
    # 
    # 
    # 
    # # EPS
    # 
    # # Print out
    # setwd("/home/master/abcflux_modeling/results/figures/")
    # ggsave(paste( i, km,  "qrf_full_model_without_larvaloutbreak_vip.eps", sep="_"), device=cairo_ps, p1, width=35, height=23, units=c("cm"))
    # 
    # 
    # write.csv(all_varImp, paste("/home/master/abcflux_modeling/results/", i, km, "qrf_full_model_without_larvaloutbreak_vip.csv", sep="_"), row.names=FALSE)
    # 
    
  } # km loop done
  
  print("km loop done")
  
  
}



# 
# ####### TEMPORARILY UNCOMMENTED
# 
# ### Partial dependence plots need to be done in a separate loop
# 
# 
# 
# for (i in resp_vars) {
# 
#   for (km in kms) {
# 
# 
#     # km <- "1km"
#     # i <- "NEE_gC_m2"
#     # i <- "GPP_gC_m2"
#     # i <- "Reco_gC_m2"
# 
# 
#     # models
#     mod <- readRDS(paste0("/home/master/abcflux_modeling/results/", paste(i,  km, "qrf_train_loocv_full_model_without_larvaloutbreak", sep="_"), ".rds"))
# 
#     # selected vars
#     if (km=="1km") {
#       
#       Selected_vars <- Baseline_vars_1km
#     } else {
#       
#       Selected_vars <- Baseline_vars_20km
#       
#     }
# 
#     nvars <- length(Selected_vars)
#     
# 
#     # model training data
#     # Add data
#     # remove NA across columns for 1 and 20 km datasets
#     modeldata2 <- d[,c("Study_ID_Short", "id", i, Baseline_vars_1km)]
#     modeldata1 <- na.omit(modeldata2) # only 86 obs
#     sapply(modeldata1, function(x) sum(is.na(x))) # no missing data
#     print("1 km data set:")
#     print(nrow(modeldata1))
# 
#     modeldata2 <- d[,c("Study_ID_Short", "id", i, Baseline_vars_20km)]
#     modeldata2 <- na.omit(modeldata2) # only 86 obs
#     sapply(modeldata2, function(x) sum(is.na(x))) # no missing data
#     print("20 km data set:")
#     print(nrow(modeldata2))
# 
#     # create a row ID
#     modeldata1$samplerow <- seq(1, length(modeldata1$Study_ID_Short), by=1)
# 
#     # create a row ID
#     modeldata2$samplerow <- seq(1, length(modeldata2$Study_ID_Short), by=1)
# 
# 
#     # # Extract the final model details: GBM
#     preds <- mod$pred %>% data.frame()
# 
#     if (km=="1km") {
# 
#       obspred <- merge(modeldata1, preds, by.x="samplerow", by.y="rowIndex")
#       #plot(obspred$NEE_gC_m2, obspred$obs)
# 
#     } else {
# 
#       obspred <- merge(modeldata2, preds, by.x="samplerow", by.y="rowIndex")
#       #plot(obspred$NEE_gC_m2, obspred$obs)
# 
#     }
# 
#     
#     library("randomForest")
# 
#     for (nvar in 1:nvars) {
#       
#       pd1 <- partial(mod, pred.var = Selected_vars[nvar], train=obspred)  # don't set plot = TRUE
#       
#       rug_data <- obspred[Selected_vars[nvar]]
#       
#       # variable names
#       # change the name
#       Selected_vars2 <- Selected_vars
#       Selected_vars2 <- ifelse(Selected_vars2=="srad_terraclimate_sites", "Solar radiation/10 W m-2	", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="vpd_terraclimate_sites", "Vapor pressure deficit/100 kPa", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="pr_terraclimate_sites", "Precipitation mm", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="pdsi_terraclimate_sites", "PDSI/100", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="tmean_TerraClimate_averages", "Mean annual air temperature over 1961-1990 ?C", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="ppt_TerraClimate_averages", "Mean annual precipitation over 1961-1990 mm", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="trend_20yrprior_terra_change_id", "Rolling last 20-year air temperature trend/10 ?C", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="terra_trend_19601990", "Annual mean air temperature trend over 1961-1990/10 ?C", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="ndvi_trend_19812010", "June-August mean NDVI trend over 1982-2010", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="Barrow_CO2_conc_Barrow_CO2conc", "Atmospheric CO2", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="Snow.cover_era5_soilmoist_temp_snow", "Snow cover %", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="Snow.depth_era5_soilmoist_temp_snow", "Snow depth m", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="Soil.temperature.level.1_era5_soilmoist_temp_snow", "Soil temperature Kelvin", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", "Volumetric soil water m3 m-3", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="NDVI_whittaker_constant_monthly_mean", "NDVI", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", "Land surface temperature (daytime) Kelvin", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="water_ground_MCD43A4_annual_water_ground_sites_low_quality", "June-August average NDWI", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality", "June-August average NDII", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "Compound topographic index", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "Topographic roughness scale", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="aboveground_biomass_carbon_2010_Above_belowground_biomass", "Aboveground biomass MgC ha-1", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="belowground_biomass_carbon_2010_Above_belowground_biomass", "Belowground biomass MgC ha-1", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="Percent_NonTree_Vegetation_MOD44B_sites", "Percent non-tree vegetation %", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="Percent_NonVegetated_MOD44B_sites", "Percent non-vegetated %", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="Percent_Tree_Cover_MOD44B_sites", "Percent tree cover %", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", "Vegetation type", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="PHIHOX_M_sl1_250m_ll_SoilGrids", "Soil pH", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="BLDFIE_M_sl1_250m_ll_SoilGrids", "Soil bulk density kg m-3", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", "Soil water content %", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="Number_of_days_since_fire_classes_MCD64A1_sites_cleaned", "Time since fire (no burn - old burn)", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="TKWP_Thermokarst", "Wetland thermokarst (low - high coverage)", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="TKHP_Thermokarst", "Hillslope thermokarst (low - high coverage)", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="forest_age_class_forest_age_sites", "Vegetation age (old - young)", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", "Permafrost probability %", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="SoilGrids_SOC_SoilGrids_SOCstock", "Soil organic carbon stock Tonnes ha-1", Selected_vars2)
#       
#       
#       Selected_vars2 <- ifelse(Selected_vars2=="Percent_TreeCover_AVHRR_VCF5KYR", "Percent tree cover %", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Percent non-tree vegetation %", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="Percent_NonVegetated_AVHRR_VCF5KYR", "Percent non-vegetated %", Selected_vars2)
#       Selected_vars2 <- ifelse(Selected_vars2=="ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled", "NDVI", Selected_vars2)
#       
#       if (!is.factor(obspred[, Selected_vars[nvar]])) {
#         
#         pdp_plot1 <- ggplot(pd1, aes(x=pd1[, 1], y=pd1[, 2])) +
#           geom_line(size=1) + theme_pub  +labs(y="yhat", x=Selected_vars2[nvar]) +
#           theme(legend.position = "none") +
#           geom_rug(data=rug_data, aes(x=rug_data[, 1]), inherit.aes = FALSE) #+ geom_rug(col=rgb(.5,0,0,alpha=.2)) # + ggtitle(title)
#       } else if  (is.factor(obspred[, Selected_vars[nvar]])) {
#         
#         pdp_plot1 <- ggplot(pd1, aes(x=pd1[, 1], y=pd1[, 2])) +
#           geom_point(size=3) + theme_pub  +labs(y="yhat", x=Selected_vars2[nvar]) +
#           theme(legend.position = "none") # + ggtitle(title)
#       }
#       
#       print(pdp_plot1)
#       
#       dev.copy(png, paste( i, km, Selected_vars[nvar],  "qrf_full_model_without_larvaloutbreak_pdp.png", sep="_"), width=600, height=400)
#       dev.off()
# 
# 
# 
# 
#     } # nvars loop
#     
#     
#   
#     
#     
# 
#   } # km loop
# 
# } # resp vars loop













# # Partial dependence plots - testing
# 
# # First baseline var
# pd1 <- partial(gbmFit, pred.var = Baseline_vars_1km[36])  # don't set plot = TRUE
# pd2 <- partial(rfFit, pred.var = Baseline_vars_1km[36])
# pd3 <- partial(svmFit, pred.var = Baseline_vars_1km[36]) # , train=d
# 
# # ggplot2
# pd1$Model <- "GBM"  # add new column
# pd2$Model <- "RF"
# pd3$Model <- "SVM"
# 
# pd.all1 <- rbind(pd1, pd2, pd3)  # bind rows
# 
# 
# if(km=="1km") {
#   
#   # First baseline var
#   pd1 <- partial(gbmFit, pred.var = Baseline_vars_1km[nvar])  # don't set plot = TRUE
#   pd2 <- partial(rfFit, pred.var = Baseline_vars_1km[nvar])
#   pd3 <- partial(svmFit, pred.var = Baseline_vars_1km[nvar]) # , train=d
#   
#   # ggplot2
#   pd1$Model <- "GBM"  # add new column
#   pd2$Model <- "RF"
#   pd3$Model <- "SVM"
#   
#   pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
#     geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[1]) + 
#     scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
#     theme(legend.position = "none") # + ggtitle(title)
# } if (km=="20km") {
#   
#   # First baseline var
#   pd1 <- partial(gbmFit, pred.var = Baseline_vars_20km[nvar])  # don't set plot = TRUE
#   pd2 <- partial(rfFit, pred.var = Baseline_vars_20km[nvar])
#   pd3 <- partial(svmFit, pred.var = Baseline_vars_20km[nvar]) # , train=d
#   
#   # ggplot2
#   pd1$Model <- "GBM"  # add new column
#   pd2$Model <- "RF"
#   pd3$Model <- "SVM"
#   
#   pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
#     geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars_20km[1]) + 
#     scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
#     theme(legend.position = "none") # + ggtitle(title)
#   
# }
# 
# 
# print(pdp_plot1)
# 
# dev.copy(png, paste0( i, km, nvar, "pdp.png", sep="_"), width=650, height=400)
# dev.off()



