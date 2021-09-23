


########### ANNA MUSITA TÄMÄ https://stackoverflow.com/questions/25121725/error-in-predicting-raster-with-randomforest-caret-and-factor-variables

# Packages
library("caret")
library("vip")
library("pdp")
library("ggplot2")
library("viridis")
library("dplyr")

### Data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")


### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 


### Predictors
names(d)

## List predictors for the models (including the one-hot encoded factors)
# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "swe_terraclimate_sites", # climate "tmean_terraclimate_sites" correlates with LST
                       
                       "tmean_TerraClimate_averages", "ppt_TerraClimate_averages",  # don't have all of these:  "swe_TerraClimate_averages", "srad_TerraClimate_averages", "vpd_TerraClimate_averages", "pdsi_TerraClimate_averages", 
                       
                       "trend_20yrprior_terra_change_id", "terra_trend_10yrprior_terra_change_id", "terra_trend_19601990", "terra_trend_19812010",# temperature change - note that the naming convention changed a bit...
                       
                       "ndvi_trend_10yrprior_ndvi_change_id",  "ndvi_trend_19812010", # ndvi change trend - not including permafrost for now
                       
                       "Barrow_CO2_conc_Barrow_CO2conc",
                       
                       "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                       
                       "NDVI_whittaker_constant_monthly_mean", # "EVI_whittaker_constant_monthly_mean", "EVI_MOD13A1v006_NDVI_EVI_sites_low_quality_merge_whittaker",  "NDVI_MOD13A1v006_NDVI_EVI_sites_low_quality_merge_whittaker", # Optical RS, dropped several because highly correlated
                       
                       "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", # "LST_Night_1km_MOD11A2v006_LST_Night_sites_low_quality", # highly correlated
                       
                       "water_ground_MCD43A4_annual_water_ground_sites_low_quality", "water_vegetation_MCD43A4_annual_water_vegetation_sites_low_quality", 
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
                       
                       "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                       
                       "Percent_NonTree_Vegetation_MOD44B_sites", "Percent_NonVegetated_MOD44B_sites", "Percent_Tree_Cover_MOD44B_sites",
                       
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", 
                       
                       "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                       "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", "wtd_Water_table_depth", 
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", # Permafrost
                       
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned",   # Disturbance
                       
                       "TKWP_Thermokarst", "TKHP_Thermokarst" # Disturbance 
                       
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km



# Variables used in 20 km spatial resolution models
Baseline_vars_20km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites", "swe_terraclimate_sites", # climate
                        
                        "tmean_TerraClimate_averages", "ppt_TerraClimate_averages",  # don't have all of these:  "swe_TerraClimate_averages", "srad_TerraClimate_averages", "vpd_TerraClimate_averages", "pdsi_TerraClimate_averages", 
                        
                        "trend_20yrprior_terra_change_id", "terra_trend_10yrprior_terra_change_id", "terra_trend_19601990", "terra_trend_19812010",# temperature change - note that the naming convention changed a bit...
                        
                        "ndvi_trend_10yrprior_ndvi_change_id",  "ndvi_trend_19812010", # ndvi change trend - not including permafrost for now
                        
                        "Barrow_CO2_conc_Barrow_CO2conc",
                        
                        "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                        
                        "ndvi3g_lower_mean_GIMMS3g_NDVI_sites_high_and_low_quality", # Optical RS
                        
                        "SMMR_SSMIS_thaw_days_NTSG_FT_SMMR_SSMIS_25km", "SMMR_SSMIS_transitional_days_NTSG_FT_SMMR_SSMIS_25km",#microwave 
                        
                        "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
                        
                        "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                        
                        "Percent_NonTree_Vegetation_AVHRR_VCF5KYR", "Percent_Tree_Cover_MOD44B_sites", "Percent_NonVegetated_AVHRR_VCF5KYR", 
                        
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", 
                        
                        "PHIHOX_M_sl1_250m_ll_SoilGrids", "BLDFIE_M_sl1_250m_ll_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                        "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent", "wtd_Water_table_depth", 
                        
                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", # Permafrost
                        
                        "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned",   # Disturbance
                        
                        "TKWP_Thermokarst", "TKHP_Thermokarst" # Disturbance 
                        
                        
)


# check that the columns exist
Baseline_vars_20km %in% colnames(d)



# variables as factors
d$TKWP_Thermokarst <- as.factor(d$TKWP_Thermokarst)
d$TKHP_Thermokarst <- as.factor(d$TKHP_Thermokarst)
d$Number_of_days_since_fire_classes_gfed_monthly_calc <- as.factor(d$Number_of_days_since_fire_classes_gfed_monthly_calc)
d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- as.factor(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- as.factor(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)



### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 

### Models
models <- c("gbm", "rf", "svm")

### Kilometers
kms <- c("1km", "20km")

### Number of variables
#nvars <- seq(1, 40) # this might be less for some!!!


### Set folder for results
setwd("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/figures")



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
  # i <- "NEE_gC_m2"
    # i <- "GPP_gC_m2"
    # i <- "Reco_gC_m2" 
  
  # empty varimp data frame
  all_varImp <- NA
  
  
  # Loop through the models 
  
  for (m in models) {
    
    # m <- "gbm"
    # m <- "rf"
    # m <- "svm"
    
    # Load model files
    mod <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i,  km, m, "loocv", sep="_"), ".rds"))
    
    
    # # Print the best variables
    # print("Best variables are:")
    # mod$bestSubset
    # mod$optVariables
    
    ggplot(data = mod, metric = "RMSE") + theme_bw()
    ggplot(data = mod, metric = "Rsquared") + theme_bw()
    
    ### Predictive performance plots ###
    
    
    # Define x and y lab titles for the plot
    if (i=="GPP_gC_m2") {
      ylab = expression(paste("Observed GPP g C m"^{-2}, month^{-1}))
      xlab = expression(paste("Predicted GPP g C m"^{-2}, month^{-1}))
      
    }
    
    if (i=="NEE_gC_m2") {
      ylab = expression(paste("Observed NEE g C m"^{-2}, month^{-1}))
      xlab = expression(paste("Predicted NEE g C m"^{-2}, month^{-1}))
      
    }
    
    if (i=="Reco_gC_m2") {
      ylab = expression(paste("Observed ER g C m"^{-2}, month^{-1}))
      xlab = expression(paste("Predicted ER g C m"^{-2}, month^{-1}))
      
    }
    


    # # Extract the final model details
    preds <- mod$pred %>%
      filter(Variables==mod$bestSubset)
    # # used this at first: plot(mod$fit$predicted, mod$fit$y) # but it is wrong
    # 
    
    # # this is for the theory-based, without feature selection
    # if (m=="gbm") {    
    #   preds <- mod$pred  %>% filter(interaction.depth == mod$bestTune$interaction.depth & n.trees == mod$bestTune$n.trees) %>% data.frame()
    # } 
    # 
    # if (m=="rf") {    
    #   preds <- mod$pred %>%
    #   filter(mtry == mod$bestTune$mtry & splitrule == mod$bestTune$splitrule & min.node.size == mod$bestTune$min.node.size) 
    # }
    # 
    # if (m=="svm") {    
    #   preds <- mod$pred %>%
    #     filter(C == mod$bestTune$C) 
    # }

    # Merge
    if (km=="1km") {
      
      obspred <- merge(modeldata11, preds, by.x="samplerow", by.y="rowIndex")
      #plot(obspred$NEE_gC_m2, obspred$obs)
      
    } else {
      
      obspred <- merge(modeldata22, preds, by.x="samplerow", by.y="rowIndex")
      #plot(obspred$NEE_gC_m2, obspred$obs)
      
    }
    
    
    ggplot(obspred) + geom_point(aes(x=Interval, y=obs))  + geom_point(aes(x=Interval, y=pred), col="red") + facet_wrap(~Biome) + theme_pub
    ggplot(obspred) + geom_boxplot(aes(x=factor(Interval), y=obs), col="blue")  + geom_boxplot(aes(x=factor(Interval), y=pred), col="red", alpha=0.5) + 
    facet_wrap(~Biome) + theme_pub
    ggplot(obspred) + geom_point(aes(x=Interval, y=obs, col=Disturbance))  + facet_wrap(~Biome) + theme_pub
    
    # First plot scatterplots for each variable based on individual models
    # Max and min of several columns 
    scale_max <- max(c(obspred$obs, obspred$pred))
    scale_min <- min(c(obspred$obs, obspred$pred))
    # I might need to check this
    # scale_max <- max(c(gbmFit$pred$obs, gbmFit$pred$pred, rfFit$pred$obs, rfFit$pred$pred, svmFit$pred$obs, svmFit$pred$pred))
    # scale_min <- min(c(gbmFit$pred$obs, gbmFit$pred$pred, rfFit$pred$obs, rfFit$pred$pred, svmFit$pred$obs, svmFit$pred$pred))

    # check whether Rsquared and correlation-derived estimate match
    #cor(r$obs, r[, m])^2 # yep, they are the same!
    #rmse(r$obs, r[, m])
    #mean(abs(r$obs -r[, m]))
    
    # Merge pred perf
    # not sure how to access the results from the best model! This is a shortcut: mod$results[which.min(mod$results[, "RMSE"]), ]
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
      
      annotate(label = sprintf("MAE = %.1f   \nRsquared = %.2f   \nRMSE = %.1f   \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m) + 
      xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
    
    # Print out
    setwd("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/figures/")
    print(p1)
    dev.copy(png, paste(i, m, km,"loocv_predperf_biome.png", sep="_"), width=500, height=400)
    dev.off()
    
    
    # Colored by veg type
    p2 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Veg_type_Short))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m) + 
      xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
    
    print(p2)
    dev.copy(png, paste(i, m, km,"loocv_predperf_vegtype.png", sep="_"), width=500, height=400)
    dev.off()
    
    
    # Colored by disturbance
    obspred$Disturbance <- as.character(obspred$Disturbance)
    obspred$Disturbance <- ifelse(is.na(obspred$Disturbance), "NA", obspred$Disturbance)
    p3 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Disturbance))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m) + 
      xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
    
    print(p3)
    dev.copy(png, paste(i, m, km,"loocv_predperf_disturbance.png", sep="_"), width=500, height=400)
    dev.off()
  
    
    
    # Colored by flux method
    p5 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Flux_method))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m) + 
      xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
    
    print(p5)
    dev.copy(png, paste(i, m, km,"loocv_predperf_fluxmethod.png", sep="_"), width=500, height=400)
    dev.off()
    
    
    # Colored by flux method detail
    p6 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Flux_method_detail))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m) + 
      xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
    
    
    print(p6)
    dev.copy(png, paste(i, m, km,"loocv_predperf_fluxmethoddetail.png", sep="_"), width=500, height=400)
    dev.off()
    
    
    
    # Colored by Measurement frequency
    p7 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Measurement_frequency))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m) + 
      xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
    
    print(p7)
    dev.copy(png, paste(i, m, km,"loocv_predperf_measfreq.png", sep="_"), width=500, height=400)
    dev.off()
    
    
    
    # Color only the sites with outlier observations
    obspred$Study_ID_figure <- ifelse(obspred$obs-obspred$pred>100 | obspred$pred-obspred$obs>100, as.character(obspred$Study_ID_Short), "ok")
    p8 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Study_ID_figure))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m) + 
      xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
    
    print(p8)
    dev.copy(png, paste(i, m, km,"loocv_predperf_outliers.png", sep="_"), width=500, height=400)
    dev.off()
    
    
    
    # Color only the sites with months - asked by Brendan
    p9 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Interval))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m) + 
      xlim(scale_min, scale_max) + ylim(scale_min, scale_max)
    
    print(p9)
    dev.copy(png, paste(i, m,km, "loocv_predperf_months.png", sep="_"), width=500, height=400)
    dev.off()
    
    
    
    p9 <- ggplot(obspred, aes(x=pred, y=obs, colour=Interval)) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=FALSE) + xlab(m) + 
      xlim(scale_min, scale_max) + ylim(scale_min, scale_max) + facet_wrap(~Interval)
    
    print(p9)
    dev.copy(png, paste(i, m, km, "loocv_predperf_months_separately.png", sep="_"), width=900, height=750)
    dev.off()
    

    
    # Residual plot
    p10 <- ggplot(obspred, aes(x=obs, y=obs-pred)) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=FALSE) + xlab("obs") + 
      xlim(scale_min, scale_max) + ylim(scale_min, scale_max) 
    
    print(p10)
    dev.copy(png, paste(i, m, km, "loocv_predperf_residuals.png", sep="_"), width=900, height=750)
    dev.off()
    
    print(paste(i, m, km, "pred perf figures done"))
    
    ### Variable importance calculation
    
    Selected_vars <- mod$optVariables
    
    ### VERY ANNOYING BUT IN PERMUTATION DATA IS SPLIT AND THEREFORE FACTORS THAT DONT HAVE MANY OBSERVATIONS MIGHT BE PROBLEMATIC
    # THIS HAPPENS WITH Number_of_days_since_fire_classes_MCD64A1_sites_cleaned now

    varImp <- vi(mod, method="permute", train=obspred[, Selected_vars], target = obspred[, i], metric = "rmse",
                   pred_wrapper = predict, nsim=100) # only 10 most important ones selected - ask package developer if we need more
    

    
    # # without feature selection:
    # if (km=="1km") {
    #   varImp <- vi(mod, method="permute", train=mod$trainingData[, Baseline_vars_1km], target = mod$trainingData$.outcome, metric = "rmse",
    #                pred_wrapper = predict, nsim=100) # only 10 most important ones selected - ask package developer if we need more
    # } else {
    #   
    #   varImp <- vi(mod, method="permute", train=mod$trainingData[, Baseline_vars_20km], target = mod$trainingData$.outcome, metric = "rmse",
    #                pred_wrapper = predict, nsim=100) # only 10 most important ones selected - ask package developer if we need more
    # }
    
    
    # Extract data and combine into one data frame
    all_varImp <- rbind(all_varImp, cbind(varImp, "Model"=c(m)))
    
    print(paste(i, m, km, "vip done"))
    

  } # model loop done
  
  print("model loop done")
  
  ### Variable importance plots

  all_varImp$Model <- as.character(all_varImp$Model)
  all_varImp$Model <- ifelse(all_varImp$Model=="gbm", "GBM", all_varImp$Model)
  all_varImp$Model <- ifelse(all_varImp$Model=="rf", "RF", all_varImp$Model)
  all_varImp$Model <- ifelse(all_varImp$Model=="svm", "SVM", all_varImp$Model)
  all_varImp$Model <- factor(all_varImp$Model)
  #all_varImp$Model <- factor(all_varImp$Model, levels=c("XGBOOST", "RF", "SVM"))
  
  all_varImp <- all_varImp[-1, ]
  
  # Print out
  print(ggplot(all_varImp) + geom_bar(aes(x=Variable, y=Importance, fill=Model), stat="identity", position="dodge")  + 
          scale_fill_manual(values = c(viridis(3)[1],viridis(3)[2], viridis(3)[3]), guide = guide_legend(reverse=TRUE))  + 
          coord_flip() +
          theme_pub) #+ ggtitle(title2)
  
  dev.copy(png, paste( i, km,  "vip.png", sep="_"), width=1300, height=1000)
  dev.off()
  
  
  write.csv(all_varImp, paste("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", i, km, "vip.csv", sep="_"), row.names=FALSE)
  
  
  } # km loop done
  
  print("km loop done")
  
  
}






### Partial dependence plots need to be done in a separate loop



for (i in resp_vars) {
  
  for (km in kms) {
    
    # models
    gbmFit <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i,  km, "gbm", sep="_"), ".rds"))
    rfFit <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i,  km, "rf", sep="_"), ".rds"))
    svmFit <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i,  km, "svm", sep="_"), ".rds"))
    
    # selected vars
    Selected_vars_gbm <- gbmFit$optVariables
    Selected_vars_rf <- rfFit$optVariables
    Selected_vars_svm <- svmFit$optVariables
    
    Selected_vars <- c(Selected_vars_gbm, Selected_vars_rf, Selected_vars_svm) %>% unique()
    nvars <- length(Selected_vars)
    
    # model training data
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
    
    
    # # Extract the final model details: GBM
    preds <- gbmFit$pred %>%
      filter(Variables==gbmFit$bestSubset)

    if (km=="1km") {
      
      obspred_gbm <- merge(modeldata1, preds, by.x="samplerow", by.y="rowIndex")
      #plot(obspred$NEE_gC_m2, obspred$obs)
      
    } else {
      
      obspred_gbm <- merge(modeldata2, preds, by.x="samplerow", by.y="rowIndex")
      #plot(obspred$NEE_gC_m2, obspred$obs)
      
    }
    
    # # Extract the final model details: RF
    preds <- rfFit$pred %>%
      filter(Variables==rfFit$bestSubset)
    
    if (km=="1km") {
      
      obspred_rf <- merge(modeldata1, preds, by.x="samplerow", by.y="rowIndex")
      #plot(obspred$NEE_gC_m2, obspred$obs)
      
    } else {
      
      obspred_rf <- merge(modeldata2, preds, by.x="samplerow", by.y="rowIndex")
      #plot(obspred$NEE_gC_m2, obspred$obs)
      
    }
    
    
    # # Extract the final model details: SVM
    preds <- svmFit$pred %>%
      filter(Variables==svmFit$bestSubset)
    
    if (km=="1km") {
      
      obspred_svm <- merge(modeldata1, preds, by.x="samplerow", by.y="rowIndex")
      #plot(obspred$NEE_gC_m2, obspred$obs)
      
    } else {
      
      obspred_svm <- merge(modeldata2, preds, by.x="samplerow", by.y="rowIndex")
      #plot(obspred$NEE_gC_m2, obspred$obs)
      
    }
    
    for (nvar in nvars) {
      
      #nvar=1
      
      
      if (Selected_vars[nvar] %in% Selected_vars_gbm & Selected_vars[nvar] %in% Selected_vars_rf & Selected_vars[nvar] %in% Selected_vars_svm) {
        
        # First baseline var 
        pd1 <- partial(gbmFit, pred.var = Selected_vars[nvar], train=obspred_gbm)  # don't set plot = TRUE
        pd2 <- partial(rfFit, pred.var = Selected_vars[nvar], train=obspred_rf)
        pd3 <- partial(svmFit, pred.var = Selected_vars[nvar], train=obspred_svm) # , train=d
        
        # ggplot2
        pd1$Model <- "GBM"  # add new column
        pd2$Model <- "RF"
        pd3$Model <- "SVM"
        
        pd.all1 <- rbind(pd1, pd2, pd3)
        
        if (!is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        } else if  (is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_point(size=3) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        }
        
        print(pdp_plot1)
        
        dev.copy(png, paste( i, km, Selected_vars[nvar],  "pdp.png", sep="_"), width=650, height=400)
        dev.off()
        
        
      } # if loop 
      
      
      if (Selected_vars[nvar] %in% Selected_vars_gbm & Selected_vars[nvar] %in% Selected_vars_rf & !(Selected_vars[nvar] %in% Selected_vars_svm)) {
        
        # First baseline var 
        pd1 <- partial(gbmFit, pred.var = Selected_vars[nvar], train=obspred_gbm)  # don't set plot = TRUE
        pd2 <- partial(rfFit, pred.var = Selected_vars[nvar], train=obspred_rf)

        # ggplot2
        pd1$Model <- "GBM"  # add new column
        pd2$Model <- "RF"

        pd.all1 <- rbind(pd1, pd2)
        
        if (!is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        } else if  (is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_point(size=3) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        }
        
        print(pdp_plot1)
        
        dev.copy(png, paste( i, km, Selected_vars[nvar],  "pdp.png", sep="_"), width=650, height=400)
        dev.off()
        
        
      } # if loop 
      
      
      if (Selected_vars[nvar] %in% Selected_vars_gbm & !(Selected_vars[nvar] %in% Selected_vars_rf) & !(Selected_vars[nvar] %in% Selected_vars_svm)) {
        
        # First baseline var 
        pd1 <- partial(gbmFit, pred.var = Selected_vars[nvar], train=obspred_gbm)  # don't set plot = TRUE

        
        # ggplot2
        pd1$Model <- "GBM"  # add new column

        
        pd.all1 <- rbind(pd1)
        
        if (!is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        } else if  (is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_point(size=3) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        }
        
        print(pdp_plot1)
        
        dev.copy(png, paste( i, km, Selected_vars[nvar],  "pdp.png", sep="_"), width=650, height=400)
        dev.off()
        
        
      } # if loop 
      
      
      
      if (Selected_vars[nvar] %in% Selected_vars_gbm & !(Selected_vars[nvar] %in% Selected_vars_rf) & Selected_vars[nvar] %in% Selected_vars_svm) {
        
        # First baseline var 
        pd1 <- partial(gbmFit, pred.var = Selected_vars[nvar], train=obspred_gbm)  # don't set plot = TRUE
        pd3 <- partial(svmFit, pred.var = Selected_vars[nvar], train=obspred_svm) # , train=d
        
        # ggplot2
        pd1$Model <- "GBM"  # add new column
        pd3$Model <- "SVM"
        
        pd.all1 <- rbind(pd1, pd3)
        
        if (!is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        } else if  (is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_point(size=3) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        }
        
        print(pdp_plot1)
        
        dev.copy(png, paste( i, km, Selected_vars[nvar],  "pdp.png", sep="_"), width=650, height=400)
        dev.off()
        
        
      } # if loop 
      
      
      
      
      if (!(Selected_vars[nvar] %in% Selected_vars_gbm) & Selected_vars[nvar] %in% Selected_vars_rf & !(Selected_vars[nvar] %in% Selected_vars_svm)) {
        
        # First baseline var 
        pd2 <- partial(rfFit, pred.var = Selected_vars[nvar], train=obspred_rf)

        # ggplot2
        pd2$Model <- "RF"

        pd.all1 <- rbind( pd2)
        
        if (!is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        } else if  (is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_point(size=3) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        }
        
        print(pdp_plot1)
        
        dev.copy(png, paste( i, km, Selected_vars[nvar],  "pdp.png", sep="_"), width=650, height=400)
        dev.off()
        
        
      } # if loop 
      
      
      
      if (!(Selected_vars[nvar] %in% Selected_vars_gbm) & !(Selected_vars[nvar] %in% Selected_vars_rf) & Selected_vars[nvar] %in% Selected_vars_svm) {
        
        # First baseline var 
        pd3 <- partial(svmFit, pred.var = Selected_vars[nvar], train=obspred_svm) # , train=d
        
        # ggplot2
        pd3$Model <- "SVM"
        
        pd.all1 <- rbind(pd3)
        
        if (!is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        } else if  (is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_point(size=3) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        }
        
        print(pdp_plot1)
        
        dev.copy(png, paste( i, km, Selected_vars[nvar],  "pdp.png", sep="_"), width=650, height=400)
        dev.off()
        
        
      } # if loop 
      
      
      if (!(Selected_vars[nvar] %in% Selected_vars_gbm) & Selected_vars[nvar] %in% Selected_vars_rf & Selected_vars[nvar] %in% Selected_vars_svm) {
        
        # First baseline var 
        pd2 <- partial(rfFit, pred.var = Selected_vars[nvar], train=obspred_rf)
        pd3 <- partial(svmFit, pred.var = Selected_vars[nvar], train=obspred_svm) # , train=d
        
        # ggplot2
        pd2$Model <- "RF"
        pd3$Model <- "SVM"
        
        pd.all1 <- rbind(pd2, pd3)
        
        if (!is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        } else if  (is.factor(obspred[, Selected_vars_gbm[nvar]])) {
          
          pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
            geom_point(size=3) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[nvar]) + 
            scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") # + ggtitle(title)
        }
        
        print(pdp_plot1)
        
        dev.copy(png, paste( i, km, Selected_vars[nvar],  "pdp.png", sep="_"), width=650, height=400)
        dev.off()
        
        
      } # if loop 
      
      

      
    } # nvars loop
    
  } # km loop
  
} # resp vars loop



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



