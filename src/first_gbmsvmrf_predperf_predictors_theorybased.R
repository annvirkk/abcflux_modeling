


# Packages
library("caret")
library("vip")
library("pdp")
library("ggplot2")
library("viridis")

### Data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")


### Calculate the factor variables 
### Factor conversions
# note that randomForest and svmRadial can handle variables coded as "as.factor"
# but xgboost cannot: https://github.com/dmlc/xgboost/issues/95
# so we will need to do one-hot encoding

# Thermokarst
d <- as.data.frame(d)
d$Thermokarst <- factor(d$Thermokarst)
onehot <- model.matrix(~0+d[, 'Thermokarst'])
attr(onehot, "dimnames")[[2]] <- paste("Thermokarst", levels(d$Thermokarst), sep="_")
d <- cbind(d, onehot)

# land cover
# first change NA in ESA CCI to something else
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- ifelse(is.na(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged), "0000", d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged <- factor(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged)
onehot <- model.matrix(~0+d[, 'ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged'])
attr(onehot, "dimnames")[[2]] <- paste("ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged", levels(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged), sep="_")
d <- cbind(d, onehot)

# Fire burn classes
d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- factor(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
onehot <- model.matrix(~0+d[, 'Number_of_days_since_fire_classes_MCD64A1_sites_cleaned'])
attr(onehot, "dimnames")[[2]] <- paste("Number_of_days_since_fire_classes_MCD64A1_sites_cleaned", levels(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned), sep="_")
d <- cbind(d, onehot)

d$Number_of_days_since_fire_classes_gfed_monthly_calc <- factor(d$Number_of_days_since_fire_classes_gfed_monthly_calc)
onehot <- model.matrix(~0+d[, 'Number_of_days_since_fire_classes_gfed_monthly_calc'])
attr(onehot, "dimnames")[[2]] <- paste("Number_of_days_since_fire_classes_gfed_monthly_calc", levels(d$Number_of_days_since_fire_classes_gfed_monthly_calc), sep="_")
d <- cbind(d, onehot)


### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 


### Predictors
names(d)

## List predictors for the models (including the one-hot encoded factors)
# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites",  "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites", 
                       # "vpd_terraclimate_sites", # dropped this because of multicollinearities
                       
                       "trend_20yrprior_terra_change_id", # temperature change trend - not correct yet!
                       
                       "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                       
                       # "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", # dropped this now because high correlation with srad and tmean but this seemed to be quite important
                       
                       "NDVI_whittaker_constant_monthly_mean",  # Optical RS
                       
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_120", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_160", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_21", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_30", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_31", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_33", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_41", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_60", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_70", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_80",
                       
                       "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                       
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
                       
                       "PHIHOX_M_250m_ll_30_agg_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                       "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent_30_agg", # check soilgrids water and water balance and water table depth
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH",  # Permafrost
                       
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_0",   # Disturbance
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_1",
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_2",
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_2",
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_3",
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_4",
                       
                       
                       "Thermokarst_0", # Disturbance 
                       "Thermokarst_10",
                       "Thermokarst_11",
                       "Thermokarst_12",
                       "Thermokarst_5",
                       "Thermokarst_6",
                       "Thermokarst_7",
                       "Thermokarst_8",
                       "Thermokarst_9"
                       
                       
)

# check that the columns exist
Baseline_vars_1km %in% colnames(d)
Baseline_vars_1km



# Variables used in 20 km spatial resolution models
Baseline_vars_20km <- c("srad_terraclimate_sites",  "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites", 
                        #"vpd_terraclimate_sites",
                        
                        "trend_20yrprior_terra_change_id", 
                        
                        "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                        
                        "ndvi3g_lower_mean_GIMMS3g_NDVI_sites_high_and_low_quality", # more NA - skip "lai3g_mean_LAI3g_sites_high_and_low_quality", "fpar3g_mean_FPAR3g_sites_high_and_low_quality",# Optical RS
                        
                        "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
                        
                        "SMMR_SSMIS_thaw_days_NTSG_FT_SMMR_SSMIS_25km", "SMMR_SSMIS_transitional_days_NTSG_FT_SMMR_SSMIS_25km",#microwave 
                        
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_120", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_160", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_21", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_30", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_31", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_33", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_41", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_60", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_70", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged_80", 
                        
                        "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                        
                        "BLDFIE_M_250m_ll_30_agg_SoilGrids", "PHIHOX_M_250m_ll_30_agg_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", "wtd_Water_table_depth", 
                        "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent_30_agg", # check soilgrids water and water balance and water table depth
                        
                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", # "PFR_ESA_CCI_Permafrostv2", - no data pre 1997 #"ALT_ESA_CCI_Permafrostv2", # Permafrost
                        
                        
                        "Number_of_days_since_fire_classes_gfed_monthly_calc_0",
                        "Number_of_days_since_fire_classes_gfed_monthly_calc_1",
                        "Number_of_days_since_fire_classes_gfed_monthly_calc_2",
                        "Number_of_days_since_fire_classes_gfed_monthly_calc_3",
                        "Number_of_days_since_fire_classes_gfed_monthly_calc_4", # Disturbance
                        
                        
                        "Thermokarst_0", # Disturbance 
                        "Thermokarst_10",
                        "Thermokarst_11",
                        "Thermokarst_12",
                        "Thermokarst_5",
                        "Thermokarst_6",
                        "Thermokarst_7",
                        "Thermokarst_8",
                        "Thermokarst_9"
                        
)

# check that the columns exist
Baseline_vars_20km %in% colnames(d)





### Response variables
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 

### Models
models <- c("gbm", "rf", "svm")

### Kilometers
kms <- c("1km", "20km")


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
    
    # m <- "rf"

    # Load model files
    mod <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i, "predictors_theory", km, m, sep="_"), ".rds"))
    
    # #No need to explore these any further for now
    # mod <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i, "predictors_theory_february", km, m, sep="_"), ".rds"))
    # 
    # mod <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i, "predictors_theory_gs", km, m, sep="_"), ".rds"))
    
    
    # # Print the best variables
    # print("Best variables are:")
    # mod$bestSubset
    # mod$optVariables
    # Selected_vars_1km <- mod$optVariables

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
    # preds <- mod$pred %>%
    #   filter(Variables==mod$bestSubset)
    # # used this at first: plot(mod$fit$predicted, mod$fit$y) # but it is wrong
    # 
    preds <- mod$pred %>%
      filter(mtry == mod$bestTune$mtry & splitrule == mod$bestTune$splitrule & min.node.size == mod$bestTune$min.node.size) 
    

    # Merge
    if (km=="1km") {
      
      obspred <- merge(modeldata11, preds, by.x="samplerow", by.y="rowIndex")
      #plot(obspred$NEE_gC_m2, obspred$obs)
      
    } else {
      
      obspred <- merge(modeldata22, preds, by.x="samplerow", by.y="rowIndex")
      #plot(obspred$NEE_gC_m2, obspred$obs)
      
    }

    
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
    dev.copy(png, paste(i, m, "predperf_biome.png", sep="_"), width=500, height=400)
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
    dev.copy(png, paste(i, m, "predperf_vegtype.png", sep="_"), width=500, height=400)
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
    dev.copy(png, paste(i, m, "predperf_disturbance.png", sep="_"), width=500, height=400)
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
    dev.copy(png, paste(i, m, "predperf_fluxmethod.png", sep="_"), width=500, height=400)
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
    dev.copy(png, paste(i, m, "predperf_fluxmethoddetail.png", sep="_"), width=500, height=400)
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
    dev.copy(png, paste(i, m, "predperf_measfreq.png", sep="_"), width=500, height=400)
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
    dev.copy(png, paste(i, m, "predperf_outliers.png", sep="_"), width=500, height=400)
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
    dev.copy(png, paste(i, m, "predperf_months.png", sep="_"), width=500, height=400)
    dev.off()
    
  
    
    
    ### Variable importance calculation
    varImp <- vip(mod, method="permute", train=obspred[, Baseline_vars_1km], target = obspred$obs, metric = "rmse",
                      pred_wrapper = predict, nsim=100) # only 10 most important ones selected - ask package developer if we need more
    
    
    

  } # model loop done
  
  ### Variable importance plots
  # Extract data and combine into one data frame
  all_varImp <- rbind(all_varImp, cbind(varImp$data, "Model"=c(m)))

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
  
  dev.copy(png, paste0( i, "_vip.png"), width=650, height=400)
  dev.off()
  
  
  
  } 
  
}






### Partial dependence plots need to be done in a separate loop

library("viridis")


for (i in resp_vars) {
  
  for (km in kms) {
    
    gbmFit <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i, "predictors_theory", km, "gbm", sep="_"), ".rds"))
    rfFit <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i, "predictors_theory", km, "rf", sep="_"), ".rds"))
    svmFit <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i, "predictors_theory", km, "svm", sep="_"), ".rds"))
    
  }
}
# Partial dependence plots

# First baseline var
pd1 <- partial(gbmFit, pred.var = Baseline_vars_1km[1], train=d)  # don't set plot = TRUE
pd2 <- partial(rfFit, pred.var = Baseline_vars_1km[1], train=d)
pd3 <- partial(svmFit, pred.var = Baseline_vars_1km[1], train=d)


# ggplot2
pd1$Model <- "GBM"  # add new column
pd2$Model <- "RF"
pd3$Model <- "SVM"

pd.all1 <- rbind(pd1, pd2, pd3)  # bind rows

pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
  geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars_1km[1]) + 
  scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
  theme(legend.position = "none") # + ggtitle(title)

