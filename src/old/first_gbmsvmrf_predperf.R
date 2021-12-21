


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
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged <- ifelse(is.na(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged), "0000", d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged)
d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged <- factor(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged)
onehot <- model.matrix(~0+d[, 'ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged'])
attr(onehot, "dimnames")[[2]] <- paste("ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged", levels(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged), sep="_")
d <- cbind(d, onehot)

# Fire burn classes
d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned <- factor(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned)
onehot <- model.matrix(~0+d[, 'Number_of_days_since_fire_classes_MCD64A1_sites_cleaned'])
attr(onehot, "dimnames")[[2]] <- paste("Number_of_days_since_fire_classes_MCD64A1_sites_cleaned", levels(d$Number_of_days_since_fire_classes_MCD64A1_sites_cleaned), sep="_")
d <- cbind(d, onehot)



# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites", "swe_terraclimate_sites", # climate
                       
                       "tmean_TerraClimate_averages", "ppt_TerraClimate_averages",  # don't have all of these:  "swe_TerraClimate_averages", "srad_TerraClimate_averages", "vpd_TerraClimate_averages", "pdsi_TerraClimate_averages", 
                       
                       "trend_20yrprior_terra_change_id", # temperature change trend
                       
                       "value_era5_soilmoisture", # soil moist
                       
                       "NDVI_MOD13A2v006_NDVI_EVI_sites_low_quality", "EVI_MOD13A2v006_NDVI_EVI_sites_low_quality", "Fpar_MCD15A3Hv006_FPAR_LAI_sites_low_quality", "Lai_MCD15A3Hv006_FPAR_LAI_sites_low_quality",# Optical RS
                       
                       "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", "LST_Night_1km_MOD11A2v006_LST_Night_sites_low_quality", "NDSI_Snow_Cover_MOD10A1_SnowCover_sites_lower_quality",
                       
                       "Albedo_WSA_shortwave_MCD43A3v006_Albedo_WSA_shortwave_sites_low_quality", 
                       
                       "Nadir_Reflectance_Band1_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band2_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band3_MCD43A4_reflectance_sites_low_quality",
                       "Nadir_Reflectance_Band4_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band5_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band6_MCD43A4_reflectance_sites_low_quality","Nadir_Reflectance_Band7_MCD43A4_reflectance_sites_low_quality", 
                       
                       "brightness_MCD43A4_tasseledcap_sites_low_quality","greenness_MCD43A4_tasseledcap_sites_low_quality","wetness_MCD43A4_tasseledcap_sites_low_quality",
                       
                       #"water_ground_MCD43A4_water_ground_sites_low_quality", "water_vegetation_MCD43A4_water_vegetation_sites_low_quality", # lots of NA
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
                       
                       "ABoVE_fractional_water_ABoVE_fractional_water", "AMSR6km_thaw_days_NTSG_FT_AMSR_6km", #microwave
                       
                       "Percent_Tree_Cover_MOD44B_sites", "Percent_NonVegetated_MOD44B_sites", #vegetation type/fraction: add "Percent_NonTree_Cover_MOD44B_sites",
                       
                       
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_0000", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_120", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_160", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_21", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_21", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_30", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_31", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_33", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_41", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_60", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_70", 
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_80", 
                       
                       "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                       
                       "BLDFIE_M_250m_ll_30_agg_SoilGrids", "PHIHOX_M_250m_ll_30_agg_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", "wtd_Water_table_depth", 
                       "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent_30_agg", # check soilgrids water and water balance and water table depth
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", "PFR_ESA_CCI_Permafrostv2", #"ALT_ESA_CCI_Permafrostv2", # Permafrost
                       
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
Baseline_vars_20km <- c("srad_terraclimate_sites", "vpd_terraclimate_sites", "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites", "swe_terraclimate_sites", # climate
                        
                        "tmean_TerraClimate_averages", "ppt_TerraClimate_averages",  # don't have all of these! "swe_TerraClimate_averages", "srad_TerraClimate_averages", "vpd_TerraClimate_averages", "pdsi_TerraClimate_averages", 
                        
                        "trend_20yrprior_terra_change_id", # temperature change trend
                        
                        #ERA5 soil moist
                        "value_era5_soilmoisture",
                        
                        "ndvi3g_mean_GIMMS3g_NDVI_sites_high_and_low_quality", "lai3g_mean_LAI3g_sites_high_and_low_quality", "fpar3g_mean_FPAR3g_sites_high_and_low_quality",# Optical RS
                        
                        #"vod_kuband_mean_vod_kuband_sites", # many NAs - explore!!
                        
                        "GlobSnow3_SWE_GlobSnow3_snowcover",
                        
                        "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
                        
                        "AMSR_thaw_days_NTSG_FT_AMSR_25km", #microwave
                        
                        "Percent_TreeCover_AVHRR_VCF5KYR",  "Percent_NonVegetated_AVHRR_VCF5KYR", #vegetation type/fraction: add "Percent_NonTreeCover_AVHRR_VCF5KYR",
                        
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_0000", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_120", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_160", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_21", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_21", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_30", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_31", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_33", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_41", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_60", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_70", 
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_80", 
                        
                        "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                        
                        "BLDFIE_M_250m_ll_30_agg_SoilGrids", "PHIHOX_M_250m_ll_30_agg_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", "wtd_Water_table_depth", 
                        "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent_30_agg", # check soilgrids water and water balance and water table depth
                        
                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", "PFR_ESA_CCI_Permafrostv2", #"ALT_ESA_CCI_Permafrostv2", # Permafrost
                        
                        # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_0",   # Disturbance
                        # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_1",
                        # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_2",
                        # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_2",
                        # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_3",
                        # "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned_4",
                        
                        
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
  
  # vegetation type NA coded as ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_0000 -> remove if this is 1
  modeldata1 <- subset(modeldata1, ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_0000==0)
  modeldata2 <- subset(modeldata2, ESACCI_cavm_general_ESAwaterfix_broadevfix_ESACCI_CAVM_merged_0000==0)
  
  # create a row ID
  modeldata1$samplerow <- seq(1, length(modeldata1$Study_ID_Short), by=1)
  
  # create a row ID
  modeldata2$samplerow <- seq(1, length(modeldata2$Study_ID_Short), by=1)
  
  
  # merge back other information - we need this!!! TÄHÄN JÄiN NYT!!!
  modeldata11 <- merge(modeldata1[ , !(names(modeldata1) %in%  c("Study_ID_Short",  i, Baseline_vars_1km))], d, by="id")
  modeldata22 <- merge(modeldata2[ , !(names(modeldata2) %in%  c("Study_ID_Short",  i, Baseline_vars_20km))], d, by="id")
  

  
  
  
  for (km in kms) {
    
  # km <- "1km"
  # i <- "NEE_gC_m2"
    
  
  # empty varimp data frame
  all_varImp <- NA
  
  
  # Loop through the models 
  
  for (m in models) {
    
    # m <- "rf"

    # Load model files
    mod <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i, km, m, sep="_"), ".rds"))
    # m <- readRDS(paste0("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/", paste(i, "1km_test", sep="_"), ".rds"))  # remove _test when we have final
    # rfFit <- readRDS(paste0("../results/", paste(i, "rf", sep="_"), ".rds"))
    # svmFit <- readRDS(paste0("../results/", paste(i, "svm", sep="_"), ".rds"))
    
    
    # Print the best variables
    print("Best variables are:")
    mod$bestSubset
    mod$optVariables
    Selected_vars_1km <- mod$optVariables

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
    


    # Extract the final model details
    preds <- mod$pred %>%
      filter(Variables==mod$bestSubset)
    # used this at first: plot(mod$fit$predicted, mod$fit$y) # but it is wrong
    

    # Merge
    if (km=="1km") {
      
      obspred <- merge(modeldata1, preds, by.x="samplerow", by.y="rowIndex")
      #plot(obspred$NEE_gC_m2, obspred$obs)
      
    } else {
      
      obspred <- merge(modeldata2, preds, by.x="samplerow", by.y="rowIndex")
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
    
    
    ### Plot XGBOOST based on categorical information that we have
    
    # Colored by biome
    p1 <- ggplot(obspred, aes(x=pred, y=obs, colour=factor(Biome))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f   \nRsquared = %.2f   \nRMSE = %.1f   \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    # Print out
    setwd("/mnt/data1/boreal/avirkkala/repos/abcflux_modeling/results/figures/")
    p1
    dev.copy(png, paste0(i, "_predperf_biome.png"), width=500, height=400)
    dev.off()
    
    
    # Colored by veg type
    p2 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Veg_type2))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p2
    dev.copy(png, paste0(i, "_predperf_vegtype.png"), width=500, height=400)
    dev.off()
    
    
    # Colored by disturbance
    r$Disturbance <- ifelse(is.na(r$Disturbance), "NA", r$Disturbance)
    p3 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Disturbance))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p3
    dev.copy(png, paste0(i, "_predperf_disturbance.png"), width=500, height=400)
    dev.off()
    
    
    # Colored by Study ID Clean
    p4 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Study_ID_Short))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p4
    dev.copy(png, paste0(i, "_predperf_studyID.png"), width=500, height=400)
    dev.off()
    
    
    # Colored by flux method
    p5 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Flux_method))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p5
    dev.copy(png, paste0(i, "_predperf_fluxmethod.png"), width=500, height=400)
    dev.off()
    
    
    # Colored by flux method detail
    p6 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Flux_method_detail))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank()) + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    
    p6
    dev.copy(png, paste0(i, "_predperf_fluxmethoddetail.png"), width=500, height=400)
    dev.off()
    
    
    
    # Colored by Measurement frequency
    p7 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Measurement_frequency))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p7
    dev.copy(png, paste0(i, "_predperf_measfreq.png"), width=500, height=400)
    dev.off()
    
    
    
    # Color only the sites with outlier observations
    r$Study_ID_figure <- ifelse(abs(r[, "obs"]-r[, m])>100, as.character(r$Study_ID_Short), "ok")
    p8 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Study_ID_figure))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p8
    dev.copy(png, paste0(i, "_", m, "_predperf_outliersites.png"), width=500, height=400)
    dev.off()
    
    
    
    # Color only the sites with months
    p9 <- ggplot(r, aes(x=r[,m], y=obs, colour=factor(Interval))) +
      geom_point(shape = 16,  size=3) + geom_abline(slope = 1) + 
      
      annotate(label = sprintf("MAE = %.1f \nRsquared = %.2f \nRMSE = %.1f \n ", 
                               r_stats %>% filter(model==m) %>% dplyr::select(MAE) %>% as.character() %>% as.numeric(), 
                               r_stats %>% filter(model==m) %>% dplyr::select(Rsquared) %>% as.numeric(),
                               r_stats %>% filter(model==m) %>% dplyr::select(RMSE) %>% as.numeric()), geom = "text", x = Inf, y = -Inf, size = 4, hjust = 1, vjust = 0) +
      
      theme_pub  + theme(legend.title=element_blank())  + scale_colour_viridis(discrete=TRUE) + xlab(m)
    
    p9
    dev.copy(png, paste0(i, "_", m, "_predperf_months.png"), width=500, height=400)
    dev.off()
    
  
    
    
    ### Variable importance calculation
    varImp <- vip(mod, method="permute", train=r[, Selected_vars_1km], target = r$obs, metric = "rsquared",
                      pred_wrapper = predict.train, nsim=100) # only 10 most important ones selected
    
    
    

  } # model loop done
  
  ### Variable importance plots
  # Extract data and combine into one data frame
  all_varImp <- rbind(all_varImp, cbind(varImp$data, "Model"=c(m)))

  all_varImp$Model <- ifelse(all_varImp$Model=="xgboost", "XGBOOST", all_varImp$Model)
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


# Partial dependence plots

# First baseline var
pd1 <- partial(gbmFit, pred.var = Baseline_vars[1], train=data1)  # don't set plot = TRUE
pd2 <- partial(rfFit, pred.var = Baseline_vars[1], train=data1)
pd3 <- partial(svmFit, pred.var = Baseline_vars[1], train=data1)


# ggplot2
pd1$Model <- "GBM"  # add new column
pd2$Model <- "RF"
pd3$Model <- "SVM"

pd.all1 <- rbind(pd1, pd2, pd3)  # bind rows

pdp_plot1 <- ggplot(pd.all1, aes(x=pd.all1[, 1], y=pd.all1[, 2], color = Model)) +
  geom_line(size=1) + theme_pub  +labs(y="yhat", x=Baseline_vars[1]) + 
  scale_color_manual(values = c(viridis(3)[3],viridis(3)[2], viridis(3)[1]), guide = guide_legend(reverse=TRUE)) +
  theme(legend.position = "none") # + ggtitle(title)

