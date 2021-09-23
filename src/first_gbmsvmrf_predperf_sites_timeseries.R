


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

## List predictors for the models 
# Variables used in 1 km spatial resolution models
Baseline_vars_1km <- c("srad_terraclimate_sites",  "pr_terraclimate_sites", "pdsi_terraclimate_sites", "tmean_terraclimate_sites", 
                       # "vpd_terraclimate_sites", # dropped this because of multicollinearities
                       
                       "trend_20yrprior_terra_change_id", # temperature change trend - not correct yet!
                       
                       "Snow.cover_era5_soilmoist_temp_snow", "Snow.depth_era5_soilmoist_temp_snow", "Soil.temperature.level.1_era5_soilmoist_temp_snow", "Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow", #era5 here
                       
                       # "LST_Day_1km_MOD11A2v006_LST_Day_sites_low_quality", # dropped this now because high correlation with srad and tmean but this seemed to be quite important
                       
                       "NDVI_whittaker_constant_monthly_mean",  # Optical RS
                       
                       "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged",
                       
                       "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                       
                       
                       "dtm_cti_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", "dtm_rough.scale_merit.dem_m_250m_s0..0cm_2018_v1.0_MERIT_topo_indices_250m", # Topo
                       
                       "PHIHOX_M_250m_ll_30_agg_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", 
                       "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent_30_agg", # check soilgrids water and water balance and water table depth
                       
                       "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH",  # Permafrost
                       
                       "Number_of_days_since_fire_classes_MCD64A1_sites_cleaned",
                       
                       "Thermokarst"
                       
                       
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
                        
                        "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged",
                        
                        "aboveground_biomass_carbon_2010_Above_belowground_biomass", "belowground_biomass_carbon_2010_Above_belowground_biomass",
                        
                        "BLDFIE_M_250m_ll_30_agg_SoilGrids", "PHIHOX_M_250m_ll_30_agg_SoilGrids", "SoilGrids_SOC_SoilGrids_SOCstock", "wtd_Water_table_depth", 
                        "sol_watercontent.1500kPa_usda.3c2a1a_m_250m_b0..0cm_1950..2017_v0.1_SoilGrids_watercontent_30_agg", # check soilgrids water and water balance and water table depth
                        
                        "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH_UiO_PEX_20181128_2000_2016_NH", # "PFR_ESA_CCI_Permafrostv2", - no data pre 1997 #"ALT_ESA_CCI_Permafrostv2", # Permafrost
                        
                        "Number_of_days_since_fire_classes_gfed_monthly_calc",
                        
                        
                        "Thermokarst"
                        
                        
)

# check that the columns exist
Baseline_vars_20km %in% colnames(d)



# variables as factors
d$Thermokarst <- as.factor(d$Thermokarst)
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
nvars <- seq(1, 21) # 21 for some, 25 for other


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



# Load the model and extract obs, pred, and Study_ID_Short (pred performance)
# Use the full model to predict back to the observations (model fit)
# Then visualize the timeseries of 1) observations, 2) pred perf observations, 3) model fit observations (with different colors)
# Make three figures (one for each model) and combine them!


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
    
    
    # Loop through the models 
    
    for (m in models) {
      
      # m <- "gbm"
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
      
      if (m=="gbm") {    
        preds <- mod$pred  %>% filter(interaction.depth == mod$bestTune$interaction.depth & n.trees == mod$bestTune$n.trees) %>% data.frame()
      } 
      
      if (m=="rf") {    
        preds <- mod$pred %>%
          filter(mtry == mod$bestTune$mtry & splitrule == mod$bestTune$splitrule & min.node.size == mod$bestTune$min.node.size) 
      }
      
      if (m=="svm") {    
        preds <- mod$pred %>%
          filter(C == mod$bestTune$C) 
      }
      
      # Merge
      if (km=="1km") {
        
        obspred <- merge(modeldata11, preds, by.x="samplerow", by.y="rowIndex")
        #plot(obspred$NEE_gC_m2, obspred$obs)
        
        
        # Predict to the data
        str(obspred)
        obspred$pred_fit <- predict(mod, newdata = obspred[, Baseline_vars_1km])
        #plot(obspred$NEE_gC_m2, obspred$pred_fit) # yes, better fit!
        #plot(obspred$pred, obspred$pred_fit)
        
        
        # plot the ones that have more than one year of measurements
        sites_keep <- obspred %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n()) %>% filter(n>2) 
        obspred2 <- subset(obspred, obspred$Study_ID_Short %in% unique(sites_keep$Study_ID_Short) )
        
        # add NAs to periods with missing data
        # need to do this in a loop
        
        obspred2_na <- NA
        
        for (site in unique(obspred2$Study_ID_Short)) {
          print(site)
          # site <- Vesala_FI-Hyy_tower1
          test <- subset(obspred2, Study_ID_Short==site)
          all_dates <- seq(min(as.Date(test$Start_date)), max(as.Date(test$Start_date)), by="month")
          test2 <- merge(test, all_dates, all=TRUE) %>% as.data.frame()
          obspred2_na <- as.data.frame(rbind(obspred2_na, test2))
        }
        
        obspred2_na <- subset(obspred2_na, !is.na(Study_ID_Short)) # this didn't help with the NA's but never mind...
        
        p1 <- ggplot(obspred2_na) + geom_line(aes(x=as.Date(Start_date), y=obs), col="black") +
          
          geom_line(aes(x=as.Date(Start_date), y=pred), col="red") +
          
          geom_line(aes(x=as.Date(Start_date), y=pred_fit), col="blue") + geom_point(aes(x=as.Date(Start_date), y=obs), col="black", size=0.3) +

         facet_wrap(~Study_ID_Short, scales="free")
       
        print(p3)
        dev.copy(png, paste(i, m, km,"predperf_timeseries_sites.png", sep="_"), width=500, height=400)
        dev.off() 
        

        
        
      } else {
        
        obspred <- merge(modeldata22, preds, by.x="samplerow", by.y="rowIndex")
        #plot(obspred$NEE_gC_m2, obspred$obs)
        
        
        # Predict to the data
        str(obspred)
        obspred$pred_fit <- predict(mod, newdata = obspred[, Baseline_vars_20km])
        #plot(obspred$NEE_gC_m2, obspred$pred_fit) # yes, better fit!
        #plot(obspred$pred, obspred$pred_fit)
        
        
        # plot the ones that have more than one year of measurements
        sites_keep <- obspred %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n()) %>% filter(n>2) 
        obspred2 <- subset(obspred, obspred$Study_ID_Short %in% unique(sites_keep$Study_ID_Short) )
        
        # add NAs to periods with missing data
        # need to do this in a loop
        
        obspred2_na <- NA
        
        for (site in unique(obspred2$Study_ID_Short)) {
          print(site)
          # site <- Vesala_FI-Hyy_tower1
          test <- subset(obspred2, Study_ID_Short==site)
          all_dates <- seq(min(as.Date(test$Start_date)), max(as.Date(test$Start_date)), by="month")
          test2 <- merge(test, all_dates, all=TRUE) %>% as.data.frame()
          obspred2_na <- as.data.frame(rbind(obspred2_na, test2))
        }
        
        obspred2_na <- subset(obspred2_na, !is.na(Study_ID_Short)) # this didn't help with the NA's but never mind...
        
        p1 <- ggplot(obspred2_na) + geom_line(aes(x=as.Date(Start_date), y=obs), col="black") +
          
          geom_line(aes(x=as.Date(Start_date), y=pred), col="red") +
          
          geom_line(aes(x=as.Date(Start_date), y=pred_fit), col="blue") + geom_point(aes(x=as.Date(Start_date), y=obs), col="black", size=0.3) +
          
          facet_wrap(~Study_ID_Short, scales="free")
        
        print(p3)
        dev.copy(png, paste(i, m, km,"predperf_timeseries_sites.png", sep="_"), width=500, height=400)
        dev.off() 
        
      }
      
      
    }
    
    
  }
  
  
}

