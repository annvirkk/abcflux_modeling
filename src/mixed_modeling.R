

library("nlme")
library("MuMIn")



### Add flux data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")


# From Belshe et al. 2013:
# "We included site and site x year x month interaction as random effects to account for variation in the slopes with respect to time due to local site differences. 
# We further tested for an autoregressive (AR1) correlation structure to account for temporal autocorrelation within sites. "


# Anna's random notes: "we a priori assume that the mean of the random slopes is zero. This means, we expect the slopes associated to the various subjects to distribute evenly around a slope of 0 (for example, half should be negative and half positive).
#Now, in the model on your data this does not seem to be true. In your second plot the estimated slopes within each subject are all positive. It looks like this model is invalid for your data. The inclusion of the fixed slope includes the mean of the subject-wise slopes as a degree of freedom, and in this plot you see the random slopes cluster evenly around zero, as you would like.


# subset monthly data
d_nona <- subset(d, !is.na(NEE_gC_m2) & !is.na(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow) & !is.na(Snow.cover_era5_soilmoist_temp_snow) &
                   !is.na(ndvi3g_lowest_mean_GIMMS3g_NDVI_sites_high_and_low_quality) & !is.na(tmean_terraclimate_sites))

# visualize
plot(d_nona$Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow, d_nona$NEE_gC_m2)
plot(d_nona$Snow.cover_era5_soilmoist_temp_snow, d_nona$NEE_gC_m2)
plot(d_nona$ndvi3g_lowest_mean_GIMMS3g_NDVI_sites_high_and_low_quality, d_nona$NEE_gC_m2)
plot(d_nona$tmean_terraclimate_sites, d_nona$NEE_gC_m2)
plot(d_nona$pr_terraclimate_sites, d_nona$NEE_gC_m2)


# process data from monthly values to annual sums/means to simplify analysis at first
d2 <- d %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2),
                                                              Veg_type_Short=paste(unique(Veg_type_Short)), Biome=paste(unique(Biome)), 
                                                              EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH=paste(unique(EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH)), Country=paste(unique(Country)), 
                                                              Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow=mean(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow), 
                                                              tmean_terraclimate_sites=mean(tmean_terraclimate_sites),
                                                              pr_terraclimate_sites=mean(pr_terraclimate_sites),
                                                              Snow.cover_era5_soilmoist_temp_snow=mean(Snow.cover_era5_soilmoist_temp_snow),
                                                              trend_30yrprior_terra_change_id=mean(trend_30yrprior_terra_change_id)) %>% filter(n==12) # select annual measurements # and calculate sums. there will be some NAs as some years won't have 12 obs of all fluxes

# NEE
d_nona <- subset(d2, !is.na(NEE_gC_m2) & !is.na(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow) & !is.na(Snow.cover_era5_soilmoist_temp_snow) &!is.na(tmean_terraclimate_sites))

lme1 <- lme(NEE_gC_m2~Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow + tmean_terraclimate_sites + 
             pr_terraclimate_sites + Snow.cover_era5_soilmoist_temp_snow,
            random=~1|Study_ID_Short,data=d_nona,correlation=corAR1()) 
summary(lme1)
plot(lme1) 
r.squaredGLMM(lme1)

# visualize the relationships
ggplot(data=d_nona) + geom_point(aes(x=d_nona$tmean_terraclimate_sites, y=d_nona$NEE_gC_m2, col=Veg_type_Short)) +
  geom_abline(intercept=25, slope=-0.7,size=1/2) 

# GPP
d_nona <- subset(d2, !is.na(GPP_gC_m2) & !is.na(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow) & !is.na(Snow.cover_era5_soilmoist_temp_snow) &!is.na(tmean_terraclimate_sites))

lme1 <- lme(GPP_gC_m2~Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow + tmean_terraclimate_sites + 
              pr_terraclimate_sites + Snow.cover_era5_soilmoist_temp_snow,
            random=~1|Study_ID_Short,data=d_nona,correlation=corAR1())
summary(lme1)
plot(lme1) 
r.squaredGLMM(lme1)


# Reco
d_nona <- subset(d2, !is.na(Reco_gC_m2) & !is.na(Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow) & !is.na(Snow.cover_era5_soilmoist_temp_snow) &!is.na(tmean_terraclimate_sites))

lme1 <- lme(Reco_gC_m2~Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow + tmean_terraclimate_sites + 
              pr_terraclimate_sites + Snow.cover_era5_soilmoist_temp_snow,
            random=~1|Study_ID_Short,data=d_nona,correlation=corAR1()) 
summary(lme1)
plot(lme1) 
r.squaredGLMM(lme1)


# need to check non-linearity, missing random structures etc...

# And then repeat to seasonal and monthly data