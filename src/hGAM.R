
library("mgcv")
library("tidyverse")
library("dplyr")
library("rethinking")
library("ggplot2")

setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
setwd("D:/repos/flux_upscaling_data/src/")
setwd("C:/Users/annav/Downloads/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")



### Time-series and predictor analysis of NEE first



# Explore how many sites to use for the analyses

# option 1
# keep only sites and years that have at least 12 measurement months (full year)
year_site_keep <- d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n()) %>% filter(n==12)
d1 <- subset(d, d$Study_ID_Short %in% year_site_keep$Study_ID_Short & d$Meas_year %in% year_site_keep$Meas_year & !is.na(d$NEE_gC_m2))

# then out of those, keep only sites that have at least 3 measurement years
year_site_keep2 <- d1  %>% group_by(Study_ID_Short) %>% summarize(nyears =n_distinct(Meas_year)) %>% filter(nyears>=3)
d2 <- subset(d1, d1$Study_ID_Short %in% year_site_keep2$Study_ID_Short)
d2$Study_ID_Short <- factor(d2$Study_ID_Short)

# distribution of years in tundra vs. boreal
ggplot(d2) + geom_histogram(aes(Meas_year)) + facet_wrap(~Biome)
ggplot(d2) + geom_histogram(aes(Meas_year, fill=Veg_type_Short)) + facet_wrap(~Biome)
ggplot(d2) + geom_histogram(aes(Meas_year, fill=Country)) + facet_wrap(~Biome)


# option 2
# keep only sites and years that have at least 5 measurement months 
year_site_keep <- d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n()) %>% filter(n>=5)
d1 <- subset(d, d$Study_ID_Short %in% year_site_keep$Study_ID_Short & d$Meas_year %in% year_site_keep$Meas_year & !is.na(d$NEE_gC_m2))

# then out of those, keep only sites that have at least 3 measurement years
year_site_keep2 <- d1  %>% group_by(Study_ID_Short) %>% summarize(nyears =n_distinct(Meas_year)) %>% filter(nyears>=3)
d2 <- subset(d1, d1$Study_ID_Short %in% year_site_keep2$Study_ID_Short)
d2$Study_ID_Short <- factor(d2$Study_ID_Short)

# distribution of years in tundra vs. boreal
ggplot(d2) + geom_histogram(aes(Meas_year)) + facet_wrap(~Biome)
ggplot(d2) + geom_histogram(aes(Meas_year, fill=Veg_type_Short)) + facet_wrap(~Biome)
ggplot(d2) + geom_histogram(aes(Meas_year, fill=Country)) + facet_wrap(~Biome)
# this would be better because it includes more obs outside US, and more diverse vegetation types (e.g. shrub ecosystems)




# start with option 1
# keep only sites and years that have at least 12 measurement months (full year)
year_site_keep <- d %>% filter(!is.na(NEE_gC_m2)) %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n()) %>% filter(n==12)
d1 <- subset(d, d$Study_ID_Short %in% year_site_keep$Study_ID_Short & d$Meas_year %in% year_site_keep$Meas_year & !is.na(d$NEE_gC_m2))

# then out of those, keep only sites that have at least 3 measurement years
year_site_keep2 <- d1  %>% group_by(Study_ID_Short) %>% summarize(nyears =n_distinct(Meas_year)) %>% filter(nyears>=3)
d2 <- subset(d1, d1$Study_ID_Short %in% year_site_keep2$Study_ID_Short)
d2$Study_ID_Short <- factor(d2$Study_ID_Short)

hist(d2$NEE_gC_m2) # normal distribution


### Explore how much NEE is increasing over time when controlling for interannual variability 
# on average across all sites, as well as separately at individual sites


# Develop model
# for this model, we want to include
# 1) A single common smoother for meas year - NEE
# 2) Group-level smoothers for year-NEE with the same wiggliness (no need to allow for very variable wiggliness in this case)
# 3) A single common smoother for month - NEE
# 4) Group-level smoothers for month-NEE with the same wiggliness
# 5) Random effect smoother, i.e. the random intercept
m1 <- gamm(NEE_gC_m2 ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. 
            s(Meas_year,Study_ID_Short,bs="fs",m=1) + # smoother shapes differ depending on ID. Each group has the same wiggliness.
            s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
            s(Interval,Study_ID_Short,bs="fs",xt=list(bs="cc"), m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
            # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1).  adding xt  important for group-level effects
            s(Study_ID_Short, bs="re"), # The random effect smoother (bs="re"), always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
          data = d2) 

plot(m1)
summary(m1)

# if you use gamm instead of gam then the codes below work:
# if you use gam it prints the following error: Error in magic(G$y, G$X, msp, G$S, G$off, L = G$L, lsp0 = G$lsp0, G$rank,  : 
#BLAS/LAPACK routine 'DLASCLSLeft' gave error code -4
m1 # intercept 3, meas year global effect 0.274?
summary(m1$gamm)
summary(m1$lme)
coef(m1$gam)
coef(m1$lme)
plot(m1$gam,pages=1) # separate smooths for each study ID look quite crazy even though the global smoother seems to make perfect sense
plot(m1$lme,pages=1)



# Predict with the model
# new data frame
years <- data.frame(Meas_year=rep(seq(1989, 2020, by=1), 12*1), Interval=rep(rep(1:12, each=32), 1), Study_ID_Short=rep(rep(unique(d2$Study_ID_Short), each=32*12), 1)) 


p <- predict(m1$gam, years, se.fit=TRUE, type="terms") 
summary(p$fit) # now all the smoother effects are listed separately - sum these 
years$pred <- p$fit[, 1] + p$fit[, 2] + p$fit[, 3] + p$fit[, 4] + p$fit[, 5] 
years$se <- p$se.fit[, 1] + p$se.fit[, 2] + p$se.fit[, 3] + p$se.fit[, 4] + p$se.fit[, 5] # or should these be transferred to variances before summing?


# another option would be to just take the global smoothers s(Meas_year) and s(Interval) - this will produce identical NEE predictions for each site though?
# ie one average prediction 
years$pred_global <- p$fit[, 1] +  p$fit[, 3] 

# visualize
d2 %>% group_by(Study_ID_Short, Meas_year) %>% summarize(sum=sum(NEE_gC_m2)) %>% ggplot() + geom_line(aes(x=Meas_year, y=sum)) + facet_wrap(~Study_ID_Short)
years %>% group_by(Study_ID_Short, Meas_year) %>% summarize(sum=sum(pred)) %>% ggplot() + geom_line(aes(x=Meas_year, y=sum)) + facet_wrap(~Study_ID_Short)
obspred <- merge(years, d2, by=c("Meas_year", "Interval", "Study_ID_Short"))
plot(obspred$NEE_gC_m2, obspred$pred); abline(0,1)

# explore model fit as the squared correlation of observed and fitted values (r²) (similar to Kemppinen et al. 2021)
cor(obspred$NEE_gC_m2, obspred$pred)^2



# 1. Use the model and extract average flux change rates across all sites
# Calculate annual flux change for the full time period (1989-2020) 
# let's focus on annual cumulative flux change first -  aggregate monthly predictions to cumulative annual flux values
years$variance <- years$se^2 # aggregating errors
years2 <- years %>% group_by(Study_ID_Short, Meas_year) %>% summarize(sum=sum(pred), sum.se=sqrt(mean(variance)))  # calculate average se per year by taking the square root of the mean variance, see https://stats.stackexchange.com/questions/25848/how-to-sum-a-standard-deviation
years3 <- years2 %>% group_by(Study_ID_Short) %>% mutate(first_flux=sum[Meas_year==1989], last_flux=sum[Meas_year==2020]) %>% mutate (change = last_flux-first_flux, change_peryear = (last_flux-first_flux)/32, mean_se=mean(sum.se)) %>% select(Study_ID_Short, change, change_peryear, mean_se) %>% unique() # calculating the change by subtracting the predicted last and first years fluxes and divigin by the number of years
# mean_se is not related to the change metric at all, not sure how to get the error for the change estimate?
hist(years3$change_peryear)
mean(years3$change_peryear) # this would be the mean change estimate.. but it's probably impossible to differentiate if its positive/negative when se is considered
# subtracting the last from the first year's value seems a bit too simplistic somehow. But I'm not sure if taking a linear slope makes sense either?


# merge with site-level biome, veg type, permafrost, and region-level data to understand patterns in different conditions
# alternative would be to include these to the model already but the model seems to be performing well already (based on r2), and I'd like to keep the model simple
d2_env <- subset(d2, select=c(Study_ID_Short, Biome, Veg_type_Short,  Country, ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, EXTENT_Permafrost_Brown)) %>% unique()

years3 <- merge(years3, d2_env, by="Study_ID_Short")

years3 %>% group_by(Biome) %>% summarize(mean(change_peryear)) # this would be the mean change estimate in the two biomes
years3 %>% ggplot + geom_histogram(aes(change_peryear)) + facet_wrap(~Biome) + geom_vline(xintercept = 0)

years3 %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% summarize(mean(change_peryear))
years3 %>% ggplot + geom_histogram(aes(change_peryear)) + facet_wrap(~ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) + geom_vline(xintercept = 0)


# Calculate flux change from 2000-2015 during which we have the largest amount of data
years3 <- years2 %>% group_by(Study_ID_Short) %>% mutate(first_flux=sum[Meas_year==2000], last_flux=sum[Meas_year==2015]) %>% mutate (change = last_flux-first_flux, change_peryear = (last_flux-first_flux)/32, mean_se=mean(sum.se)) %>% select(Study_ID_Short, change, change_peryear, mean_se) %>% unique()
hist(years3$change_peryear)
mean(years3$change_peryear)

years3 <- merge(years3, d2_env, by="Study_ID_Short")

years3 %>% group_by(Biome) %>% summarize(mean(change_peryear))
years3 %>% ggplot + geom_histogram(aes(change_peryear)) + facet_wrap(~Biome) + geom_vline(xintercept = 0)

years3 %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% summarize(mean(change_peryear))
years3 %>% ggplot + geom_histogram(aes(change_peryear)) + facet_wrap(~ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) + geom_vline(xintercept = 0)


# Calculate flux change from 2001-2020 which is the MODIS period
years3 <- years2 %>% group_by(Study_ID_Short) %>% mutate(first_flux=sum[Meas_year==2001], last_flux=sum[Meas_year==2020]) %>% mutate (change = last_flux-first_flux, change_peryear = (last_flux-first_flux)/32, mean_se=mean(sum.se)) %>% select(Study_ID_Short, change, change_peryear, mean_se) %>% unique()
hist(years3$change_peryear)
mean(years3$change_peryear)

years3 <- merge(years3, d2_env, by="Study_ID_Short")

years3 %>% group_by(Biome) %>% summarize(mean(change_peryear))
years3 %>% ggplot + geom_histogram(aes(change_peryear)) + facet_wrap(~Biome) + geom_vline(xintercept = 0)

years3 %>% group_by(ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) %>% summarize(mean(change_peryear))
years3 %>% ggplot + geom_histogram(aes(change_peryear)) + facet_wrap(~ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged) + geom_vline(xintercept = 0)




# 2. Flux change rates at individual sites 
# a) how many of the sites show clear patterns of flux increase vs decrease vs no change?
# should probably use standard errors for this to determine if the change is truly positive/negative after considering the error
# but I might be doing this in a too simple way again...
# as an example I'm focusing on the 1989-2020 time period again but this time frame should probably be narrowed down

# a simple approach would be to add/subtract the standard errors from first and last flux values
years3 <- years2 %>% group_by(Study_ID_Short) %>% mutate(first_flux=sum[Meas_year==1989], last_flux=sum[Meas_year==2020]) %>% mutate (
  change1 = (last_flux+sum.se)-(first_flux+sum.se), change_peryear1 = (change1)/32 ,
  change2 = (last_flux-sum.se)-(first_flux-sum.se), change_peryear2 = (change2)/32 ,
  change3 = (last_flux-sum.se)-(first_flux+sum.se), change_peryear3 = (change3)/32 ,
  change4 = (last_flux+sum.se)-(first_flux-sum.se), change_peryear4 = (change4)/32 ) %>% select(Study_ID_Short, change1, change_peryear1 , change2, change_peryear2, change3, change_peryear3, change4, change_peryear4) %>% unique()
# something still wrong with this, change3 and 4 hhave several values per site

# if all of the 4 change per year calculations are negative -> site is experiencing increasing net uptake. etc...
years3$overall_trend <- ifelse(years3$change_peryear1<0 & years3$change_peryear2<0 & years3$change_peryear3<0 & years3$change_peryear4<0, "increasing net uptake", NA)
years3$overall_trend <- ifelse(years3$change_peryear1>0 & years3$change_peryear2>0 & years3$change_peryear3>0 & years3$change_peryear4>0, "decreasing net uptake", years3$overall_trend)
years3$overall_trend <- ifelse(is.na(years3$overall_trend), "no clear trend", years3$overall_trend)


# b) how many of the sites that are on average CO2 sinks show clear patterns of flux increase vs decrease vs no change - and same for CO2 sources and Co2 neutral sites (CO2 neutral could be defined as annual NEE -5 - +5 g C m-2 yr-1)


# define neutral, source, and sink sites
d2_avgsites <- d2 %>% group_by(Study_ID_Short, Meas_year) %>% summarize(nee=sum(NEE_gC_m2)) %>% group_by(Study_ID_Short) %>% summarize(avg_nee=mean(nee))
d2_avgsites$sitestatus <- NA
d2_avgsites$sitestatus <- ifelse(d2_avgsites$avg_nee<= -5, "sink", d2_avgsites$sitestatus)
d2_avgsites$sitestatus <- ifelse(d2_avgsites$avg_nee>= 5, "source", d2_avgsites$sitestatus)
d2_avgsites$sitestatus <- ifelse(is.na(d2_avgsites$sitestatus), "neutral", d2_avgsites$sitestatus)

# merge with the change table
years4 <- merge(years3, d2_avgsites, by="Study_ID_Short")

# explore
years5 <- years4 %>% select(Study_ID_Short, sitestatus, overall_trend) %>% unique() # mostly source sites are experiencing increasing net uptake


# 3. How much does a temperature, NDVI, VPD, snow cover, soil moisture, and CO2 concentration change influence NEE?
d3 <- subset(d2, select=c(Study_ID_Short, Meas_year, NEE_gC_m2, tmean_terraclimate_sites, ndvi3g_lowest_gapfilled_mean_GIMMS3g_NDVI_sites_low_quality_gapfilled,
                          
                          vpd_terraclimate_sites, Snow.cover_era5_soilmoist_temp_snow, Volumetric.soil.water.layer.1_era5_soilmoist_temp_snow,
                          
                          Barrow_CO2_conc_Barrow_CO2conc))

d4 <- na.omit(d3)

# check correlations
cor(d4[, 2:ncol(d4)])

# high correlations - focus on tmean alone because it correlates >0.8 with most of the key variables? # tmean values are monthly averages Celsius*10
d3 <- subset(d2, select=c(Study_ID_Short, Meas_year, Interval, NEE_gC_m2, tmean_terraclimate_sites))

d4 <- na.omit(d3)
plot(d4$tmean_terraclimate_sites, d4$NEE_gC_m2)


# Use the same model as above but include tmean smoother
m1 <- gamm(NEE_gC_m2 ~ s(Meas_year, bs = "tp",k=3) + 
             s(Meas_year,Study_ID_Short,bs="fs",m=1) + 
             s(Interval, bs = "cc", k=12) +  
             s(Interval,Study_ID_Short,bs="fs",xt=list(bs="cc"), m=1) +  
             s(Study_ID_Short, bs="re") + 
             s(tmean_terraclimate_sites), 
           data = d4) 


m1 
summary(m1$gamm)
summary(m1$lme)
coef(m1$gam)
coef(m1$lme)
plot(m1$gam,pages=1) # temperature response seems a bit strange - in warm conditions the net emissions seem to increase
# also the temporal trends change in this model version (no clear changes), not sure what to think about that?
plot(m1$lme,pages=1)

# where are the sites that have >190 C temperatures?
d4 %>% filter(tmean_terraclimate_sites>180) %>% summarize(unique(Study_ID_Short))
d4 %>% filter(tmean_terraclimate_sites>200) %>% summarize(unique(Study_ID_Short))
d4 %>% filter(tmean_terraclimate_sites>190) %>% ggplot() + geom_point(aes(x=tmean_terraclimate_sites, y=NEE_gC_m2, col=Study_ID_Short))
# Ru-Fyo seems to be driving this pattern


# Predict with the model for each year and month
# for this we might need to have the full time series of tmean from 1989 to 2020 for the sites, right?
# and otherwise the dataset would be similar to the years df as described above

# let's assume d4 would be our prediction data frame
years <- d4
p <- predict(m1$gam, years, se.fit=TRUE, type="terms") 
summary(p$fit) 
years$pred <- p$fit[, 1] + p$fit[, 2] + p$fit[, 3] + p$fit[, 4] + p$fit[, 5] 
years$se <- p$se.fit[, 1] + p$se.fit[, 2] + p$se.fit[, 3] + p$se.fit[, 4] + p$se.fit[, 5] 

# how to get the flux response from here now that the response is non-linear?
# I'd like to know how a temp increase of eg 1 C increases the flux
# a) across the entire data set
# b) across individual sites 
# and then these could be explored across the different seasons (spring=MAM, summer=JJA, autumn=SON, winter=DJF), regions etc.















### Konsta's and Anna's earlier model versions with a focus on log-transformed GPP and Reco data ###



dd <- d %>% dplyr::select(Study_ID_Short,Interval,Meas_year,NEE_gC_m2,tmean_terraclimate_sites,terra_trend_19601990,GPP_gC_m2,Reco_gC_m2) %>% na.omit()
dd$Study_ID_Short <- as.factor(dd$Study_ID_Short)
dd <- dd %>% mutate(id = as.numeric(Study_ID_Short))
dd %>%
  filter(id %in% 1:30) %>%
  ggplot(aes(Meas_year,NEE_gC_m2)) +
  geom_col(aes(color=factor(Interval, levels = 1:12, ordered = TRUE)), position = "dodge") +
  facet_wrap(~id, scales = "free") +
  theme(legend.position = "none")
dd %>% ggplot(aes(Meas_year,NEE_gC_m2)) + geom_point()



d1 <- dd %>% filter(id %in% c(9,12,13,29))

d1 %>% ggplot(aes(Meas_year,GPP_gC_m2)) +
  geom_col(aes(fill=factor(Interval, levels = 1:12, ordered = TRUE)), position = "dodge") +
  facet_wrap(~id, scales = "free") +
  theme(legend.position = "none")
d1 %>% ggplot(aes(Meas_year,Reco_gC_m2)) +
  geom_col(aes(fill=factor(Interval, levels = 1:12, ordered = TRUE)), position = "dodge") +
  facet_wrap(~id, scales = "free") +
  theme(legend.position = "none")

d1$ID <- factor(d1$id)

d1 <- d1 %>% mutate(GPP = -GPP_gC_m2, ER = Reco_gC_m2)
d1 <- d1 %>% mutate(GPP = case_when(GPP<=0 ~ 0.3,TRUE ~ GPP),
                    ER = case_when(ER<=0 ~ 2.9,TRUE ~ ER))


### Example by Konsta that answers the following questions: 
# 1) how does Reco increase annually over time at individual sites?
# 2) what does interannual variability across sites look like?
# 3) how much does an increase in tmean increase Reco?
m1 <- gamm(log(ER) ~ s(Meas_year, bs = "tp",by = ID,k=3) + # tp=thin plate regression splines (TPRS) smoother. by: allow each group-specific smoother to have its own smoothing parameter
             s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
             s(Interval,ID,bs="fs",m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
             # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability
             s(ID, bs="re") + # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
             s(tmean_terraclimate_sites),
           #correlation = corARMA(form = ~ 1|ID, p = 1),
           data = d1)

m1$gam
summary(m1$gam)
m1
plot(m1$gam,pages=1)
plot(m1$gam,select = 6, ylim = c(-3, 3))
acf(resid(m1$lme))
pacf(resid(m1$lme))


# We  explicitly include a random effect for the intercept (the bs="re" term), as
# group-specific intercepts are not incorporated into factor by variable smoothers
# (as would be the case with a factor smoother or a tensor product random effect).




### Anna modifying the model to understand how much Reco is increasing over time when controlling for interannual variability 
# For this model, we want to include
# 1) A single common smoother for meas year - Reco
# 2) Group-level smoothers for year-Reco with the same wiggliness (no need to allow for very variable wiggliness in this case)
# 3) A single common smoother for month - Reco
# 4) Group-level smoothers for month-Reco with the same wiggliness
# 5) Random effect smoother, i.e. the random intercept
m1 <- gam(log(ER) ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. 
                           s(Meas_year,ID,bs="fs",m=1) + # smoother shapes differ depending on ID. Each group has the same wiggliness.
                           s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
                           s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
                           # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
                           s(ID, bs="re"), # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
                       data = d1) # s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) adding xt was important for group-level effects

plot(m1)
summary(m1)

# if you use gamm instead of gam then the codes below work:
m1 # intercept 3, meas year global effect 0.274?
summary(m1$gamm)
summary(m1$lme)
coef(m1$gam)
coef(m1$lme)
plot(m1$gam,pages=1)
plot(m1$lme,pages=1)



# Predict using the global smoother only
# new data frame

years <- data.frame(Meas_year=rep(seq(1989, 2020, by=1), 12*4), Interval=rep(rep(1:12, each=32), 4), ID=rep(rep(unique(d1$ID), each=32*12), 4)) 
# years <- data.frame(Meas_year=rep(seq(1989, 2020, by=1), 12), Interval=rep(1:12, each=32)) 
# yearsd <- data.frame(rbind(data.frame(cbind(years$Meas_year, years$Interval, years$ID=))))

p <- predict(m1, years, se.fit=TRUE, type="terms") # include only the two basic splines
years$pred <- p$fit
years$se <- p$se.fit


# Calculate % change
years2 <- years %>% group_by(ID, Meas_year) %>% summarize(mean=mean(pred)) ##  sum of log is the same as multiplication, don't calculate that

years3 <- years2 %>% group_by(ID) %>% mutate(first_flux=mean[Meas_year==1989], last_flux=mean[Meas_year==2020]) %>% mutate (change = last_flux-first_flux) 

# log change is already percentage change: https://stats.stackexchange.com/questions/244199/why-is-it-that-natural-log-changes-are-percentage-changes-what-is-about-logs-th
mean(years3$change) # Reco decreased by 36 % over the 32-year study period, i.e. 36/32=1 % per year

# Calculate flux change by using intercept (exp) as an average flux
exp(coef(m1)[1]) # Reco in the beginning of the period
exp(coef(m1)[1])*(1-0.36)
# so flux decreased by 20 g C m-2 over the study period




### Try the same without Interval
m1 <- gam(log(ER) ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. 
            s(Meas_year,ID,bs="fs",m=1) + # smoother shapes differ depending on ID. Each group has the same wiggliness.
            # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
            s(ID, bs="re"), # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
          data = d1) # s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) adding xt was important for group-level effects

plot(m1) # losing the trend here...
summary(m1) 



# Try the same with annual sum - need more data for this
d2 <- d %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n(), NEEsum=sum(NEE_gC_m2) , Recosum=sum(Reco_gC_m2)) %>% filter(n==12) 
d2$Study_ID_Short <- as.factor(d2$Study_ID_Short)
m1 <- gam(NEEsum ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. 
            s(Meas_year,Study_ID_Short,bs="fs",m=1) + # smoother shapes differ depending on ID. Each group has the same wiggliness.
            # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
            s(Study_ID_Short, bs="re"), # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
          data = d2) # s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) adding xt was important for group-level effects

plot(m1) # losing the trend here...
summary(m1) 

### Add complexity: biome information to the model 
m1 <- gam(log(ER) ~ s(Meas_year, bs = "tp",k=3) + # tp=thin plate regression splines (TPRS) smoother. 
            s(Meas_year,ID,bs="fs",m=1) + # smoother shapes differ depending on ID. Each group has the same wiggliness.
            s(Interval, bs = "cc", k=12) +  # bs="cc": Cyclic cubic regression splines: start and end values constrained to match. k=amount of smoothing, ie months in this case
            s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) +  # bs="fs": factor smooth interaction. This smooth class allows a separate smooth for each level of a factor, with the same smoothing parameter for all smooths. It is an alternative to using factor by variables.
            # m=The order of the penalty for this term, usually quite low in examples, 1-2. m=1: direct, m=2: direct, any direction. is defined because increases stability s(Meas_year,ID,bs="fs",m=1) +
            s(ID, bs="re"), # The random effect smoother (bs="re") that we used, always has a k value equal to the number of levels in the grouping variable. This represents a random effect for the intercept, similar to mixed model structure!
          data = d1) # s(Interval,ID,bs="fs",xt=list(bs="cc"), m=1) adding xt was important for group-level effects

plot(m1)
summary(m1)




