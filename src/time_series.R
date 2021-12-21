


# We used the nonparametric Mann–Kendall test to estimate annual and seasonal flux and environmental trends across the sites and the entire ABZ. 
# The test was run in the zyp package
# with pre-whitening (Yue and Pilon method) to remove autocorrelation. 
# We report Kendall’s correlation coefficient, τ, to describe the strength of the time-series and the Theil– Sen slope to describe trends over time. 
# We ran the analysis using the fluxes aggregated to annual cumulative values, 
# after which seasonal separate tests for spring, summer, autumn, and winter cumulative values were calculated, 
# and different versions of these grouped to vegetation types and biomes. 
# Similarly, environmental variables were grouped to annual or seasonal averages. Flux trends were first estimated at the site level and pixel-wise trends across the entire ABZ, after which an average trend from those was calculated.
# 
# To verify that the overall observed trend at flux sites, with varying amounts of data from each site, 
# is robust we used a non-linear mixed effects model with site as a random effect together with an autoregressive (AR1) correlation structure. 
# Some flux time series consist of monthly fluxes partitioned in different ways (see column Gap_fill). 
# At these sites, we kept the observations that were partitioned the same way and had the largest number of observations in our analysis; 
# the remaining observations were removed to assure that the site-level time series analyses will not be affected by the various data processing pipelines.





# packages 
library(raster)
library(parallel)
library("zyp") # trend would be a more efficient package but it does not include the option to detrend time series
library(snow)
library("dplyr")
library("nlme")




### Add flux data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv")

### Annual trend analysis
# Preprocess
# calculate annual sums
d2 <- d %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) %>% filter(n==12) # select annual measurements # and calculate sums. there will be some NAs as some years won't have 12 obs of all fluxes
d3 <- d2 %>% group_by(Study_ID_Short) %>% summarize(n=n()) %>% filter(n>=5) # select sites that have >=5 years - check if this makes sense
d4 <- subset(d2, d2$Study_ID_Short %in%  d3$Study_ID_Short )

# Calculate the trend analysis for each site with at least 5 years of data
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 
annual_trends <- NA


for (i in unique(d4$Study_ID_Short)) {
  
  for (r in resp_vars) {
    
    print(i)
    print(r)
    # i <- "Euskirchen_US-TFBS_tower1"
    # i <- "Vesala_FI-Hyy_tower1"
    # i <- "Schuur_US-EML_tower1"
    # r <- "NEE_gC_m2"
    
    d5 <- subset(d4, Study_ID_Short==i & !is.na(d4[, r])) # or any(is.na(r))????
    
    if(nrow(d5)>=5) {
      
      trend_site <- zyp.trend.vector(y=pull(d5, r), x=d5$Meas_year, method="yuepilon") %>% t() %>% as.data.frame() # THIS IS A DATA FRAME WITH SEVERAL COLUMNS!!
      
      annual_trends <- rbind(annual_trends, cbind(trend_site, "Study_ID_Short"=c(i), "Flux"=c(r)))
      
      
    }

  }
  
}


# Check the outcome
# we're interested of Kendall’s correlation coefficient, τ (tau), to describe the strength of the time-series 
# and the Theil– Sen slope to describe trends over time (trend)
annual_trends %>% filter(Flux=="NEE_gC_m2") %>% summary()
annual_trends %>% filter(Flux=="GPP_gC_m2") %>% summary()
annual_trends %>% filter(Flux=="Reco_gC_m2") %>% summary()
unique(annual_trends$Study_ID_Short)



### Seasonal trend analysis
# Preprocess
# calculate annual means
d2 <- d %>% group_by(Study_ID_Short, Meas_year, Season) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) %>% filter(n==3) # select annual measurements # and calculate sums. there will be some NAs as some years won't have 12 obs of all fluxes
d3 <- d2 %>% group_by(Study_ID_Short) %>% summarize(n=n()) %>% filter(n>=5) # select sites that have >=5 years - check if this makes sense
d4 <- subset(d2, d2$Study_ID_Short %in%  d3$Study_ID_Short )

# Calculate the trend analysis for each site with at least 5 years of data
resp_vars <- c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2") 
seasons <- c("spring", "summer", "autumn", "winter")
seasonal_trends <- NA


for (i in unique(d4$Study_ID_Short)) {
  
  for (r in resp_vars) {
    
    for (s in seasons) {
      
      print(s)
      print(i)
      print(r)
      # i <- "Euskirchen_US-TFBS_tower1"
      # i <- "Vesala_FI-Hyy_tower1"
      # r <- "NEE_gC_m2"
      
      d5 <- subset(d4, Study_ID_Short==i & !is.na(d4[, r]) & Season==s) # or any(is.na(r))????
      
      if(nrow(d5)>=5) {
        
        trend_site <- zyp.trend.vector(y=pull(d5, r), x=d5$Meas_year, method="yuepilon") %>% t() %>% as.data.frame() # THIS IS A DATA FRAME WITH SEVERAL COLUMNS!!
        
        seasonal_trends <- rbind(seasonal_trends, cbind(trend_site, "Study_ID_Short"=c(i), "Flux"=c(r), "Season"=s))
        
      }
      
    }
    
  }
  
}


# Check the outcome
# we're interested of Kendall’s correlation coefficient, τ (tau), to describe the strength of the time-series 
# and the Theil– Sen slope to describe trends over time (trend)
seasonal_trends %>% filter(Flux=="NEE_gC_m2") %>% group_by(Season) %>% summarize(trendmean=mean(trend), trendmedian=median(trend))
seasonal_trends %>% filter(Flux=="GPP_gC_m2") %>% group_by(Season) %>% summarize(trendmean=mean(trend), trendmedian=median(trend))
seasonal_trends %>% filter(Flux=="Reco_gC_m2") %>% group_by(Season) %>% summarize(trendmean=mean(trend), trendmedian=median(trend))
unique(seasonal_trends$Study_ID_Short)



# Combine with Biome, Country, Veg type, and Permafrost info

env <- subset(d, select=c(Study_ID_Short, Biome, Country, Veg_type_Short, Permafrost)) %>% unique()

annual_trends2 <- merge(annual_trends, env, by="Study_ID_Short")
seasonal_trends2 <- merge(seasonal_trends, env, by="Study_ID_Short")


summary(annual_trends2)
summary(seasonal_trends2)

annual_trends2 %>% group_by(Flux, Biome) %>% summarize(trendmean=mean(trend, na.rm=TRUE), trendmedian=median(trend, na.rm=TRUE))



### Try the same with the entire data

# To verify that the overall observed trend at flux sites, with varying amounts of data from each site, 
# is robust we used a non-linear mixed effects model with site as a random effect together with an autoregressive (AR1) correlation structure. 
# Some flux time series consist of monthly fluxes partitioned in different ways (see column Gap_fill). 
# At these sites, we kept the observations that were partitioned the same way and had the largest number of observations in our analysis; 
# the remaining observations were removed to assure that the site-level time series analyses will not be affected by the various data processing pipelines.


# calculate annual means
d2 <- d %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2),
                                                              Veg_type_Short=paste(unique(Veg_type_Short)), Biome=paste(unique(Biome)), 
                                                              EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH=paste(unique(EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH)), Country=paste(unique(Country))) %>% filter(n==12) # select annual measurements # and calculate sums. there will be some NAs as some years won't have 12 obs of all fluxes
d3 <- d2 %>% group_by(Study_ID_Short) %>% summarize(n=n()) %>% filter(n>=5) # select sites that have >=5 years - check if this makes sense
d4 <- subset(d2, d2$Study_ID_Short %in%  d3$Study_ID_Short )

d5 <- merge(d4, env, by="Study_ID_Short")

# very simple graph
ggplot() + geom_point(data=d5, aes(x=Meas_year, y=NEE_gC_m2, col=Biome))  + geom_smooth(data=subset(d5, Biome=="Boreal"), aes(x=Meas_year, y=NEE_gC_m2), col="red", method="lm") + 
  geom_smooth(data=d5, aes(x=Meas_year, y=NEE_gC_m2), col="black", method="lm") +
  geom_smooth(data=subset(d5, Biome=="Tundra"), aes(x=Meas_year, y=NEE_gC_m2), col="blue", method="lm") 

hist(d5$NEE_gC_m2) #normally distributed!


# NEE, whole data, as an example
ggplot(d2) + geom_point(aes(x=Meas_year, y=NEE_gC_m2)) 
lme1 <- lme(NEE_gC_m2~Meas_year,random=~Meas_year|Study_ID_Short,data=subset(d2, !is.na(NEE_gC_m2)),correlation=corAR1()) # should I add Meas_year??
lme1
summary(lme1)
plot(lme1, which = 1) # residuals quite nicely flat
qqnorm(lme1)

# function to do the same
nlme_function <- function(data, resp_var) {
  
  d5_nona <- subset(data, !is.na(data[[resp_var]]))
  
  # Meas year as a fixed term, Study ID as a random intercept and slope, correlation structure
  lme1 <- lme(as.formula( paste( resp_var, "~", paste(c("Meas_year"), collapse="+") ) ), random=~Meas_year|Study_ID_Short,data=d5_nona,correlation=corAR1(), control=c(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'))
  plot(lme1, which = 1) # residuals quite nicely flat
  qqnorm(lme1)
  lme1
  fixed_effects <- summary(lme1)$tTable
  
}

nee_all <- nlme_function(d2, "NEE_gC_m2")
gpp_all <- nlme_function(d2, "GPP_gC_m2") # error.  this helps: lme1 <- lme(as.formula( paste( resp_var, "~", paste(c("Meas_year"), collapse="+") ) ), random=~Meas_year|Study_ID_Short,data=d5_nona)
reco_all <- nlme_function(d2, "Reco_gC_m2") # same error. 



nlme_function_nocorar <- function(data, resp_var) {
  
  d5_nona <- subset(data, !is.na(data[[resp_var]]))
  
  # Meas year as a fixed term, Study ID as a random intercept and slope, correlation structure
  lme1 <- lme(as.formula( paste( resp_var, "~", paste(c("Meas_year"), collapse="+") ) ), random=~Meas_year|Study_ID_Short,data=d5_nona, control=c(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'))
  plot(lme1, which = 1) # residuals quite nicely flat
  qqnorm(lme1)
  lme1
  fixed_effects <- summary(lme1)$tTable
  
}

nee_all <- nlme_function_nocorar(d2, "NEE_gC_m2")
gpp_all <- nlme_function_nocorar(d2, "GPP_gC_m2") 
reco_all <- nlme_function_nocorar(d2, "Reco_gC_m2")  # error??



# function to do the same across different groups, no corar
nlme_function_category <- function(data, resp_var, category) {
  
  d5_nona <- subset(data, !is.na(data[[resp_var]]))
  
  categorylevels <- levels(factor(data[[category]]))
  
  fixed_effects_all <- NA
  
  for (i in categorylevels) {
    
    # i <- "spring"
    d5_nona2 <- subset(d5_nona, d5_nona[[category]]==i)
    
    # Meas year as a fixed term, Study ID as a random intercept and slope, correlation structure
    lme1 <- lme(as.formula( paste( resp_var, "~", paste(c("Meas_year"), collapse="+") ) ), random=~Meas_year|Study_ID_Short,data=d5_nona2, control=c(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'))
    plot(lme1, which = 1) # residuals quite nicely flat
    qqnorm(lme1)
    lme1
    fixed_effects <- summary(lme1)$tTable
    fixed_effects_all <- rbind(fixed_effects_all, cbind(fixed_effects, "category"=i))
    
  }
  
  return(fixed_effects_all)
  
}


# before applying, check that there are enough years for vegetation types!
dd <- subset(d2, !is.na(NEE_gC_m2))
dd %>% group_by(Veg_type_Short) %>% summarize(n=n(), sites=length(unique(Study_ID_Short)))
ggplot(d2) + geom_point(aes(x=Meas_year, y=NEE_gC_m2)) + facet_wrap(~Veg_type_Short)

d22 <- subset(d2, Veg_type_Short=="BW" | Veg_type_Short=="EN" | Veg_type_Short=="G" | Veg_type_Short=="W")
test <- nlme_function_category(d22, "NEE_gC_m2", "Veg_type_Short")

# biome
ggplot(d2) + geom_point(aes(x=Meas_year, y=NEE_gC_m2)) + facet_wrap(~Biome)

test <- nlme_function_category(d2, "NEE_gC_m2", "Biome")
test <- nlme_function_category(d2, "GPP_gC_m2", "Biome")
test <- nlme_function_category(d2, "Reco_gC_m2", "Biome")

ggplot(subset(d2, Biome=="Boreal")) + geom_point(aes(x=Meas_year, y=NEE_gC_m2)) + geom_abline(slope=-2.57, intercept=5132) + 
  theme_pub + ylab(expression(paste("NEE g C m"^{-2}, yr^{-1}))) + xlab("Measurement year")

ggplot(subset(d2, Biome=="Tundra")) + geom_point(aes(x=Meas_year, y=NEE_gC_m2)) + geom_abline(slope=0.42, intercept=-836) + 
  theme_pub + ylab(expression(paste("NEE g C m"^{-2}, yr^{-1})))  + xlab("Measurement year")

# permafrost
test <- nlme_function_category(d2, "NEE_gC_m2", "EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH")

# Region
test <- nlme_function_category(d2, "NEE_gC_m2", "Country")

# season
d22 <- d %>% group_by(Study_ID_Short, Meas_year, Season) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2),
                                                               Veg_type_Short=paste(unique(Veg_type_Short)), Biome=paste(unique(Biome)), Season=paste(unique(Season))) %>% filter(n==3) 
ggplot(d22) + geom_point(aes(x=Meas_year, y=Reco_gC_m2)) + facet_wrap(~Season)

test <- nlme_function_category(d22, "NEE_gC_m2", "Season")
test <- nlme_function_category(d22, "Reco_gC_m2", "Season")
test <- nlme_function_category(d22, "GPP_gC_m2", "Season")


# season+biome
d22$Season_Biome <- paste(d22$Season, d22$Biome, sep="_")

ggplot(d22) + geom_point(aes(x=Meas_year, y=GPP_gC_m2)) + facet_wrap(~Season_Biome)

test <- nlme_function_category(d, "NEE_gC_m2", "Season_Biome")

# season-vegetation type
d22 <- subset(d22, Veg_type_Short=="BW" | Veg_type_Short=="EN" | Veg_type_Short=="G" | Veg_type_Short=="W")
d22$Season_Veg_type_Short <- paste(d22$Season, d22$Veg_type_Short, sep="_")

ggplot(d22) + geom_point(aes(x=Meas_year, y=GPP_gC_m2)) + facet_wrap(~Season_Veg_type_Short)

test <- nlme_function_category(d22, "NEE_gC_m2", "Season_Veg_type_Short")


# this might help to error code 1: https://stats.stackexchange.com/questions/101265/nlme-convergence-error-code-1 https://sakai.unc.edu/access/content/group/2842013b-58f5-4453-aa8d-3e01bacbfc3d/public/Ecol562_Spring2012/docs/solutions/assign7.htm







d5_nona <- subset(d5, !is.na(NEE_gC_m2))


### ANNA: need to check how interactions are formulated


# simple case, only site as a random factor - only gives an intercept without predictors!
lme1 <- lme(NEE_gC_m2~1,random=~1|Study_ID_Short,data=d5_nona)
plot(lme1, which = 1) # residuals quite nicely flat
lme1
summary(lme1)


# add Study ID as a random intercept without Meas_years
# ie we a priori assume that the mean of the random slopes is zero. This means, we expect the slopes associated to the various subjects to distribute evenly around a slope of 0 (for example, half should be negative and half positive).
# see: https://stats.stackexchange.com/questions/199377/is-it-reasonable-to-include-a-random-slope-term-in-an-lmer-model-without-the-cor
lme1 <- lme(NEE_gC_m2~Meas_year,random=~1|Study_ID_Short,data=d5_nona) # now we have a slope under Meas_year: sink strengthents with each meas year
plot(lme1, which = 1) # residuals quite nicely flat
lme1
summary(lme1) # slope not statistically significant...


# add Meas_year as predictor, Study ID as a random intercept AND slope - we need the slope since the directions can be different
lme1 <- lme(NEE_gC_m2~Meas_year,random=~Meas_year|Study_ID_Short,data=d5_nona) # now we have a slope under Meas_year: sink strengthents with each meas year
plot(lme1, which = 1) # residuals quite nicely flat
lme1
summary(lme1) # slope not statistically significant...


# add autocorrelation structure
lme1 <- lme(NEE_gC_m2~Meas_year,random=~Meas_year|Study_ID_Short,data=d5_nona,correlation=corAR1()) # should I add Meas_year??
plot(lme1, which = 1) # residuals quite nicely flat
lme1
summary(lme1)


# finally, add an interaction between study ID and year - this is not the correct formula
lme1 <- lme(NEE_gC_m2~Meas_year,random=~1|Study_ID_Short/Meas_year,data=d5_nona,correlation=corAR1()) # should I add Meas_year??
plot(lme1, which = 1) # something strange happening here
lme1 # and the slope changed too
summary(lme1)


### Choose the final model structure, repeat for different fluxes and seasons and regions....
lme1 <- lme(NEE_gC_m2~Meas_year,random=~1|Study_ID_Short,data=subset(d5_nona, Biome=="Boreal"),correlation=corAR1()) # should I add Meas_year??
plot(lme1, which = 1) # something strange happening here
lme1 # and the slope changed too
summary(lme1)


lme1 <- lme(NEE_gC_m2~Meas_year,random=~1|Study_ID_Short,data=subset(d5_nona, Biome=="Tundra"),correlation=corAR1()) # should I add Meas_year??
plot(lme1, which = 1) # something strange happening here
lme1 # and the slope changed too
summary(lme1)

### Rasters

rasterOptions(maxmemory = 40e9, chunksize = 4e9, tmpdir="I:/Temp/") # This is crucial! It improves the efficiency.

# HUOM NYT rasterOptioneita ei annettu anneen clusterevalq:ssa

# function
chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

### annee ###
#############


all <- list.files("I:/CO2_prediction/Predictions/", pattern="annee.*.median.tif$", full.names=T)
# remove annee_all
all <- all[1:26]

# Stack rasters
r <- stack(all)
r2 <- brick(r) # more efficient


# Rasters have a different number of NA values -> Set all pixels to NA where NA is occuring in any of the rasters

beginCluster(10)

mask_layer <- clusterR(r2, calc, args=list(sum)) # , na.rm=T not accepted!

endCluster()

## Synchronize the NA values
r2_namasked <- mask(r2, mask_layer)



# ANNEE with zyp!

# example size
# r2 <- crop(r2, extent(50, 62, 68, 70))

# Create the time stamp for rasters
year <- seq(1990, 2015, by=1)


# Calculate the number of cores
no_cores <- detectCores() - 1

# Start the cluster and define number of cores
cl <- makeCluster(no_cores) # not sure if 2 is the appropriate number here?

# R opens 7 programs (nr of cores) simultaneously, but these other programs do not have the same libraries downloaded yet -> let's download them.
clusterEvalQ(cl, {
  ## set up each worker.  Could also use clusterExport()
  .libPaths("F:/UniversityComputer/ANNVIRKK/documents/Rscripts/packages")
  library(raster)
  library(zyp)
})

# These clusters do not know the data sets that are needed to calculate the trend analysis either: let them know about r2 and year
clusterExport(cl,c( "r2_namasked","year"))


# Create a function to run the trend analysis 
# i are the pixel numbers in the non-na raster
f <- function(i) {
  
  # Select the first i number of pixels from the brick
  y <- r2_namasked[i][1:26]
  
  # Calculate the trend analysis
  zyp.trend.vector(y=y, x=year, method="yuepilon")
  
  
}



# Extract the pixels that are na during all 26 years

# The number of na for each layer should now be the same
no_na <- which(!is.na(r2_namasked[[1]][]))
# no_na2 <- which(!is.na(r2_namasked[[18]][])) # same length!
counts <- chunk2(no_na, 3)
no_na1 <- counts$`1`
no_na2 <- counts$`2`
no_na3 <- counts$`3`

# Run the script in 7 R programs, in three pieces
results1 <- parLapply(cl, no_na1,  f)
results2 <- parLapply(cl, no_na2,  f)
results3 <- parLapply(cl, no_na3,  f)




# This was the original
# results <- parLapply(cl, no_na,  f)

# length(results)
# length(no_na)
# f(no_na[1000])
# results[[1]]

# Results 1
trendp1 <- sapply(results1, function(x) x[3] )
tau1 <- sapply(results1, function(x) x[5] )
sig1 <- sapply(results1, function(x) x[6] )
autocor1 <- sapply(results1, function(x) x[8] )
sen_intercept1 <- sapply(results1, function(x) x[11] )

head(tau)

# Create empty raster 
rtrendp1 <- raster(r2_namasked)
rtau1 <- raster(r2_namasked)
rsig1 <- raster(r2_namasked)
rautocor1 <- raster(r2_namasked)
rsen_intercept1 <- raster(r2_namasked)

# Add tau values 
rtrendp1[no_na1] <- trendp1
rtau1[no_na1] <- tau1
rsig1[no_na1] <- sig1
rautocor1[no_na1] <- autocor1
rsen_intercept1[no_na1] <- sen_intercept1

plot(rsig1)



# Results 2
trendp2 <- sapply(results2, function(x) x[3] )
tau2 <- sapply(results2, function(x) x[5] )
sig2 <- sapply(results2, function(x) x[6] )
autocor2 <- sapply(results2, function(x) x[8] )
sen_intercept2 <- sapply(results2, function(x) x[11] )

head(tau)

# Create empty raster 
rtrendp2 <- raster(r2_namasked)
rtau2 <- raster(r2_namasked)
rsig2 <- raster(r2_namasked)
rautocor2 <- raster(r2_namasked)
rsen_intercept2 <- raster(r2_namasked)


# Add tau values 
rtrendp2[no_na2] <- trendp2
rtau2[no_na2] <- tau2
rsig2[no_na2] <- sig2
rautocor2[no_na2] <- autocor2
rsen_intercept2[no_na2] <- sen_intercept2

plot(rsig2)


# Results 3
trendp3 <- sapply(results3, function(x) x[3] )
tau3 <- sapply(results3, function(x) x[5] )
sig3 <- sapply(results3, function(x) x[6] )
autocor3 <- sapply(results3, function(x) x[8] )
sen_intercept3 <- sapply(results3, function(x) x[11] )

head(tau)

# Create empty raster 
rtrendp3 <- raster(r2_namasked)
rtau3 <- raster(r2_namasked)
rsig3 <- raster(r2_namasked)
rautocor3 <- raster(r2_namasked)
rsen_intercept3 <- raster(r2_namasked)


# Add tau values 
rtrendp3[no_na3] <- trendp3
rtau3[no_na3] <- tau3
rsig3[no_na3] <- sig3
rautocor3[no_na3] <- autocor3
rsen_intercept3[no_na3] <- sen_intercept3

plot(rsig3)



# Merge rasters
rtrend <- merge(rtrendp1, rtrendp2, rtrendp3)
rtau <- merge(rtau1, rtau2, rtau3)
rsig <- merge(rsig1, rsig2, rsig3)
rautocor <- merge(rautocor1, rautocor2, rautocor3)
rsen_intercept <- merge(rsen_intercept1, rsen_intercept2, rsen_intercept3)

plot(rtrend)
plot(rtau)
plot(rsig)

setwd("F:/UniversityComputer/ANNVIRKK/documents/Artikkelit/MS_3_CO2_prediction/GIS/Time_series/")
writeRaster(rtrend, "annee_sen_slope_time.tif", overwrite=T)
writeRaster(rtau, "annee_tau.tif", overwrite=T)
writeRaster(rsig, "annee_tau_pval.tif", overwrite=T)
writeRaster(rautocor, "annee_autocor.tif", overwrite=T)
writeRaster(rsen_intercept, "annee_sen_slope_intercept.tif", overwrite=T)

stopCluster(cl)



