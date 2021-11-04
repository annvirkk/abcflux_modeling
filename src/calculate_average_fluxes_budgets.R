

library("dplyr")
library("tidyr")
library("ggplot2")
library("terra")

### Add flux data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv", stringsAsFactors = F)




# RANDOM VISUALIZATIONS, CAN BE REMOVED
d1 <- d %>% group_by(Study_ID_Short, Biome, Meas_year, Country) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) %>% filter(n==12)
d2 <- d1 %>% group_by(Study_ID_Short, Biome, Country) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))


theme_pub <- theme_bw() + theme(panel.border=element_rect(size=1, colour="black"),
                                axis.text=element_text(size=14, face="bold"),
                                plot.title = element_text(size = 14, face = "bold"),
                                axis.title=element_text(size=14, face="bold"),
                                plot.subtitle=element_text(size=14, face="bold", color="black"),
                                strip.text.x = element_text(size = 14, face="bold"),
                                legend.text=element_text(size=14, face="bold"), legend.title=element_text(size=14))

ggplot(d2) + geom_violin(aes(y=NEE_gC_m2, x=Biome)) + theme_pub + geom_hline(yintercept=0, color="grey") + ylab(expression(paste("NEE g C m"^{-2}, yr^{-1})))

d2$Region <- NA
d2$Region <- ifelse(d2$Country=="Canada" | d2$Country=="Alaska" | d2$Country=="USA", "North America", d2$Region)
d2$Region <- ifelse(d2$Country=="Russia" | d2$Country=="Mongolia", "Russia", d2$Region)
d2$Region <- ifelse(is.na(d2$Region), "Europe", d2$Region)

ggplot(d2) + geom_violin(aes(y=NEE_gC_m2, x=Biome)) + facet_grid(~Region) + geom_hline(yintercept=0, color="grey")  + ylab(expression(paste("NEE g C m"^{-2}, yr^{-1}))) + theme_pub

### In-situ data averages: annual and seasonal, add number of obs and sites


insitu <- NA


# Whole data

# annual cumulative
d1 <- d %>% group_by(Study_ID_Short, Meas_year) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) %>% filter(n==12)
d2 <- d1 %>% group_by(Study_ID_Short) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))
mean(d2$NEE_gC_m2, na.rm=TRUE)

insitu <- rbind(insitu, data.frame(cbind("Flux"=c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2"),
                                         "Mean"=c(mean(d2$NEE_gC_m2, na.rm=TRUE), mean(d2$GPP_gC_m2, na.rm=TRUE), mean(d2$Reco_gC_m2, na.rm=TRUE)),
                                         "SD"=c(sd(d2$NEE_gC_m2, na.rm=TRUE), sd(d2$GPP_gC_m2, na.rm=TRUE), sd(d2$Reco_gC_m2, na.rm=TRUE)),
                                         "Time"="annual", "Domain"="ABZ",
                                         "obs"=c(length(which(!is.na(d1$NEE_gC_m2))), length(which(!is.na(d1$GPP_gC_m2))), length(which(!is.na(d1$Reco_gC_m2)))),
                                         "sites"=c(length(which(!is.na(d2$NEE_gC_m2))), length(which(!is.na(d2$GPP_gC_m2))), length(which(!is.na(d2$Reco_gC_m2)))))))
insitu[] <- lapply(insitu, as.character)

# seasonal cumulative
d1 <- d %>% group_by(Study_ID_Short, Meas_year, Season) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) %>% filter(n==3)

# first means
d2 <- d1 %>% group_by(Study_ID_Short, Season) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))
mean(d2$NEE_gC_m2, na.rm=TRUE)

d3 <- d2 %>% group_by(Season) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))

d4 <- d3 %>% pivot_longer(d3, cols=NEE_gC_m2:Reco_gC_m2, names_to = "Flux", values_to = "Mean") %>% mutate("Domain"="ABZ")
names(d4)[names(d4)=="Season"] <- "Time"

# then sds
# first calculate site-level means, and then a sd out of those - or should this be done in another way???
d2 <- d1 %>% group_by(Study_ID_Short, Season) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))
mean(d2$NEE_gC_m2, na.rm=TRUE)

d3 <- d2 %>% group_by(Season) %>% summarize(NEE_gC_m2=sd(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=sd(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=sd(Reco_gC_m2, na.rm=TRUE))

d5 <- d3 %>% pivot_longer(d3, cols=NEE_gC_m2:Reco_gC_m2, names_to = "Flux", values_to = "SD") %>% mutate("Domain"="ABZ")
names(d5)[names(d5)=="Season"] <- "Time"

# finally sample sizes
obs <- d %>% group_by(Season) %>% summarize(c(length(which(!is.na(NEE_gC_m2))), length(which(!is.na(GPP_gC_m2))), length(which(!is.na(Reco_gC_m2)))))
names(obs) <- c("Season", "obs")
d4$obs <- obs$obs

sites <- d2 %>% group_by(Season) %>% summarize(c(length(which(!is.na(NEE_gC_m2))), length(which(!is.na(GPP_gC_m2))), length(which(!is.na(Reco_gC_m2)))))
names(sites) <- c("Season", "sites")
d4$sites <- sites$sites

# add sd
d4$SD <- d5$SD


d4 <- subset(d4, select=c(Flux, Mean, SD, Time, Domain, obs, sites))

d4[] <- lapply(d4, as.character)

insitu <- rbind(insitu, data.frame(d4))




# For the remaining splits, we create a function and apply that


avg_category <- function(d, category) {


  insitu <- NA

  #


  # annual cumulative
  d1 <- d %>% group_by(Study_ID_Short, Meas_year, .data[[category]]) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) %>% filter(n==12)

  # first means
  d2 <- d1 %>% group_by(Study_ID_Short, .data[[category]]) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))
  mean(d2$NEE_gC_m2, na.rm=TRUE)

  d3 <- d2 %>% group_by(.data[[category]]) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))

  d4 <- d3 %>% pivot_longer(d3, cols=NEE_gC_m2:Reco_gC_m2, names_to = "Flux", values_to = "Mean")


  # then sds
  # first calculate site-level means, and then a sd out of those - or should this be done in another way???
  d2 <- d1 %>% group_by(Study_ID_Short, .data[[category]]) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))
  mean(d2$NEE_gC_m2, na.rm=TRUE)

  d3 <- d2 %>% group_by(.data[[category]]) %>% summarize(NEE_gC_m2=sd(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=sd(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=sd(Reco_gC_m2, na.rm=TRUE))

  d5 <- d3 %>% pivot_longer(d3, cols=NEE_gC_m2:Reco_gC_m2, names_to = "Flux", values_to = "SD")


  # finally sample sizes
  obs <- d %>% group_by(.data[[category]]) %>% summarize(c(length(which(!is.na(NEE_gC_m2))), length(which(!is.na(GPP_gC_m2))), length(which(!is.na(Reco_gC_m2)))))
  #names(obs) <- c("category", "obs")
  #d4$obs <- obs$obs
  names(obs) <- c(category, "obs")
  obs$Flux <- rep(c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2"), nrow(unique(obs[, category])))
  d4 <- merge(d4, obs, by=c(category, "Flux"))


  sites <- d2 %>% group_by(.data[[category]]) %>% summarize(c(length(which(!is.na(NEE_gC_m2))), length(which(!is.na(GPP_gC_m2))), length(which(!is.na(Reco_gC_m2)))))
  # names(sites) <- c("category", "sites")
  # d4$sites <- sites$sites
  names(sites) <- c(category, "sites")
  sites$Flux <- rep(c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2"), nrow(unique(sites[, category])))
  d4 <- merge(d4, sites, by=c(category, "Flux"))

  # add sd
  d4$SD <- d5$SD

  # change column names
  d4$Time <- "annual"

  #d4 <- subset(d4, select=c(Flux, Mean, SD, Time, obs, sites)) #Domain,
  d4 <- subset(d4, select=c("Flux", "Mean", "SD", category, "Time", "obs", "sites"))

  d4[] <- lapply(d4, as.character)

  insitu <- rbind(insitu, data.frame(d4))




  # seasonal cumulative
  d1 <- d %>% group_by(Study_ID_Short, Meas_year, .data[[category]], Season) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) %>% filter(n==3)

  # first means
  d2 <- d1 %>% group_by(Study_ID_Short, .data[[category]], Season) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))
  mean(d2$NEE_gC_m2, na.rm=TRUE)

  d3 <- d2 %>% group_by(Season, .data[[category]]) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))

  d4 <- d3 %>% pivot_longer(d3, cols=NEE_gC_m2:Reco_gC_m2, names_to = "Flux", values_to = "Mean")
  names(d4)[names(d4)=="Season"] <- "Time"

  # then sds
  # first calculate site-level means, and then a sd out of those - or should this be done in another way???
  d2 <- d1 %>% group_by(Study_ID_Short, Season, .data[[category]]) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))
  mean(d2$NEE_gC_m2, na.rm=TRUE)

  d3 <- d2 %>% group_by(Season, .data[[category]]) %>% summarize(NEE_gC_m2=sd(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=sd(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=sd(Reco_gC_m2, na.rm=TRUE))

  d5 <- d3 %>% pivot_longer(d3, cols=NEE_gC_m2:Reco_gC_m2, names_to = "Flux", values_to = "SD") %>% mutate("Domain"="ABZ")
  names(d5)[names(d5)=="Season"] <- "Time"

  # finally sample sizes
  obs <- d %>% group_by(Season, .data[[category]]) %>% summarize(c(length(which(!is.na(NEE_gC_m2))), length(which(!is.na(GPP_gC_m2))), length(which(!is.na(Reco_gC_m2)))))
  # names(obs) <- c("Season", "category", "obs")
  # d4$obs <- obs$obs
  names(obs) <- c("Time", category, "obs")
  obs$Flux <- rep(c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2"), nrow(obs)/3 ) # 4 seasons: nrow(unique(obs[, category]))*4 original... TÄSSÄ VIRHE koska jotkut seasonit puuttuu joistain kasv tyyp luokista!!!
  d4 <- merge(d4, obs, by=c(category, "Time", "Flux"))


  sites <- d2 %>% group_by(Season, .data[[category]]) %>% summarize(c(length(which(!is.na(NEE_gC_m2))), length(which(!is.na(GPP_gC_m2))), length(which(!is.na(Reco_gC_m2)))))
  # names(sites) <- c("Season", "category",  "sites")
  # d4$sites <- sites$sites
  names(sites) <- c("Time", category,  "sites")
  sites$Flux <- rep(c("NEE_gC_m2", "GPP_gC_m2", "Reco_gC_m2"), nrow(sites)/3)
  d4 <- merge(d4, sites, by=c(category, "Time", "Flux"))


  # add sd
  d4$SD <- d5$SD

  d4[] <- lapply(d4, as.character)

  d4 <- subset(d4, select=c("Flux", "Mean", "SD", category, "Time", "obs", "sites"))

  insitu <- rbind(insitu, data.frame(d4))



}


### Zonal statistics - note that these are first done for the site-level classifications (if available) and then the gridded data (which will likely be used in the paper!)


# Biomes
biome1 <- avg_category(d, "Biome")
biome <- avg_category(d, "BIOME_NAME_Ecoregions") # some NA's - need to add back!


# Biome - continent
d$Biome_continent <- paste(d$Biome, d$REALM_Ecoregions)
Biome_continent1 <- avg_category(d, "Biome_continent")

# Permafrost types
permafrost1 <- avg_category(d, "Permafrost")
permafrost <- avg_category(d, "EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH")

d$EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH_simple <- ifelse(d$EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH!="No", "Yes", d$EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH)
permafrost_simple <- avg_category(d, "EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH_simple")



# Biomes + permafrost types
d$Biome_perma1 <- paste(d$Biome, d$Permafrost, sep="_")
d$Biome_perma <- paste(d$Biome, d$EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH, sep="_")
d$Biome_permasimple <- paste(d$Biome, d$EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH_simple, sep="_")

biomeperma1 <- avg_category(d, "Biome_perma1")
biomeperma <- avg_category(d, "Biome_perma")
Biome_permasimple <- avg_category(d, "Biome_permasimple")

# Region/country
d$Region <- NA
d$Region <- ifelse(d$Country=="Canada" | d$Country=="Alaska" | d$Country=="USA", "North America", d$Region)
d$Region <- ifelse(d$Country=="Russia" | d$Country=="Mongolia", "Russia", d$Region)
d$Region <- ifelse(is.na(d$Region), "Europe", d$Region)

region <- avg_category(d, "Region")

# Region + permafrost type
d$Region_perma1 <- paste(d$Region, d$Permafrost, sep="_")
d$Region_perma <- paste(d$Region, d$EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH, sep="_")
d$Region_permasimple <- paste(d$Region, d$EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH_simple, sep="_")

regionperma1 <- avg_category(d, "Region_perma1")
regionperma <- avg_category(d, "Region_perma")
region_permasimple <- avg_category(d, "Region_permasimple")


# Vegetation types
veg1 <- avg_category(d, "Veg_type_Short")
veg <- avg_category(d, "ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged")

# Vegetation types + Permafrost types
d$Veg_perma1 <- paste(d$Veg_type_Short, d$Permafrost, sep="_")
d$Veg_perma <- paste(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, d$EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH, sep="_")
d$Veg_permasimple <- paste(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, d$EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH_simple, sep="_")

vegperma1 <- avg_category(d, "Veg_perma1")
vegperma <- avg_category(d, "Veg_perma")
veg_permasimple <- avg_category(d, "Veg_permasimple")

# Region + vegetatoin type + permafrost type
d$Veg_permasimple_region1 <- paste(d$Veg_type_Short, d$Permafrost, d$Region, sep="_")
veg_permasimple_region1 <- avg_category(d, "Veg_permasimple_region1")

d$Veg_permasimple_region <- paste(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged, d$EXTENT_UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH_simple, d$Region, sep="_")
veg_permasimple_region <- avg_category(d, "Veg_permasimple_region")

# Region + vegetation type
d$Veg_region1 <- paste(d$Veg_type_Short,  d$Region, sep="_")
veg_region1 <- avg_category(d, "Veg_region1")

d$Veg_region <- paste(d$ESACCI_cavm_general_ESAwaterfix_broadevfix_mixfix_cropfix_nowaterglacier_ESACCI_CAVM_merged,  d$Region, sep="_")
veg_region <- avg_category(d, "Veg_region")





### Proportion of sink vs. source vs. neutral observations


prop_category <- function(d, category) {

  
  
  # annual cumulative
  d1 <- d %>% group_by(Study_ID_Short, Meas_year, .data[[category]]) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) %>% filter(n==12)
  
  # first means
  d2 <- d1 %>% group_by(Study_ID_Short, .data[[category]]) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))

  d3 <- d2 %>% group_by(.data[[category]]) %>% summarize(sink=length(which(NEE_gC_m2< -5)), source=length(which(NEE_gC_m2>5)), n=n()) %>% mutate(sink_prop = sink/n, source_prop = source/n, neutral_prop=(n-(sink+source))/n)
  
  
  
}

biomeprop1 <- prop_category(d, "Biome")

biomecontinentprop1 <- prop_category(d, "Biome_continent")



### Load rasters and calculate average

# as a test case we'll use the simplemodel
# fluxes need to be divided by 1000


# list rasters 


terraOptions(memfrac=0.9, tempdir = "/mnt/data1/boreal/avirkkala/Temp") # testing a different style


# loop through years
years <- seq(2001, 2020, by=1)

rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km", pattern="simpletestmodel2.tif$", full.names=TRUE)

for (y in years) {

  # y <- 2001
  i <- "NEE_gC_m2"
  km <- "1km"
  m <- "gbm"

  for (c in c(2, 4)) { ## TEMPORARY SKIP FOR TWO!!!!

    # c <- 1

    library("stringi")
    r1 <- rasters[stri_detect_fixed(rasters,c(y)) & stri_detect_fixed(rasters,paste0("_", c, "_"))]
    r1 <- rast(r1)
    print(r1)

    # annual
    r1sum <- sum(r1)
    r1rangetemp <- range(r1)
    r1range <- abs(r1rangetemp[[1]]- r1rangetemp[[2]])
    writeRaster(r1sum, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/prep/", paste(i,"annual_sum",  km, m, y, c, "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
    writeRaster(r1range, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/prep/", paste(i,"annual_range",  km, m, y, c, "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
    rm(r1sum); rm(r1rangetemp); rm(r1range)


    # gs
    r1sum <- sum(r1[[5:9]])
    r1rangetemp <- range(r1[[5:9]])
    r1range <- abs(r1rangetemp[[1]]- r1rangetemp[[2]])
    writeRaster(r1sum, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/prep/", paste(i,"gs_sum",  km, m, y, c, "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
    writeRaster(r1range, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/prep/", paste(i,"gs_range",  km, m, y, c, "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
    rm(r1sum); rm(r1rangetemp); rm(r1range)


    # ngs
    r1sum <- sum(r1[[c(1:4, 10:12)]])
    r1rangetemp <- range(r1[[c(1:4, 10:12)]])
    r1range <- abs(r1rangetemp[[1]]- r1rangetemp[[2]])
    writeRaster(r1sum, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/prep/", paste(i,"ngs_sum",  km, m, y, c, "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
    writeRaster(r1range, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/prep/", paste(i,"ngs_range",  km, m, y, c, "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
    rm(r1sum); rm(r1rangetemp); rm(r1range); rm(r1)

    gc()



  }





}




### mosaic

# mosaicking files in R using terra has memory issues, so using gdal instead: https://stackoverflow.com/questions/15876591/merging-multiple-rasters-in-r
library(gdalUtils)
library(rgdal)


rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/prep", pattern=".tif$", full.names=TRUE)

for (y in years) {

  # y <- 2001
  i <- "NEE_gC_m2"
  km <- "1km"
  m <- "gbm"

  library("stringi")
  # annual
  r1 <- rasters[stri_detect_fixed(rasters,c(y)) & stri_detect_fixed(rasters,"annual_sum")]
  mosaic_rasters(gdalfile=r1,dst_dataset=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/", paste(i,"annual_sum",  km, m, y,  "loocv", sep="_"), ".tif"),of="GTiff")


  # ngs
  r1 <- rasters[stri_detect_fixed(rasters,c(y)) & stri_detect_fixed(rasters,"ngs_sum")]
  mosaic_rasters(gdalfile=r1,dst_dataset=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/", paste(i,"ngs_sum",  km, m, y,  "loocv", sep="_"), ".tif"),of="GTiff")

  # gs
  r1 <- rasters[stri_detect_fixed(rasters,c(y)) & stri_detect_fixed(rasters,"_gs_sum")]
  mosaic_rasters(gdalfile=r1,dst_dataset=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/", paste(i,"gs_sum",  km, m, y,  "loocv", sep="_"), ".tif"),of="GTiff")


}







#### LASKE 2001-2020 KESKIARVO
rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg", pattern=".tif$", full.names=TRUE)
rasters <- rasters[!(stri_detect_fixed(rasters,"avg_"))]

r1 <- rast(rasters[stri_detect_fixed(rasters,"annual_sum")])
r1mean <- mean(r1, na.rm=TRUE)
writeRaster(r1mean, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/", paste(i,"annual_sum_avg",  km, m,  "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
rm(r1); rm(r1mean)
gc()

r1 <- rast(rasters[stri_detect_fixed(rasters,"_gs_sum")])
r1mean <- mean(r1, na.rm=TRUE)
writeRaster(r1mean, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/", paste(i,"gs_sum_avg",  km, m,  "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
rm(r1); rm(r1mean)
gc()

r1 <- rast(rasters[stri_detect_fixed(rasters,"ngs_sum")])
r1mean <- mean(r1, na.rm=TRUE)
writeRaster(r1mean, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/", paste(i,"ngs_sum_avg",  km, m,  "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
rm(r1); rm(r1mean)
gc()



#### calculate trend too
library("zyp", lib.loc="/mnt/data1/boreal/avirkkala/packages") 
library("stringi")
library("terra", lib.loc="/mnt/data1/boreal/avirkkala/packages")

i <- "NEE_gC_m2"
km <- "1km"
m <- "gbm"


rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/prep", pattern=".tif$", full.names=TRUE)
library("stringi")


for (c in c(2, 4)) { ## TEMPORARY SKIP FOR TWO!!!!
  
  # c <- 1

  
  ### annual
  
  r1 <- rasters[stri_detect_fixed(rasters,"annual_sum") & stri_detect_fixed(rasters,paste0("_", c, "_"))]
  r1 <- rast(r1[5:20])
  print(r1)
  r1_df <- terra::as.data.frame(r1, xy=TRUE)
  r1_df <- na.omit(r1_df)
  str(r1_df)

  
  print("calculating trend")
  trend <- zyp.trend.dataframe(r1_df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  print("trend calculated")
  
  trendval <- as.matrix(cbind(r1_df$x, r1_df$y, round(trend$trend)))
  write.csv(trendval, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"annual_sum_trend",  km, m, c,  "loocv", sep="_"), ".csv"), row.names=FALSE)
  
  
  # ## NOT WORKING!!!
  # print(trendval)
  # str(trendval)
  #trendval <- as.data.frame(cbind(r1_df$x, r1_df$y, trend$trend))
  
  # # rtrend <- terra::rast(trendval, crs=crs(r1), type="xyz")
  #  # trend the Sen’s slope (trend) per unit time. Note that there's another column: trendp the Sen’s slope (trend) over the time period.
  # # crs="+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
  # writeRaster(rtrend, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"annual_sum_trend",  km, m, c,  "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
  # 
  
  
  
  # trendval <- as.matrix(cbind(tmean_df1$x, tmean_df1$y, trend$trend))
  # rtrend <- terra::rast(trendval, crs=crs(tmean), type="xyz") 
  
  
  rm(r1); rm(r1_df); rm(trend); rm(trendval)#; rm(rtrend)
  gc()
  print("first trend analysis and raster output written")
  
  
  
  ### gs
  
  r1 <- rasters[stri_detect_fixed(rasters,"_gs_sum")& stri_detect_fixed(rasters,paste0("_", c, "_"))]
  r1 <- rast(r1[5:20])
  print(r1)
  r1_df <- terra::as.data.frame(r1, xy=TRUE)
  str(r1_df)
  r1_df <- na.omit(r1_df)
  
  
  trend <- zyp.trend.dataframe(r1_df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  
  trendval <- as.matrix(cbind(r1_df$x, r1_df$y, round(trend$trend)))
  write.csv(trendval, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"gs_sum_trend",  km, m, c,  "loocv", sep="_"), ".csv"), row.names=FALSE)
  
  # rtrend <- terra::rast(trendval, crs=crs(r1), type="xyz") # trend the Sen’s slope (trend) per unit time. Note that there's another column: trendp the Sen’s slope (trend) over the time period.
  # 
  # writeRaster(rtrend, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"gs_sum_trend",  km, m, c,  "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
  # 
  
  rm(r1); rm(r1_df); rm(trend); rm(trendval)#; rm(rtrend)
  gc()
  
  
  
  
  ### ngs
  
  r1 <- rasters[stri_detect_fixed(rasters,"ngs_sum")& stri_detect_fixed(rasters,paste0("_", c, "_"))]
  r1 <- rast(r1[5:20])
  print(r1)
  
  r1_df <- terra::as.data.frame(r1, xy=TRUE)
  str(r1_df)
  r1_df <- na.omit(r1_df)
  
  
  trend <- zyp.trend.dataframe(r1_df, metadata.cols=2, method="yuepilon",
                               conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  
  
  trendval <- as.matrix(cbind(r1_df$x, r1_df$y, round(trend$trend)))
  write.csv(trendval, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"ngs_sum_trend",  km, m, c,  "loocv", sep="_"), ".csv"), row.names=FALSE)
  
  # rtrend <- terra::rast(trendval, crs=crs(r1), type="xyz") # trend the Sen’s slope (trend) per unit time. Note that there's another column: trendp the Sen’s slope (trend) over the time period.
  # 
  # writeRaster(rtrend, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"ngs_sum_trend",  km, m, c,  "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
  # 
  
  rm(r1); rm(r1_df); rm(trend); rm(trendval)#; rm(rtrend)
  gc()
  
}




### WRITING THE OUTPUT TO RASTER - has to be done on Rstudio server because for some reason the script doesnt work in Kubernetes
i <- "NEE_gC_m2"
km <- "1km"
m <- "gbm"
c <- 1


rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/prep", pattern=".tif$", full.names=TRUE)
library("stringi")

r1 <- rasters[stri_detect_fixed(rasters,"annual_sum") & stri_detect_fixed(rasters,paste0("_", c, "_"))]
r1 <- rast(r1[5])

for (c in 1:4) {
  
  print(c)
  d <- read.csv(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"annual_sum_trend",  km, m, c,  "loocv", sep="_"), ".csv"))
  d <- as.matrix(d)
  rtrend <- terra::rast(d, crs=crs(r1), type="xyz")
  print("rasterized")
  writeRaster(rtrend, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"annual_sum_trend",  km, m, c,  "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
  print("written")
  rm(d); rm(rtrend); gc()
  print("annual sum done")
  
  d <- read.csv(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"gs_sum_trend",  km, m, c,  "loocv", sep="_"), ".csv"))
  d <- as.matrix(d)
  rtrend <- terra::rast(d, crs=crs(r1), type="xyz")
  writeRaster(rtrend, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"gs_sum_trend",  km, m, c,  "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
  rm(d); rm(rtrend); gc()
  print("gs done")
  
  d <- read.csv(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"ngs_sum_trend",  km, m, c,  "loocv", sep="_"), ".csv"))
  d <- as.matrix(d)
  rtrend <- terra::rast(d, crs=crs(r1), type="xyz")
  writeRaster(rtrend, paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep/", paste(i,"ngs_sum_trend",  km, m, c,  "loocv", sep="_"), ".tif"), overwrite=TRUE, datatype='INT4S')
  rm(d); rm(rtrend); gc()
  
}




### mosaickin trend rasters

# mosaicking files in R using terra has memory issues, so using gdal instead: https://stackoverflow.com/questions/15876591/merging-multiple-rasters-in-r
library(gdalUtils)
library(rgdal)
library("stringi")


rasters <- list.files("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/prep", pattern=".tif$", full.names=TRUE)


i <- "NEE_gC_m2"
km <- "1km"
m <- "gbm"

library("stringi")
# annual
r1 <- rasters[stri_detect_fixed(rasters,"annual_sum")]
mosaic_rasters(gdalfile=r1,dst_dataset=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/", paste(i,"annual_sum",  km, m,   "loocv", sep="_"), ".tif"),of="GTiff")


# ngs
r1 <- rasters[stri_detect_fixed(rasters,"ngs_sum")]
mosaic_rasters(gdalfile=r1,dst_dataset=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/", paste(i,"ngs_sum",  km, m,  "loocv", sep="_"), ".tif"),of="GTiff")

# gs
r1 <- rasters[stri_detect_fixed(rasters,"_gs_sum")]
mosaic_rasters(gdalfile=r1,dst_dataset=paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/", paste(i,"gs_sum",  km, m,  "loocv", sep="_"), ".tif"),of="GTiff")







#### statistics
i <- "NEE_gC_m2"
km <- "1km"
m <- "gbm"
# calculate budget for the boreal and tundra
r <- rast(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/", paste(i,"annual_sum_avg",  km, m,  "loocv", sep="_"), ".tif"))
r <- r*(xres(r)*yres(r)) # g C yr-1
r <- r/1000
sumr <- global(r, fun="sum", na.rm=TRUE) 
sumr3 <- sumr/1000000000000 # tg c yr-1


biome <- rast("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/Ecoregions2017_biomenum_tundraboreal_northpolelambert1km.tif")

biome <- shapefile("/mnt/data1/boreal/avirkkala/abcflux_modeling/masking_summary_rasters/Ecoregions2017_tundraboreal_dissolved_northpolelambert.shpp")

biome <- rast("/mnt/data1/boreal/avirkkala/study_domains/Ecoregions2017_biomenum_tundraboreal.tif")

biome2 <- vect("/mnt/data1/boreal/avirkkala/study_domains/Ecoregions2017_tundraboreal.shp")

biome2 <- biome2[, 3]

biome3 <- aggregate(biome2, "BIOME_NUM")

biome3 <- terra::project(biome3, "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")

b <- terra::extract(r, biome3, fun="sum",  na.rm=TRUE)

b$mean/1000000000000



# calculate proportion of source pixels in the tundra
tundra <- vect("/mnt/data1/boreal/avirkkala/study_domains/Ecoregions2017_tundra.shp")
tundra <- aggregate(tundra, "BIOME_NUM")
tundra <- terra::project(tundra, "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")
tundrar <- rasterize(tundra, r)
r2 <- mask(r, tundrar)
vals <- values(r2)

vals2 <- vals[!is.na(vals)]

hist(vals2)

length(vals2[vals2< -5])/length(vals2)

length(vals2[vals2> 5])/length(vals2)



# boreal
tundra <- vect("/mnt/data1/boreal/avirkkala/study_domains/Ecoregions2017_boreal.shp")
tundra <- aggregate(tundra, "BIOME_NUM")
tundra <- terra::project(tundra, "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")
tundrar <- rasterize(tundra, r)
r2 <- mask(r, tundrar)
vals <- values(r2)

vals2 <- vals[!is.na(vals)]

hist(vals2)

length(vals2[vals2< -5])/length(vals2)

length(vals2[vals2> 5])/length(vals2)




# trends?

t <- rast(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_trend/", paste(i,"annual_sum",  km, m,   "loocv", sep="_"), ".tif"))


biome2 <- vect("/mnt/data1/boreal/avirkkala/study_domains/Ecoregions2017_tundraboreal.shp")

biome3 <- biome2[, 3]

biome3 <- aggregate(biome3, "BIOME_NUM")

biome3 <- terra::project(biome3, "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")

b <- terra::extract(t, biome3, fun="mean",  na.rm=TRUE)





vals <- values(t)
vals2 <- vals[!is.na(vals)]
length(vals2[vals2<0])/length(vals2)


# calculate avg predicted flux
r <- rast(paste0("/mnt/data1/boreal/avirkkala/abcflux_modeling/predictions_1km_avg/", paste(i,"annual_sum_avg",  km, m,  "loocv", sep="_"), ".tif"))
r <- r/1000

biome2 <- vect("/mnt/data1/boreal/avirkkala/study_domains/Ecoregions2017_tundraboreal.shp")

biome3 <- biome2[, 3]

biome3 <- aggregate(biome3, "BIOME_NUM")

biome3 <- terra::project(biome3, "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")

b <- terra::extract(r, biome3, fun="mean",  na.rm=TRUE)


biome2$Biome_continent <- paste(biome2$BIOME_NUM, biome2$REALM)

biome3 <- biome2[, 12]

biome3 <- aggregate(biome3, "Biome_continent")

biome3 <- terra::project(biome3, "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")

b <- terra::extract(r, biome3, fun="mean",  na.rm=TRUE)




# proportion of sink vs neutral vs source pixels

sinksource <- function(x) {
  
}

# ### ORIGINAL GROUPING WITHOUT FUNCTION
# #### ORIGINAL
# insitu <- NA
# 
# 
# # annual cumulative
# d1 <- d %>% group_by(Study_ID_Short, Meas_year, Biome) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) %>% filter(n==12)
# 
# # first means
# d2 <- d1 %>% group_by(Study_ID_Short, Biome) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE)) 
# mean(d2$NEE_gC_m2, na.rm=TRUE) 
# 
# d3 <- d2 %>% group_by(Biome) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))
# 
# d4 <- d3 %>% pivot_longer(d3, cols=NEE_gC_m2:Reco_gC_m2, names_to = "Flux", values_to = "Mean") 
# 
# 
# # then sds
# # first calculate site-level means, and then a sd out of those - or should this be done in another way???
# d2 <- d1 %>% group_by(Study_ID_Short, Biome) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE)) 
# mean(d2$NEE_gC_m2, na.rm=TRUE) 
# 
# d3 <- d2 %>% group_by(Biome) %>% summarize(NEE_gC_m2=sd(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=sd(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=sd(Reco_gC_m2, na.rm=TRUE))
# 
# d5 <- d3 %>% pivot_longer(d3, cols=NEE_gC_m2:Reco_gC_m2, names_to = "Flux", values_to = "SD") 
# 
# 
# # finally sample sizes
# obs <- d %>% group_by(Biome) %>% summarize(c(length(which(!is.na(NEE_gC_m2))), length(which(!is.na(GPP_gC_m2))), length(which(!is.na(Reco_gC_m2))))) 
# names(obs) <- c("Biome", "obs")
# d4$obs <- obs$obs
# 
# sites <- d2 %>% group_by(Biome) %>% summarize(c(length(which(!is.na(NEE_gC_m2))), length(which(!is.na(GPP_gC_m2))), length(which(!is.na(Reco_gC_m2))))) 
# names(sites) <- c("Biome", "sites")
# d4$sites <- sites$sites
# 
# # add sd
# d4$SD <- d5$SD
# 
# # change column names
# d4$Domain=d4$Biome
# d4$Time <- "annual"
# 
# d4 <- subset(d4, select=c(Flux, Mean, SD, Time, Domain, obs, sites))
# 
# d4[] <- lapply(d4, as.character)
# 
# insitu <- rbind(insitu, data.frame(d4))
# 
# 
# 
# 
# # seasonal cumulative
# d1 <- d %>% group_by(Study_ID_Short, Meas_year, Biome, Season) %>% summarize(n=n(), NEE_gC_m2=sum(NEE_gC_m2), GPP_gC_m2=sum(GPP_gC_m2), Reco_gC_m2=sum(Reco_gC_m2)) %>% filter(n==3)
# 
# # first means
# d2 <- d1 %>% group_by(Study_ID_Short, Biome, Season) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE)) 
# mean(d2$NEE_gC_m2, na.rm=TRUE) 
# 
# d3 <- d2 %>% group_by(Season, Biome) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE))
# 
# d4 <- d3 %>% pivot_longer(d3, cols=NEE_gC_m2:Reco_gC_m2, names_to = "Flux", values_to = "Mean") 
# names(d4)[names(d4)=="Season"] <- "Time"
# 
# # then sds
# # first calculate site-level means, and then a sd out of those - or should this be done in another way???
# d2 <- d1 %>% group_by(Study_ID_Short, Season, Biome) %>% summarize(NEE_gC_m2=mean(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=mean(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=mean(Reco_gC_m2, na.rm=TRUE)) 
# mean(d2$NEE_gC_m2, na.rm=TRUE) 
# 
# d3 <- d2 %>% group_by(Season, Biome) %>% summarize(NEE_gC_m2=sd(NEE_gC_m2, na.rm=TRUE), GPP_gC_m2=sd(GPP_gC_m2, na.rm=TRUE), Reco_gC_m2=sd(Reco_gC_m2, na.rm=TRUE))
# 
# d5 <- d3 %>% pivot_longer(d3, cols=NEE_gC_m2:Reco_gC_m2, names_to = "Flux", values_to = "SD") %>% mutate("Domain"="ABZ")
# names(d5)[names(d5)=="Season"] <- "Time"
# 
# # finally sample sizes
# obs <- d %>% group_by(Season, Biome) %>% summarize(c(length(which(!is.na(NEE_gC_m2))), length(which(!is.na(GPP_gC_m2))), length(which(!is.na(Reco_gC_m2))))) 
# names(obs) <- c("Season", "Biome", "obs")
# d4$obs <- obs$obs
# 
# sites <- d2 %>% group_by(Season, Biome) %>% summarize(c(length(which(!is.na(NEE_gC_m2))), length(which(!is.na(GPP_gC_m2))), length(which(!is.na(Reco_gC_m2))))) 
# names(sites) <- c("Season", "Biome",  "sites")
# d4$sites <- sites$sites
# 
# # add sd
# d4$SD <- d5$SD
# 
# d4[] <- lapply(d4, as.character)
# 
# # column modifications
# d4$Domain <- d4$Biome
# 
# d4 <- subset(d4, select=c(Flux, Mean, SD, Time, Domain, obs, sites))
# 
# 
# insitu <- rbind(insitu, data.frame(d4))
# 
# 
# 
