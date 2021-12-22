

library("dplyr")
library("tidyr")
library("ggplot2")
library("terra")

### Add flux data
setwd("/mnt/data1/boreal/avirkkala/repos/flux_upscaling_data/src/")
d <- read.csv("../results/final/modeldata_avg.csv", stringsAsFactors = F)




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


