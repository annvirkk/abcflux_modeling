
# setwd("/home/master")
# gcs_get_object("flux_upscaling_data/results/final/modeldata_avg.csv", saveToDisk = "flux_upscaling_data/results/final/modeldata_avg.csv", overwrite=TRUE)

#Nov 8th 2022 noticed some GPP outliers in January from N Finland


d <- read.csv("/home/master/flux_upscaling_data/results/final/modeldata_avg.csv")

#write.csv(d, "/home/master/flux_upscaling_data/results/final/modeldata_avg_outliers_still_included.csv")


k <- subset(d, Country=="Finland")

k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Notes_SiteInfo, Notes_TimeVariant))


k <- subset(d, Interval==1)

k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Notes_SiteInfo, Notes_TimeVariant))

k <- subset(d, Interval==2)

k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Notes_SiteInfo, Notes_TimeVariant))

k <- subset(d, Interval==3)

k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Notes_SiteInfo, Notes_TimeVariant))

k <- subset(d, Interval==4)

k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Notes_SiteInfo, Notes_TimeVariant))


k <- subset(d, Interval==7)

k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Notes_SiteInfo, Notes_TimeVariant))


# to be removed: Lund KObbefjord

# Old Canadian sites - ok to remove these as there are many NS towers near each other

k <- subset(d, startsWith(Study_ID_Short, "Goulden_CA-NS2")) # high winter uptake
k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Notes_SiteInfo, Notes_TimeVariant))

k$Start_date <- as.Date(k$Start_date)

# calculate all days
all_days = data.frame(Start_date = seq.Date(from = min(k$Start_date), to = max(k$Start_date), by = "month"))
# join to original data
library(dplyr)
dd_complete = left_join(all_days, k, by = "Start_date")

# ggplot won't connect lines across missing values
print(ggplot(dd_complete, aes(Start_date, GPP_gC_m2)) +
  geom_point() +
  geom_line())


k <- subset(d, startsWith(Study_ID_Short, "Goulden_CA-NS3")) # high winter uptake - this does not seem so problematic after all?
k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Notes_SiteInfo, Notes_TimeVariant))


k$Start_date <- as.Date(k$Start_date)

# calculate all days
all_days = data.frame(Start_date = seq.Date(from = min(k$Start_date), to = max(k$Start_date), by = "month"))
# join to original data
library(dplyr)
dd_complete = left_join(all_days, k, by = "Start_date")

# ggplot won't connect lines across missing values
print(ggplot(dd_complete, aes(Start_date, GPP_gC_m2)) +
  geom_point() +
  geom_line())

k <- subset(d, startsWith(Study_ID_Short, "McCaughey_CA-Man")) # high winter uptake
k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Gap_perc, Tower_QA.QC.NEE.flag, Notes_SiteInfo, Notes_TimeVariant)) # nothing to suggest any particular issues at the site

k$Start_date <- as.Date(k$Start_date)

# calculate all days
all_days = data.frame(Start_date = seq.Date(from = min(k$Start_date), to = max(k$Start_date), by = "month"))
# join to original data
library(dplyr)
dd_complete = left_join(all_days, k, by = "Start_date")

# ggplot won't connect lines across missing values
ggplot(dd_complete, aes(Start_date, GPP_gC_m2)) +
  geom_point() +
  geom_line()



# Loop through all the EC sites to look at GPP time series

d <- subset(d, Flux_method=="EC")
sites <- unique(d$Study_ID_Short)
sites[sites=="L\xf3pez-Blanco_GL-NuF_tower1"] <- "Lopez-Blanco_GL-NuF_tower1"
sites[sites=="L\xf3pez-Blanco_GL-ZaF_tower1"] <- "Lopez-Blanco_GL-Zaf_tower1"


for (i in sites) {
  
  #i <- "Harazano_US-Cms_tower1"
  k <- subset(d, startsWith(Study_ID_Short, i)) # high winter uptake
  k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, Start_date, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Gap_perc, Tower_QA.QC.NEE.flag, Notes_SiteInfo, Notes_TimeVariant)) # nothing to suggest any particular issues at the site
  
  if(any(!(is.na(k2$GPP_gC_m2))) & nrow(k2)>12) {
    
    k2$Start_date <- as.Date(k2$Start_date)
    
    # calculate all days
    all_days = data.frame(Start_date = seq.Date(from = min(k2$Start_date), to = max(k2$Start_date), by = "month"))
    # join to original data
    dd_complete = left_join(all_days, k2, by = "Start_date")
    
    # ggplot won't connect lines across missing values
    print(ggplot(dd_complete, aes(Start_date, GPP_gC_m2)) +
            geom_point() +
            geom_line() + ggtitle(i)) 
  }
  
  
  # # plot also temperatures?
  # 
  # k <- subset(d, startsWith(Study_ID_Short, i)) # high winter uptake
  # k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, Start_date, tmean_terraclimate_sites)) # nothing to suggest any particular issues at the site
  # 
  # if(any(!(is.na(k2$tmean_terraclimate_sites))) & nrow(k2)>12) {
  #   
  #   k2$Start_date <- as.Date(k2$Start_date)
  #   
  #   # calculate all days
  #   all_days = data.frame(Start_date = seq.Date(from = min(k2$Start_date), to = max(k2$Start_date), by = "month"))
  #   # join to original data
  #   dd_complete = left_join(all_days, k2, by = "Start_date")
  #   
  #   # ggplot won't connect lines across missing values
  #   print(ggplot(dd_complete, aes(Start_date, tmean_terraclimate_sites)) +
  #           geom_point() +
  #           geom_line() + ggtitle(i)) 
  # }
  # 
  # 
  # 
  # # plot also ndvi?
  # 
  # k <- subset(d, startsWith(Study_ID_Short, i)) # high winter uptake
  # k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, Start_date, NDVI_whittaker_constant_monthly_mean)) # nothing to suggest any particular issues at the site
  # 
  # if(any(!(is.na(k2$NDVI_whittaker_constant_monthly_mean))) & nrow(k2)>12) {
  #   
  #   k2$Start_date <- as.Date(k2$Start_date)
  #   
  #   # calculate all days
  #   all_days = data.frame(Start_date = seq.Date(from = min(k2$Start_date), to = max(k2$Start_date), by = "month"))
  #   # join to original data
  #   dd_complete = left_join(all_days, k2, by = "Start_date")
  #   
  #   # ggplot won't connect lines across missing values
  #   print(ggplot(dd_complete, aes(Start_date, NDVI_whittaker_constant_monthly_mean)) +
  #           geom_point() +
  #           geom_line() + ggtitle(i)) 
  # }
  # 
  # 
  
  
  
}


# check Shaver_US-ICh_tower1, Mkhabela_CA-SF2_tower2, Lund_DK-ZaH_tower1, Lindroth_SE-Nor_tower1, Strachan_CA-LCC_tower1




i <- "Lindroth_SE-Nor_tower1"
k <- subset(d, startsWith(Study_ID_Short, i)) # high winter uptake
k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, Start_date, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Gap_perc, Tower_QA.QC.NEE.flag, Notes_SiteInfo, Notes_TimeVariant)) # nothing to suggest any particular issues at the site

if(any(!(is.na(k2$GPP_gC_m2))) & nrow(k2)>12) {
  
  k2$Start_date <- as.Date(k2$Start_date)
  
  # calculate all days
  all_days = data.frame(Start_date = seq.Date(from = min(k2$Start_date), to = max(k2$Start_date), by = "month"))
  # join to original data
  dd_complete = left_join(all_days, k2, by = "Start_date")
  
  # ggplot won't connect lines across missing values
  print(ggplot(dd_complete, aes(Start_date, GPP_gC_m2)) +
          geom_point() +
          geom_line() + ggtitle(i)) 
}



# Check NEE time series


for (i in sites) {
  
  #i <- "Parmentier_NO-And_tower1"
  # i <- "Lindroth_SE-Kno_tower1"
  k <- subset(d, startsWith(Study_ID_Short, i)) # high winter uptake
  k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, Start_date, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Gap_perc, Tower_QA.QC.NEE.flag, Notes_SiteInfo, Notes_TimeVariant)) # nothing to suggest any particular issues at the site
  
  if(any(!(is.na(k2$NEE_gC_m2))) & nrow(k2)>12) {
    
    k2$Start_date <- as.Date(k2$Start_date)
    
    # calculate all days
    all_days = data.frame(Start_date = seq.Date(from = min(k2$Start_date), to = max(k2$Start_date), by = "month"))
    # join to original data
    dd_complete = left_join(all_days, k2, by = "Start_date")
    
    # ggplot won't connect lines across missing values
    print(ggplot(dd_complete, aes(Start_date, NEE_gC_m2)) +
            geom_point() +
            geom_line() + ggtitle(i)) 
  }
  
  
  
  
  
  
}



# Conclusion: remove some observations from the sites
d2 <- subset(d, Study_ID_Short!="Lund_Kobbefjord_Ch")


k <- subset(d, startsWith(Study_ID_Short, "Goulden_CA-NS2")) # high winter uptake
k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Notes_SiteInfo, Notes_TimeVariant)) # 2002 Jan wrong
d2 <- subset(d2, !(startsWith(Study_ID_Short, "Goulden_CA-NS2") & d2$Start_date=="2002-01-01"))

k <- subset(d, startsWith(Study_ID_Short, "Goulden_CA-NS3")) # high winter uptake - this does not seem so problematic after all?
k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Notes_SiteInfo, Notes_TimeVariant))
d2 <- subset(d2, !(startsWith(Study_ID_Short, "Goulden_CA-NS3") & d2$Start_date=="2002-01-01"))

k <- subset(d, startsWith(Study_ID_Short, "McCaughey_CA-Man")) # high winter uptake
k2 <- subset(k, select=c(Study_ID, Meas_year, Interval, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Gap_perc, Tower_QA.QC.NEE.flag, Notes_SiteInfo, Notes_TimeVariant)) # nothing to suggest any particular issues at the site
d2 <- subset(d2, !(startsWith(Study_ID_Short, "McCaughey_CA-Man") & d2$Meas_year>=2005))
d2 <- subset(d2, !(startsWith(Study_ID_Short, "McCaughey_CA-Man") & d2$Start_date=="1997-01-01"))
d2 <- subset(d2, !(startsWith(Study_ID_Short, "McCaughey_CA-Man") & d2$Start_date=="1997-02-01"))

ggplot(d2) + geom_boxplot(aes(x=factor(Interval), y=GPP_gC_m2)) + facet_wrap(~Biome)
ggplot(d2) + geom_boxplot(aes(x=factor(Interval), y=Reco_gC_m2)) + facet_wrap(~Biome)

k2 <- subset(d2, select=c(Study_ID, Meas_year, Interval, Start_date, NEE_gC_m2, GPP_gC_m2, Reco_gC_m2, Gap_perc, Tower_QA.QC.NEE.flag, Notes_SiteInfo, Notes_TimeVariant)) # nothing to suggest any particular issues at the site


# still some strange GPP values during winter months? but no reason to exclude them
test <- subset(d2, Interval==3)


setwd("/home/master/")
write.csv(d2, "/home/master/flux_upscaling_data/results/final/modeldata_avg.csv")

gcs_upload_set_limit(50000000L)
gcs_upload("flux_upscaling_data/results/final/modeldata_avg_outliers_still_included.csv", "flux_upscaling_data/results/final/modeldata_avg_outliers_still_included.csv")













### Explore annual NEE in the tundra sites
d <- read.csv("/home/master/flux_upscaling_data/results/final/modeldata_avg.csv")
d2 <- d %>% group_by(Study_ID_Short, Meas_year, Latitude, Longitude, Biome) %>% summarize(NEE=sum(NEE_gC_m2), n=n(), Biome=unique(Biome)) %>% filter(n==12) %>% 
  group_by(Study_ID_Short, Latitude, Longitude ) %>% summarize(NEE=mean(NEE), Biome=unique(Biome)) %>% filter(!is.na(NEE))


d3 <- subset(d2, Biome=="Tundra")

hist(d3$NEE)
summary(d3$NEE)



# not averaged across sites
d <- read.csv("/home/master/flux_upscaling_data/results/final/modeldata_avg.csv")
d2 <- d %>% group_by(Study_ID_Short, Meas_year, Latitude, Longitude, Biome, Country) %>% summarize(NEE=sum(NEE_gC_m2), n=n(), Biome=unique(Biome), Country=unique(Country)) %>% filter(n==12) %>% filter(!is.na(NEE))


d3 <- subset(d2, Biome=="Tundra")

hist(d3$NEE)
summary(d3$NEE)

d3$source <- ifelse(d3$NEE>0, d3$Study_ID_Short, NA)


ggplot(d3) + geom_boxplot(aes(y=NEE, x=Country)) +
  geom_point(aes(y=NEE, x=Country, fill = Study_ID_Short), size = 5, shape = 21, position = position_jitterdodge())




# number of annual sites with gpp or reco estimates
d2 <- d %>% group_by(Study_ID_Short, Meas_year, Latitude, Longitude, Biome, Country) %>% summarize(GPP=sum(GPP_gC_m2), n=n(), Biome=unique(Biome), Country=unique(Country)) %>% filter(n==12) %>% filter(!is.na(GPP)) %>% 
  group_by(Study_ID_Short, Latitude, Longitude, Biome ) %>% summarize(GPP=mean(GPP)) %>% filter(!is.na(GPP))
